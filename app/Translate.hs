{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Translate where

import           Config
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Control.Exception (throwIO)
import           Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as P
import           Text.Pandoc.Walk (walkM)
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Data.List (tails)

-- | DeepL requests
data DeepLRequest
  = DeepLRequest { source :: Text
                 , context :: Text
                 }
  deriving Show

-- | DeepL Response types
newtype DeepLResponse = DeepLResponse [Translation]
  deriving Show

newtype Translation = Translation { translationText :: Text }
  deriving Show

instance FromJSON DeepLResponse where
  parseJSON (Object v)  =  DeepLResponse <$> v .: "translations"
  parseJSON other = typeMismatch "Object" other

instance FromJSON Translation where
  parseJSON (Object v)  = Translation <$> v .: "text" 
  parseJSON other = typeMismatch "Object" other

-- | Main translation function
translatePandoc :: Config -> Pandoc -> IO Pandoc
translatePandoc config (Pandoc meta blocks) = do
  translatedBlocks <- walkM (translateInlines config) blocks
  return $ Pandoc meta translatedBlocks 


-- | ++++++++++ INLINE SPLIT & JOIN ++++++++++

data InlineChunk
  = Translatable Text
  | Untranslatable Inline
  deriving Show

-- | Translates text inlines while preserving code, maths, etc.
translateInlines :: Config -> [Inline] -> IO [Inline]
translateInlines config inlines = do
  translated <- mapM translateChunks (joinChunks config.window $
                                       splitInlines inlines)
  return $ P.toList $ mconcat translated
  where
    translateChunks :: [InlineChunk] -> IO P.Inlines
    translateChunks chunks
      = case last chunks of
          (Untranslatable inline) -> return (P.singleton inline)
          (Translatable txt) -> do
            let ctx = mconcat (map chunkText $ init chunks)
            let req = DeepLRequest { source = txt, context = ctx }
            when (config.verbosity>=3) $
              print req
            translated <- sendTranslationRequest config req
            return (P.text translated)

joinChunks :: Int -> [InlineChunk] -> [[InlineChunk]]
joinChunks n chunks =
  slidingWindow n (replicate (n-1) (Untranslatable Space) ++ chunks)

-- | Splits inlines into chunks that should or should not be translated
splitInlines :: [Inline] -> [InlineChunk]
splitInlines [] = []
splitInlines (Str s:rest)
  = go rest s
  where
    go :: [Inline] -> Text -> [InlineChunk]
    go [] acc = [Translatable acc]
    go (x:xs) acc
      | Just s' <- toText x = go xs (acc <> s')
      | otherwise = Translatable acc : Untranslatable x : splitInlines xs
splitInlines (x:xs) = Untranslatable x : splitInlines xs

-- text inlines can should be collected and translated
toText :: Inline -> Maybe Text
toText (Str s)   = Just s
toText Space     = Just " "
toText SoftBreak = Just " "
toText LineBreak = Just "\n"
toText _         = Nothing

chunkText :: InlineChunk -> Text
chunkText (Translatable txt) = txt
chunkText (Untranslatable _) = ""

-- | break a list into sliding windows of a given size
slidingWindow :: Int -> [a] -> [[a]]
slidingWindow k xs
  | k <= n = take (n-k+1) $ map (take k) (tails xs)
  | otherwise = [xs]
  where n = length xs


-- | Send DeepL API request
-- implements an exponential back-off when we get
-- a TooManyRequests reply from the server
sendTranslationRequest :: Config -> DeepLRequest -> IO Text
sendTranslationRequest conf req = send 10 
  where
    request = prepareRequest conf req
    send :: Int -> IO Text
    send delay = httpLBS request >>= continue delay
    continue :: Int -> Response L.ByteString -> IO Text
    continue delay response 
      | status == ok200
      , Just (DeepLResponse (t:_)) <- decode (getResponseBody response)
      = return (translationText t)
      | status == tooManyRequests429 = do
          when (conf.verbosity >= 2) $ do
            putStrLn "*** DeepL returned TooManyRequests"
            putStrLn ("*** Retrying after " ++ show delay ++ "s")
          threadDelay (delay * 1000000)
          send (2*delay)
      | otherwise = throwIO $ userError (show status)
      where status = getResponseStatus response 

prepareRequest :: Config -> DeepLRequest -> Request
prepareRequest conf req =
  let requestBody
        = objectNoNulls [ "target_lang" .= conf.targetLang
                        , "source_lang" .= conf.sourceLang
                        , "text" .= [req.source]
                        , "context" .= req.context
                        , "preserve_formatting" .= True
                        ]
    in setRequestMethod "POST"
         $ setRequestSecure True
         $ setRequestHeader "Authorization"
                     [E.encodeUtf8 ("DeepL-Auth-Key " <> conf.deeplAPIKey)]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON requestBody
         $ parseRequest_ conf.deeplAPIURL


objectNoNulls :: [Pair] -> Value
objectNoNulls = object . filter (\(_,v) -> v/=Null)
