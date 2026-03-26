{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Translate where

import           Config
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Control.Concurrent (threadDelay)
import           Control.Monad (mzero, when)
import           Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as P
import           Text.Pandoc.Walk (walkM)
import           Network.HTTP.Simple

-- | Response types
newtype TranslationResponse = TranslationResponse [Translation]
  deriving Show

newtype Translation = Translation { translationText :: Text }
  deriving Show

instance FromJSON TranslationResponse where
  parseJSON (Object v)  = TranslationResponse <$> v .: "translations"
  parseJSON _ = mzero

instance FromJSON Translation where
  parseJSON (Object v)  = Translation <$> v .: "text" 
  parseJSON _ = mzero

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

-- | Translates blocks of text while preserving others
translateInlines :: Config -> [Inline] -> IO [Inline]
translateInlines config inlines = do
  translatedChunks <- mapM translateChunk (splitInlines inlines)
  return $ P.toList $ mconcat translatedChunks
  where
    translateChunk :: InlineChunk -> IO P.Inlines
    translateChunk (Untranslatable inline) = return (P.singleton inline)
    translateChunk (Translatable txt) = do     
      translated <- sendTranslationRequest config txt
      return (P.text translated)
      {-
      -- work around DeepL bug that sometimes removes trailing spaces
      let padr | not (T.null txt) && isSpace (T.last txt) = P.space
               | otherwise = mempty
      let padl | not (T.null txt) && isSpace (T.head txt) = P.space
               | otherwise = mempty
      return (padl <> P.text translated <> padr)
      -}

-- | Splits inlines into chunks that should or should not be translated
splitInlines :: [Inline] -> [InlineChunk]
splitInlines [] = []
splitInlines (Str s:rest)
  = go rest s
  where
    go [] acc = [Translatable acc]
    go (x:xs) acc
      | Just s' <- toText x = go xs (acc<>s')
      | otherwise = Translatable acc : Untranslatable x : splitInlines xs
splitInlines (x:xs) = Untranslatable x : splitInlines xs

-- text inlines can should be collected and translated
toText :: Inline -> Maybe Text
toText (Str s)   = Just s
toText Space     = Just " "
toText SoftBreak = Just " "
toText LineBreak = Just "\n"
toText _         = Nothing



-- | Send DeepL API request
-- implements an exponential backoff in case of errors
sendTranslationRequest :: Config -> Text -> IO Text
sendTranslationRequest conf text = go 10 
  where
    request = prepareRequest conf text
    go :: Int -> IO Text
    go delay = do
          response <- httpLBS request
          case decode (getResponseBody response) of
            Just (TranslationResponse (t:_)) -> return (translationText t)
            _what -> do
              when (conf.verbosity >= 2) $ do
                putStrLn ("*** DeepL returned " ++ show _what)
                putStrLn ("*** Retrying after " ++ show delay ++ "s")
              threadDelay (delay * 1000000)
              go (2*delay)
              

prepareRequest :: Config -> Text -> Request
prepareRequest conf text =
  let requestBody
        = object (maybe [] (\lang -> ["source_lang" .=  lang]) conf.sourceLang
                  ++
                  [ "target_lang" .= conf.targetLang
                  , "text" .= [text]
                  , "preserve_formatting" .= True
                  ])
    in setRequestMethod "POST"
         $ setRequestSecure True
         $ setRequestHeader "Authorization"
                     [E.encodeUtf8 ("DeepL-Auth-Key " <> conf.deeplAPIKey)]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON requestBody
         $ parseRequest_ conf.deeplAPIURL
