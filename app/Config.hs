
module Config where

import Options.Applicative
import Data.Text (Text)

data Config
  = Config  { deeplAPIKey :: Text
            , deeplAPIURL :: String
            , targetLang :: Text
            , sourceLang :: Maybe Text
            , verbosity :: Int
            , files :: [FilePath]
            }


parseConfig :: Parser Config
parseConfig =
  Config
  <$> strOption (long "deeplKey"
                 <> short 'k'
                 <> metavar "STRING"
                 <> help "DeepL API key")
  <*> strOption (long "deeplURL"
                <> short 'u'
                <> metavar "URL"
                <> help "DeepL API URL")
  <*> strOption (long "targetLang"
                <> short 't'
                <> help "target language")
  <*> (optional $ strOption (long "sourceLang"
                <> short 's'
                <> help "source language"))
  <*> option auto (long "verbosity"
                  <> help "verbosity level"
                  <> short 'v'
                  <> metavar "INT"
                  <> value 1                
                  <> showDefault )
  <*> many (argument str (metavar "FILES"))
      
