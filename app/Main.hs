{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Config
import Translate
import FileIO
import Options.Applicative
import Control.Monad (when)

main :: IO ()
main = execParser opts >>= runner
  where opts = info (helper <*> parseConfig)
                    (fullDesc <>
                     header "Automatically translate Pandoc Markdown files using DeepL.")

runner :: Config -> IO ()
runner config = mapM_ (translateFile config) (files config)

translateFile :: Config -> FilePath -> IO ()
translateFile config path = do
  let path' = addLanguage (targetLang config) path
  when (config.verbosity >= 1) $
    putStrLn ("*** Translating " ++ path ++ " to " ++ path')
  doc <- readMarkdownFile path
  translated <- translatePandoc config doc
  writeMarkdownFile path' translated
  when (config.verbosity >= 1) $
    putStrLn ("*** Wrote " ++ path')
