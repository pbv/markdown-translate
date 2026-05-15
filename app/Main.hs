{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Config
import Translate
import FileIO
import Options.Applicative
import Control.Monad (when)
import System.Directory (doesFileExist)

main :: IO ()
main = execParser opts >>= runner
  where opts = info (helper <*> parseConfig)
                    (fullDesc <>
                     header "Automatically translate Pandoc Markdown files using DeepL.")

runner :: Config -> IO ()
runner config = mapM_ (translateFile config) config.files

translateFile :: Config -> FilePath -> IO ()
translateFile config path = do
  let path' = addLanguage (targetLang config) path
  check <- doesFileExist path'
  if check then 
    when (config.verbosity >= 1) $
      putStrLn ("*** Destination file "
                 <> path' <>
                 " already exists; skipping.")
    else do
    when (config.verbosity >= 1) $
      putStrLn ("*** Translating file " <> path <> " to " <> path')
    doc <- readMarkdownFile path
    translated <- translatePandoc config doc
    writeMarkdownFile path' translated
    when (config.verbosity >= 1) $
      putStrLn ("*** Wrote file " <> path')
