
# Markdown-translate

A command-line tool for automatically translating Markdown files to differente
languages using Pandoc and the DeepL HTTP API.

Tested with the V2 API and the free API endpoint.

## Usage

~~~
markdown-translate (-k|--deeplKey STRING) (-u|--deeplURL URL)
                   (-t|--targetLang ARG) [-s|--sourceLang ARG] 
                   [-v|--verbosity INT] [FILES]

Available options:
  -h,--help                Show this help text
  -k,--deeplKey STRING     DeepL API key
  -u,--deeplURL URL        DeepL API URL
  -t,--targetLang ARG      target language
  -s,--sourceLang ARG      source language
  -v,--verbosity INT       verbosity level (default: 1)
~~~

## Usage examples

Using the commercial API endpoint; switch to
`https://api-free.deepl.com/v2/translate` for the free one.

~~~bash
$ export URL=https://api.deepl.com/v2/translate 
$ export KEY=<insert your key here>
$ markdown-translate -k $KEY -u $URL -s EN -t PT file1.md file2.md ...
    # Translate from English to Portuguese
    # Writes outputs to file1_PT.md file2_PT.md ...
$ markdown-translate -k $KEY -u $URL -t FR file1.md file2.md ...
    # Translate to French guessing the source language
    # Writes outpyts to file1_FR.md file2_FR.md ...
~~~

## Instalation

You'll need the `ghc` Haskell compiler and `cabal` tool to build this.

~~~bash
$ cabal build
$ cabal install 
~~~

NB: The dependencies will take a while to download and compile (around
30 minutes in my laptop).


------

Pedro Vasconcelos, 2026
