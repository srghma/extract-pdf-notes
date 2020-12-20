module PdfAnkiTranslator.Config where
import Protolude
import Data.Maybe as Maybe

import Options.Applicative

type Config =
  { cache  :: String
  }

configParser :: Parser Config
configParser = ado
  cache  <- option str $ long "cache" <> metavar "ANYFILE"

  in
    { cache
    }

opts :: ParserInfo Config
opts =
  info (configParser <**> helper) $ fullDesc <> progDesc "Adds translation to words" <> header "pdf-anki-translator - adds translation to words"
