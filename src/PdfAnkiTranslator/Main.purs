module PdfAnkiTranslator.Main where

import Data.Argonaut.Decode
import Data.Argonaut.Decode.Decoders as Decoders
import Protolude
import Foreign
import Options.Applicative as Options.Applicative
import PdfAnkiTranslator.Config as PdfAnkiTranslator.Config
import PdfAnkiTranslator.ReadStdin as PdfAnkiTranslator.ReadStdin
import PdfAnkiTranslator.Input as PdfAnkiTranslator.Input
import PdfAnkiTranslator.Cache as PdfAnkiTranslator.Cache

-- ./extract_notes.sh | spago run --main PdfAnkiTranslator.Main --node-args '--cache ./mycache.json'

main :: Effect Unit
main = do
  config <- Options.Applicative.execParser PdfAnkiTranslator.Config.opts

  launchAff_ do
    inputJsonString <- PdfAnkiTranslator.ReadStdin.readStdin
      >>= maybe (throwError $ error "Expected stdin") pure

    input <- parseJson inputJsonString
      >>= Decoders.decodeArray PdfAnkiTranslator.Input.decodeInputElement
      # either (throwError <<< error <<< printJsonDecodeError) pure

    traceM input
    -- | PdfAnkiTranslator.Cache.withCache config.cache \cache -> do
    -- |   pure unit
