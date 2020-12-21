module PdfAnkiTranslator.Main where

import Data.Argonaut.Decode
import Protolude
import Foreign
import Data.Argonaut.Decode.Decoders                    as Decoders
import Options.Applicative                              as Options.Applicative
import PdfAnkiTranslator.Config                         as PdfAnkiTranslator.Config
import PdfAnkiTranslator.ReadStdin                      as PdfAnkiTranslator.ReadStdin
import PdfAnkiTranslator.Input                          as PdfAnkiTranslator.Input
import PdfAnkiTranslator.Cache                          as PdfAnkiTranslator.Cache
import PdfAnkiTranslator.Lingolive.Actions.Authenticate as PdfAnkiTranslator.Lingolive.Actions.Authenticate
import Affjax                           as Affjax
import Affjax.ResponseFormat            as Affjax.ResponseFormat
import PdfAnkiTranslator.Lingolive.Actions.Translation as PdfAnkiTranslator.Lingolive.Actions.Translation
import PdfAnkiTranslator.Languages
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import PdfAnkiTranslator.Lingolive.Types

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

    abbyyAccessKey <- PdfAnkiTranslator.Lingolive.Actions.Authenticate.authenticate { apiKey: config.abbyy_api_key }
      >>= either (throwError <<< error <<< PdfAnkiTranslator.Lingolive.Actions.Authenticate.printError) pure

    traceM abbyyAccessKey

    PdfAnkiTranslator.Cache.withCache config.cache \cache -> do
      let text = "ankommen"
      (result :: NonEmptyArray ArticleModel) <- PdfAnkiTranslator.Lingolive.Actions.Translation.translation { accessKey: abbyyAccessKey } { text, srcLang: German, dstLang: Russian }
        >>= either (throwError <<< error <<< PdfAnkiTranslator.Lingolive.Actions.Translation.printError text) pure
      traceM result
