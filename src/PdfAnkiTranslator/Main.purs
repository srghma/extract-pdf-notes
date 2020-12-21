module PdfAnkiTranslator.Main where

import Data.Argonaut.Decode
import Foreign
import PdfAnkiTranslator.Languages
import PdfAnkiTranslator.Lingolive.Types
import Protolude

import Affjax as Affjax
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Options.Applicative as Options.Applicative
import PdfAnkiTranslator.AffjaxCache as PdfAnkiTranslator.AffjaxCache
import PdfAnkiTranslator.Config as PdfAnkiTranslator.Config
import PdfAnkiTranslator.GoogleTranslate.Translate as PdfAnkiTranslator.GoogleTranslate.Translate
import PdfAnkiTranslator.Input (InputElement)
import PdfAnkiTranslator.Input as PdfAnkiTranslator.Input
import PdfAnkiTranslator.Lingolive.Actions.Authenticate as PdfAnkiTranslator.Lingolive.Actions.Authenticate
import PdfAnkiTranslator.Lingolive.Actions.Translation as PdfAnkiTranslator.Lingolive.Actions.Translation
import PdfAnkiTranslator.Print as PdfAnkiTranslator.Print
import PdfAnkiTranslator.ReadStdin as PdfAnkiTranslator.ReadStdin

-- ./extract_notes.sh | spago run --main PdfAnkiTranslator.Main --node-args '--cache ./mycache.json'

main :: Effect Unit
main = do
  config <- Options.Applicative.execParser PdfAnkiTranslator.Config.opts

  launchAff_ do
    inputJsonString <- PdfAnkiTranslator.ReadStdin.readStdin
      >>= maybe (throwError $ error "Expected stdin") pure

    (input :: NonEmptyArray InputElement) <- parseJson inputJsonString
      >>= Decoders.decodeNonEmptyArray PdfAnkiTranslator.Input.decodeInputElement
      # either (throwError <<< error <<< printJsonDecodeError) pure

    -- | traceM input

    abbyyAccessKey <- PdfAnkiTranslator.Lingolive.Actions.Authenticate.authenticate { apiKey: config.abbyy_api_key }
      >>= either (throwError <<< error <<< PdfAnkiTranslator.Lingolive.Actions.Authenticate.printError) pure

    -- | traceM abbyyAccessKey

    PdfAnkiTranslator.AffjaxCache.withCache config.cache \cache -> do
      let text = "ankommen"

      (abbyyResult :: NonEmptyArray ArticleModel) <- PdfAnkiTranslator.Lingolive.Actions.Translation.translation
        { accessKey: abbyyAccessKey
        , requestFn: PdfAnkiTranslator.AffjaxCache.requestWithCache cache
        }
        { text
        , srcLang: German
        , dstLang: Russian
        }
        >>= either (throwError <<< error <<< PdfAnkiTranslator.Lingolive.Actions.Translation.printError text) pure

      -- | traceM abbyyResult

      (googleResult :: NonEmptyArray String) <- PdfAnkiTranslator.GoogleTranslate.Translate.request
        { accessKey: config.google_translate_access_key
        , requestFn: PdfAnkiTranslator.AffjaxCache.requestWithCache cache
        }
        { q: text
        , source: German
        , target: Russian
        }
        >>= either (throwError <<< error <<< PdfAnkiTranslator.GoogleTranslate.Translate.printError text) pure

      let renderedWord = PdfAnkiTranslator.Print.printArticleModel
            { fromAbbyy:           abbyyResult
            , fromGoogleTranslate: googleResult
            , sentence:            (NonEmptyArray.head input).sentence
            , annotation_text:     (NonEmptyArray.head input).annotation_text
            , annotation_content:  (NonEmptyArray.head input).annotation_content
            }
