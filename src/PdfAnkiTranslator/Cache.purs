module PdfAnkiTranslator.Cache where

import Node.Path
import Protolude

import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Argonaut.Decode as ArgonautCodecs
import Data.Argonaut.Encode (encodeJson)
import Effect.Aff (bracket)
import Effect.Ref as Effect.Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding as Node.Encoding
import Node.FS.Aff as Node.FS.Aff

newtype Key = Key String

type Cache =
  { get :: Key -> Effect (Maybe Json)
  , set :: Key -> Json -> Effect Unit
  }

type CacheWithPersist =
  { cache :: Cache
  , persist :: Aff Unit
  }

createCacheWithPersist :: String -> Aff CacheWithPersist
createCacheWithPersist filename = do
  (json :: Object Json) <-
    Node.FS.Aff.exists filename >>=
      if _
        then do
          text <- Node.FS.Aff.readTextFile Node.Encoding.UTF8 filename
          (ArgonautCodecs.parseJson text >>= ArgonautCodecs.decodeJson) # either (throwError <<< error <<< ArgonautCodecs.printJsonDecodeError) pure
        else pure $ Object.empty

  ref <- liftEffect $ Effect.Ref.new json

  pure
    { cache:
      { get: \(Key key) -> do
          obj <- Effect.Ref.read ref
          pure $ Object.lookup key obj
      , set: \(Key key) newJson -> Effect.Ref.modify_ (Object.insert key newJson) ref
      }
    , persist: do
       obj <- liftEffect $ Effect.Ref.read ref
       Node.FS.Aff.writeTextFile Node.Encoding.UTF8 filename (stringifyWithIndent 2 $ encodeJson obj)
    }

withCache :: String -> (Cache -> Aff Unit) -> Aff Unit
withCache filename action = bracket (createCacheWithPersist filename) (\{ persist } -> persist) (\{ cache } -> action cache)
