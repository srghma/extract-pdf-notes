module PdfAnkiTranslator.AffjaxCache where

import Node.Path
import Protolude

import Affjax as Affjax
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as Affjax
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json, stringify, stringifyWithIndent)
import Data.Argonaut.Decode as ArgonautCodecs
import Data.Argonaut.Encode (encodeJson)
import Data.Codec as Data.Codec
import Data.Codec.Argonaut as Data.Codec.Argonaut
import Data.Codec.Argonaut.Common as Data.Codec.Argonaut
import Data.HTTP.Method (unCustomMethod)
import Data.Profunctor (wrapIso)
import Effect.Aff (bracket)
import Effect.Ref as Effect.Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding as Node.Encoding
import Node.FS.Aff as Node.FS.Aff

type Cache =
  { get :: Affjax.Request Json -> Effect (Maybe (Either Affjax.Error (Affjax.Response Json)))
  , set :: Affjax.Request Json -> Either Affjax.Error (Affjax.Response Json) -> Effect Unit
  }

affjaxRequestToKey :: forall a . Affjax.Request a -> String
affjaxRequestToKey request = stringify $ encodeJson $ Object.fromHomogeneous
  { method: either show unCustomMethod request.method
  , url: request.url
  , content:
      maybe
      "Nothing"
      (case _ of
            ArrayView f                   -> "ArrayView unsupported"
            Blob blob                     -> "Blob unsupported"
            Document document             -> "Document unsupported"
            String string                 -> "String unsupported"
            FormData formData             -> "FormData unsupported"
            FormURLEncoded formURLEncoded -> "FormURLEncoded unsupported"
            Json json                     -> stringify json
      )
      request.content
  -- | , headers: show request.headers
  -- | , username: fromMaybe "Nothing" request.username
  -- | , password: fromMaybe "Nothing" request.password
  -- | , withCredentials: show request.withCredentials
  -- | , timeout: maybe "Nothing" show request.timeout
  }


codecResponseHeader :: Data.Codec.Argonaut.JsonCodec ResponseHeader
codecResponseHeader =
  Data.Codec.Argonaut.indexedArray "ResponseHeader" $
  ResponseHeader
    <$> (\(ResponseHeader x _) -> x) Data.Codec.~ Data.Codec.Argonaut.index 0 Data.Codec.Argonaut.string
    <*> (\(ResponseHeader _ x) -> x) Data.Codec.~ Data.Codec.Argonaut.index 1 Data.Codec.Argonaut.string

codecResponse :: Data.Codec.Argonaut.JsonCodec (Affjax.Response Json)
codecResponse =
    Data.Codec.Argonaut.object "Affjax.Response" $ Data.Codec.Argonaut.record
      # Data.Codec.Argonaut.recordProp (SProxy :: _ "status") (wrapIso StatusCode Data.Codec.Argonaut.int)
      # Data.Codec.Argonaut.recordProp (SProxy :: _ "statusText") Data.Codec.Argonaut.string
      # Data.Codec.Argonaut.recordProp (SProxy :: _ "headers") (Data.Codec.Argonaut.array codecResponseHeader)
      # Data.Codec.Argonaut.recordProp (SProxy :: _ "body") Data.Codec.Argonaut.json

codecAffjaxError :: Data.Codec.Argonaut.JsonCodec Affjax.Error
codecAffjaxError =
  Data.Codec.Argonaut.prismaticCodec dec Affjax.printError Data.Codec.Argonaut.string
  where
    dec str = Just $ Affjax.RequestContentError str

codecEitherResponse :: Data.Codec.Argonaut.JsonCodec (Either Affjax.Error (Affjax.Response Json))
codecEitherResponse = Data.Codec.Argonaut.either codecAffjaxError codecResponse

-------------------

-- just like affjax request, but only supports Json
requestWithCache :: Cache -> Affjax.Request Json -> Aff (Either Affjax.Error (Affjax.Response Json))
requestWithCache cache request =
  liftEffect (cache.get request) >>=
    case _ of
         Nothing -> do
            response <- Affjax.request request
            liftEffect $ cache.set request response
            pure response
         Just x -> pure x

-------------------

createCacheWithPersist ::
  String ->
  Aff
  { cache :: Cache
  , persist :: Aff Unit
  }
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
      { get: \affjaxRequest -> do
          obj <- Effect.Ref.read ref
          case Object.lookup (affjaxRequestToKey affjaxRequest) obj of
               Nothing -> pure Nothing
               Just json ->
                 Data.Codec.decode codecEitherResponse json # either (throwError <<< error <<< Data.Codec.Argonaut.printJsonDecodeError) (pure <<< Just)
      , set: \affjaxRequest new -> Effect.Ref.modify_ (Object.insert (affjaxRequestToKey affjaxRequest) (Data.Codec.encode codecEitherResponse new)) ref
      }
    , persist: do
       obj <- liftEffect $ Effect.Ref.read ref
       Node.FS.Aff.writeTextFile Node.Encoding.UTF8 filename (stringifyWithIndent 2 $ encodeJson obj)
    }

withCache :: String -> (Cache -> Aff Unit) -> Aff Unit
withCache filename action = bracket (createCacheWithPersist filename) (\{ persist } -> persist) (\{ cache } -> action cache)
