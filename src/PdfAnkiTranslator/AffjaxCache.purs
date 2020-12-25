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
import Data.Profunctor (dimap, wrapIso)
import Effect.Aff (bracket)
import Effect.Ref as Effect.Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding as Node.Encoding
import Node.FS.Aff as Node.FS.Aff

-- because why we nned them?
type AffjaxResponseWithoutHeaders a =
  { status :: StatusCode
  , statusText :: String
  , body :: a
  }

toAffjaxResponseWithoutHeaders :: forall a . Affjax.Response a -> AffjaxResponseWithoutHeaders a
toAffjaxResponseWithoutHeaders x =
  { status: x.status
  , statusText: x.statusText
  , body: x.body
  }

fromAffjaxResponseWithoutHeaders :: forall a . AffjaxResponseWithoutHeaders a -> Affjax.Response a
fromAffjaxResponseWithoutHeaders x =
  { status: x.status
  , statusText: x.statusText
  , body: x.body
  , headers: []
  }

type Cache =
  { get ::
      forall a .
      { affjaxRequest :: Affjax.Request a
      , bodyCodec :: Data.Codec.Argonaut.JsonCodec a
      } ->
      Effect (Maybe (Either Affjax.Error (Affjax.Response a)))
  , set ::
      forall a .
      { affjaxRequest :: Affjax.Request a
      , bodyCodec :: Data.Codec.Argonaut.JsonCodec a
      , response :: Either Affjax.Error (AffjaxResponseWithoutHeaders a)
      } ->
      Effect Unit
  }

affjaxRequestToKey :: forall a . Affjax.Request a -> String
affjaxRequestToKey affjaxRequest = stringify $ encodeJson $ Object.fromHomogeneous
  { method: either show unCustomMethod affjaxRequest.method
  , url: affjaxRequest.url
  , content:
      maybe
      "Nothing"
      (case _ of
            ArrayView f                   -> "ArrayView unsupported"
            Blob blob                     -> "Blob unsupported"
            Document document             -> "Document unsupported"
            String string                 -> string
            FormData formData             -> "FormData unsupported"
            FormURLEncoded formURLEncoded -> "FormURLEncoded unsupported"
            Json json                     -> stringify json
      )
      affjaxRequest.content
  -- | , headers: show affjaxRequest.headers
  -- | , username: fromMaybe "Nothing" affjaxRequest.username
  -- | , password: fromMaybe "Nothing" affjaxRequest.password
  -- | , withCredentials: show affjaxRequest.withCredentials
  -- | , timeout: maybe "Nothing" show affjaxRequest.timeout
  }


codecResponseHeader :: Data.Codec.Argonaut.JsonCodec ResponseHeader
codecResponseHeader =
  Data.Codec.Argonaut.indexedArray "ResponseHeader" $
  ResponseHeader
    <$> (\(ResponseHeader x _) -> x) Data.Codec.~ Data.Codec.Argonaut.index 0 Data.Codec.Argonaut.string
    <*> (\(ResponseHeader _ x) -> x) Data.Codec.~ Data.Codec.Argonaut.index 1 Data.Codec.Argonaut.string

codecResponse :: forall a . Data.Codec.Argonaut.JsonCodec a -> Data.Codec.Argonaut.JsonCodec (AffjaxResponseWithoutHeaders a)
codecResponse bodyCodec =
    Data.Codec.Argonaut.object "AffjaxResponseWithoutHeaders" $ Data.Codec.Argonaut.record
      # Data.Codec.Argonaut.recordProp (SProxy :: _ "status") (wrapIso StatusCode Data.Codec.Argonaut.int)
      # Data.Codec.Argonaut.recordProp (SProxy :: _ "statusText") Data.Codec.Argonaut.string
      # Data.Codec.Argonaut.recordProp (SProxy :: _ "body") bodyCodec

-- TODO
codecAffjaxError :: Data.Codec.Argonaut.JsonCodec Affjax.Error
codecAffjaxError =
  Data.Codec.Argonaut.prismaticCodec dec Affjax.printError Data.Codec.Argonaut.string
  where
    dec str = Just $ Affjax.RequestContentError str

codecEitherResponse :: forall a . Data.Codec.Argonaut.JsonCodec a -> Data.Codec.Argonaut.JsonCodec (Either Affjax.Error (AffjaxResponseWithoutHeaders a))
codecEitherResponse bodyCodec = Data.Codec.Argonaut.either codecAffjaxError (codecResponse bodyCodec)

-------------------

-- just like affjax request, but only supports Json
requestWithCache :: Cache -> Affjax.Request Json -> Aff (Either Affjax.Error (Affjax.Response Json))
requestWithCache cache affjaxRequest =
  liftEffect (cache.get { affjaxRequest, bodyCodec: Data.Codec.Argonaut.json }) >>=
    case _ of
         Nothing -> do
            response <- Affjax.request affjaxRequest
            let (response' :: Either Affjax.Error (AffjaxResponseWithoutHeaders Json)) = map toAffjaxResponseWithoutHeaders response
            liftEffect $ cache.set { affjaxRequest, bodyCodec: Data.Codec.Argonaut.json, response: response' }
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
      { get: undefined
        -- \{ affjaxRequest, bodyCodec } -> do
          -- | obj <- Effect.Ref.read ref
          -- | case Object.lookup (affjaxRequestToKey affjaxRequest) obj of
          -- |      Nothing -> pure Nothing
          -- |      Just json ->
          -- |        Data.Codec.decode (codecEitherResponse bodyCodec) json # either (throwError <<< error <<< Data.Codec.Argonaut.printJsonDecodeError) (pure <<< Just)
      , set: \{ affjaxRequest, bodyCodec, response } ->
          Effect.Ref.modify_ (Object.insert (affjaxRequestToKey affjaxRequest) (Data.Codec.encode (codecEitherResponse bodyCodec) response)) ref
      }
    , persist: do
       obj <- liftEffect $ Effect.Ref.read ref
       Node.FS.Aff.writeTextFile Node.Encoding.UTF8 filename (stringifyWithIndent 2 $ encodeJson obj)
    }

withCache :: String -> (Cache -> Aff Unit) -> Aff Unit
withCache filename action = bracket (createCacheWithPersist filename) (\{ persist } -> persist) (\{ cache } -> action cache)
