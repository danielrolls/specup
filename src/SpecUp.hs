module SpecUp(invokeTemplateOnSpec) where

import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import Data.Yaml (decodeEither', ParseException, prettyPrintParseException)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Mustache (compileMustacheText, renderMustache)
import qualified Data.Text.Lazy as LazyText
import Data.Either.Extra (mapLeft)

invokeTemplateOnSpec :: Text -> ByteString -> Either String LazyText.Text
invokeTemplateOnSpec templ spec =
  let compiledTemplate = mapLeft (("Texplate compilation failed with the following error: " <>) . errorBundlePretty)
                                 $ compileMustacheText "base template" templ
      spec' = mapLeft (("yaml decoding failed with the following error: " <>) . prettyPrintParseException)
                      $ decodeEither' spec
  in renderMustache <$> compiledTemplate <*> spec'

