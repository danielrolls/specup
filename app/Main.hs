module Main where

import Prelude hiding (readFile)
import Data.ByteString.Char8 (readFile)
import SpecUp (invokeTemplateOnSpec)
import Options.Applicative (Parser, execParser, info, helper, fullDesc, long, short, metavar, value, help, strOption, progDesc, (<**>), option, auto)
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.IO as TextIO

data Args = Args {
  specFile :: String
, template :: String
}

argParser :: Parser Args
argParser = Args
  <$> strOption
        (  long "spec"
        <> short 's'
        <> metavar "YAML_FILE"
        <> value defaultSpecFile
        <> help ("File to read the yaml specification from. Defaults to " <> defaultSpecFile)
        )
  <*> strOption
        (  long "mustache"
        <> short 't'
        <> metavar "TEMPLATE_FILE"
        <> value defaultTemplateFile
        <> help ("File to read the mustache template from. Defaults to " <> defaultTemplateFile)
        )
  where defaultSpecFile = "spec.yaml"
        defaultTemplateFile = "template.m"


run :: Args -> IO ()
run args = do
  spec <- readFile $ specFile args
  template <- TextIO.readFile $ template args
  either Prelude.putStrLn
         LazyText.putStrLn
         $ invokeTemplateOnSpec template spec

main :: IO ()
main = execParser (
         info (argParser <**> helper)
         (fullDesc
            <> progDesc "Generate markup from a custom yaml spec"
         )
       ) >>= run
