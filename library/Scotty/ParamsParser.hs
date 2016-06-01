-- |
-- A DSL for parsing of an HTTP request query parameters.
module Scotty.ParamsParser
(
  Params,
  run,
  param,
)
where

import BasePrelude hiding (lookup)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text.Lazy (Text)
import qualified Web.Scotty.Trans as Scotty
import qualified Success.Pure as Success
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy
import qualified Data.Text
import qualified Matcher


-- |
-- A multiple parameters parser.
newtype Params a =
  Params (Matcher.Matcher (HashMap.HashMap Text Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

run :: (Scotty.ScottyError e, Monad m) => Params a -> Scotty.ActionT e m a
run (Params matcher) =
  Scotty.params >>= onParams
  where
    onParams params =
      either (Scotty.raise . Scotty.stringError . Data.Text.unpack) return $
      Matcher.run matcher $
      HashMap.fromList params

lookup :: Text -> Params Text
lookup name =
  Params $
  Matcher.converts $
  \hashMap ->
    maybe (Left ("No parameter named \"" <> Data.Text.Lazy.toStrict name <> "\"")) Right $
    HashMap.lookup name hashMap

-- |
-- Given a parameter name and a parser attempts to get it.
param :: Text -> Matcher.Matcher Text a -> Params a
param name matcher2 =
  case lookup name of
    Params matcher1 ->
      Params (matcher2 . matcher1)

