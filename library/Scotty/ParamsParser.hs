-- |
-- A DSL for parsing of an HTTP request query parameters.
module Scotty.ParamsParser
(
  Params,
  run,
  lookup,
  param,
  Param,
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


-- |
-- A multiple parameters parser.
newtype Params a =
  Params (ReaderT (HashMap.HashMap Text Text) (Success.Success Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

run :: (Scotty.ScottyError e, Monad m) => Params a -> Scotty.ActionT e m a
run (Params (ReaderT success')) =
  Scotty.params >>= onParams
  where
    onParams params =
      either (Scotty.raise . Scotty.stringError . Data.Text.Lazy.unpack . fromMaybe "") return $
      Success.asEither $
      success' $
      HashMap.fromList params

lookup :: Text -> Params Text
lookup name =
  Params $ ReaderT $ \hashMap ->
    maybe (Success.failure $ "No parameter named" <> name) Success.success $
    HashMap.lookup name hashMap

-- |
-- Given a parameter parser and name attempts to get it.
param :: Param a -> Text -> Params a
param paramParser name =
  lookup name >>= onText
  where
    onText text =
      Params $ ReaderT $ const $ either Success.failure Success.success $ paramParser text

-- |
-- A single parameter parser.
type Param a =
  Text -> Either Text a

