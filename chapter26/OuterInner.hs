module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

maybeUnwrap :: ExceptT String
               (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO
                (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: ()
             -> IO (Either String
                           (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = MaybeT $ ExceptT $ ReaderT $ const $ return $ Right $ Just 1