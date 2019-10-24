{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

Copyright:
  This file is part of the package iolaus.  It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/open/iolaus.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: BSD-2-Clause

-}
module Main (main) where

--------------------------------------------------------------------------------
-- Imports:
import Control.Lens.TH (makeClassy, makeClassyPrisms)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Iolaus.Database as DB
import Opaleye (Table, Field, table, tableField, selectTable)
import qualified Opaleye as O
import qualified Opaleye.Constant as C
import Opaleye.SqlTypes (SqlText)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import qualified System.Metrics as Metrics

--------------------------------------------------------------------------------
-- | Access the schema files for the database migrations:
import Paths_iolaus_opaleye (getDataDir)

--------------------------------------------------------------------------------
-- | Custom errors for this application.
data AppError = GenericError Text
              | DatabaseError DB.DBError
              deriving Show

makeClassyPrisms ''AppError

-- Our custom error type needs to hold database errors and this is how
-- Iolaus can find them.  The @AsError@ class is created by the lens
-- library.
instance DB.AsDBError AppError where
  _DBError = _DatabaseError


--------------------------------------------------------------------------------
-- | A custom reader environment for this application.
data AppEnv = AppEnv
  { _db        :: DB.Database -- ^ The Database run time.
  , _something :: Text        -- ^ Example value.
  }

makeClassy ''AppEnv

-- Iolaus needs to know how to get the 'Opaleye' environment out of
-- our application's reader environment.  This instance will tell it
-- how to do that.
instance DB.HasDatabase AppEnv where
  database = db

--------------------------------------------------------------------------------
-- | A custom transformer stack for your application:
--
-- Note: You don't need to make your custom monad an instance of
-- @MonadIO@.  If fact, in most cases you probably don't want to do
-- that.
--
-- If you don't make your monad an instance of @MonadIO@ then the
-- instance for @MonadOpaleye@ needs to lift the @runQuery@ function
-- into the inner monad (which is @MonadIO@) to work.
--
-- We'll use @MonadIO@ here to make this code simpler.
newtype App a = App
  { unApp :: ExceptT AppError (ReaderT AppEnv IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader AppEnv
           , MonadError AppError
           , MonadIO
           )

-- Define how queries are lifted into your application:
instance DB.MonadDB App where
  liftQuery = DB.liftQueryIO

--------------------------------------------------------------------------------
-- | Database table type.  Standard Opaleye stuff.
data Person' name = Person
  { firstName :: name
  , lastName  :: name
  } deriving (Generic, Show)

$(makeAdaptorAndInstance "pPerson" ''Person')

people :: Table (Person' (Field SqlText)) (Person' (Field SqlText))
people = table "people" (pPerson
  Person { firstName = tableField "first_name"
         , lastName  = tableField "last_name"
         })

--------------------------------------------------------------------------------
-- | Insert a new person into the database.
createNewPerson :: (DB.MonadDB m) => Text -> Text -> m ()
createNewPerson fn ln = do

  let p = Person (C.constant fn) (C.constant ln)
  _ <- DB.liftQuery (DB.insert $ O.Insert people [p] O.rCount Nothing)
  pure ()

--------------------------------------------------------------------------------
-- | Example running a database SELECT from within our app's
-- transformer stack.
fetchEveryone :: (DB.MonadDB m) => m [Person' Text]
fetchEveryone = DB.liftQuery (DB.select $ selectTable people)

--------------------------------------------------------------------------------
-- | Unwind the transformer stack and get back to IO.
runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp env = flip runReaderT env . runExceptT . unApp

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Giving an EKG store to Iolaus is optional, but we'll do it so we
  -- can see how many queries were executed.
  store <- Metrics.newStore

  -- Create a configuration and initialize Iolaus.  Normally we would
  -- read the configuration from a file.  Here we'll just grab the
  -- database connection string from the environment and use the
  -- default configuration settings.
  --
  -- NOTE: If you are running this example directly from source you
  -- need to tell Cabal where the source directory is so it can find
  -- the schema files.  Set the @iolaus_opaleye_datadir@ environment
  -- variable to the directory containing the @example@ directory.
  config <- DB.defaultConfig . Text.pack <$> getEnv "DB_CONN"
  opaleye <- DB.initDatabase config (Just store)

  -- Makes the name prompting in createNewPerson nicer.
  hSetBuffering stdout NoBuffering

  -- Run an action in our transformer stack.  It will migrate the
  -- database then do some database work.
  result <- runApp (AppEnv opaleye "Something") $ do
    schemaDir <- (</> "example" </> "schema") <$> liftIO getDataDir
    DB.migrate schemaDir True

    fn <- Text.pack <$> liftIO (putStr "Enter your given/first name: "   >> getLine)
    ln <- Text.pack <$> liftIO (putStr "And now your family/last name: " >> getLine)

    createNewPerson fn ln
    fetchEveryone >>= mapM_ (liftIO . print)

  -- Print out the result (which should be @Right ()@) and the EKG store:
  print result
  print =<< Metrics.sampleAll store
