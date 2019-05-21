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
import Control.Lens.TH (makeClassyPrisms)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Iolaus.Opaleye as DB
import Opaleye (Table, Field, table, tableField, selectTable)
import qualified Opaleye as O
import qualified Opaleye.Constant as C
import Opaleye.SqlTypes (SqlText)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

--------------------------------------------------------------------------------
-- | Access the schema files for the database migrations:
import Paths_iolaus_opaleye (getDataDir)

--------------------------------------------------------------------------------
-- | Custom errors for this application.
data AppError = GenericError Text
              | DatabaseError DB.Error
              deriving Show

makeClassyPrisms ''AppError

-- Our custom error type needs to hold database errors and this is how
-- Iolaus can find them.  The @AsError@ class is created by the lens
-- library.
instance DB.AsError AppError where
  _Error = _DatabaseError

--------------------------------------------------------------------------------
-- | A custom transformer stack for your application:
newtype App a = App
  { unApp :: ExceptT AppError (ReaderT DB.Opaleye IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader DB.Opaleye
           , MonadError AppError
           , MonadIO
           )

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
createNewPerson :: App ()
createNewPerson = do
  fn <- Text.pack <$> liftIO (putStr "Enter your first name: "    >> getLine)
  ln <- Text.pack <$> liftIO (putStr "And now your family name: " >> getLine)

  let p = Person (C.constant fn) (C.constant ln)
  _ <- DB.insert $ O.Insert people [p] O.rCount Nothing
  pure ()

--------------------------------------------------------------------------------
-- | Example running a database SELECT from within our app's
-- transformer stack.
printEveryone :: App ()
printEveryone = do
    ps <- DB.select $ selectTable people
    mapM_ printPerson ps

  where
    printPerson :: Person' Text -> App ()
    printPerson = liftIO . print

--------------------------------------------------------------------------------
-- | Unwind the transformer stack and get back to IO.
runApp :: DB.Opaleye -> App a -> IO (Either AppError a)
runApp opaleye = flip runReaderT opaleye . runExceptT . unApp

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Create a configuration and initialize Iolaus.  Normally we would
  -- read the configuration from a file.  Here we'll just grab the
  -- database connection string from the environment and use the
  -- default confutation settings.
  connString <- Text.pack <$> getEnv "DB_CONN"
  opaleye <- DB.initOpaleye $ DB.defaultConfig connString

  -- Makes the name prompting in createNewPerson nicer.
  hSetBuffering stdout NoBuffering

  -- Run an action in our transformer stack.  It will migrate the
  -- database then do some database work.
  result <- runApp opaleye $ do
    schemaDir <- (</> "example" </> "schema") <$> liftIO getDataDir
    DB.migrate schemaDir True
    createNewPerson
    printEveryone

  print result
