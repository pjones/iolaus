{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

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
import Control.Monad.Database
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as Text
import Iolaus.Database.Config
import Iolaus.Database.Error
import Iolaus.Database.Query
import Iolaus.Database.Table
import List.Transformer (ListT(..), Step(..))
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import qualified System.Metrics as Metrics

--------------------------------------------------------------------------------
-- Access the schema files for the database migrations:
import Paths_iolaus_opaleye (getDataDir)

--------------------------------------------------------------------------------
-- | The table's primary key.
type PersonId = Key Int64 Person

--------------------------------------------------------------------------------
-- | Database table type, using the type family style from the
-- "Iolaus.Database.Table" module.
data Person f = Person
  { pkey      :: Col f "id"         PersonId SqlInt8 ReadOnly
  , firstName :: Col f "first_name" Text     SqlText Required
  , lastName  :: Col f "last_name"  Text     SqlText Required
  }

deriving instance Show (Person ForHask)
deriving instance Eq (Person ForHask)

makeTable ''Person "people"

--------------------------------------------------------------------------------
-- | Insert a new person into the database.
createNewPerson
  :: ( MonadDatabase m
     , MonadError  e m
     , AsDbError   e
     )
  => Text -> Text -> m ()
createNewPerson fn ln = do
  let p = Person Nothing (toFields fn) (toFields ln)
  _ <- runQuery (insert (Insert people [p] rCount Nothing))
  pure ()

--------------------------------------------------------------------------------
-- | Example running a database SELECT.
fetchEveryone
  :: ( MonadDatabase m
     , MonadError  e m
     , AsDbError   e
     )
  => m [Person ForHask]
fetchEveryone = runQuery (select (selectTable people))

--------------------------------------------------------------------------------
printEveryoneStreaming
  :: ( MonadIO       m
     , MonadDatabase m
     , MonadError  e m
     , AsDbError   e
     )
  => m ()
printEveryoneStreaming = do
    list <- runQuery (selectToStream (selectTable people))
    liftIO (loop list)
  where
    loop :: ListT IO (Person ForHask) -> IO ()
    loop list = do
      row <- next list

      case row of
        Nil -> return ()
        Cons person list' -> do
          print person
          loop list'

--------------------------------------------------------------------------------
-- | Example of counting matching rows.
numberOfPeople
  :: ( MonadDatabase m
     , MonadError  e m
     , AsDbError   e
     )
  => m Int64
numberOfPeople = runQuery (count (selectTable people))

--------------------------------------------------------------------------------
app
  :: ( MonadIO       m
     , MonadDatabase m
     , MonadError  e m
     , AsDbError   e
     )
  => m ()
app = do
  schemaDir <- (</> "example" </> "schema") <$> liftIO getDataDir
  migrate schemaDir MigrateVerbosely >>= liftIO . print

  fn <- Text.pack <$> liftIO (putStr "Enter your given/first name: "   >> getLine)
  ln <- Text.pack <$> liftIO (putStr "And now your family/last name: " >> getLine)

  createNewPerson fn ln

  liftIO (putStrLn "Fetching all rows as once: ")
  fetchEveryone >>= mapM_ (liftIO . print)

  liftIO (putStrLn "Streaming the entire table row by row: ")
  printEveryoneStreaming

  n <- numberOfPeople
  liftIO (putStrLn ("Number of records in the table: " <> show n))

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
  config <- defaultDbConfig . Text.pack <$> getEnv "DB_CONN"
  runtime <- initRuntime config (Just store)

  -- Makes the name prompting in 'app' nicer.
  hSetBuffering stdout NoBuffering

  -- Run the app by discharging all of the effects.
  result <- runExceptT (runDatabaseT runtime app)

  -- Print out the result (which should be @Right ()@) and the EKG store:
  print (result :: Either DbError ())
  print =<< Metrics.sampleAll store
