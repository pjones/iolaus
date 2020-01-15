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
import Control.Carrier.Lift
import Control.Carrier.Database
import Control.Carrier.Throw.Either
import Data.Function ((&))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import qualified Data.Text as Text
import Opaleye (Table, Field, table, tableField, selectTable)
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
-- | Database table type.  Standard Opaleye stuff.
data Person' name = Person
  { firstName :: name
  , lastName  :: name
  } deriving Show

$(makeAdaptorAndInstance "pPerson" ''Person')

people :: Table (Person' (Field SqlText)) (Person' (Field SqlText))
people = table "people" (pPerson
  Person { firstName = tableField "first_name"
         , lastName  = tableField "last_name"
         })

--------------------------------------------------------------------------------
-- | Insert a new person into the database.
createNewPerson :: (Has Database sig m, Has (Throw DbError) sig m)
                => Text -> Text -> m ()
createNewPerson fn ln = do
  let p = Person (C.constant fn) (C.constant ln)
  _ <- runQuery (insert (Insert people [p] rCount Nothing))
  pure ()

--------------------------------------------------------------------------------
-- | Example running a database SELECT from within our app's
-- transformer stack.
fetchEveryone :: (Has Database sig m, Has (Throw DbError) sig m)
              => m [Person' Text]
fetchEveryone = runQuery (select (selectTable people))

--------------------------------------------------------------------------------
app :: ( Has Database sig m
       , Has (Throw DbError) sig m
       , Has (Lift IO) sig m
       )
    => m ()
app = do
  schemaDir <- (</> "example" </> "schema") <$> sendM getDataDir
  migrate schemaDir MigrateVerbosely >>= sendM . print

  fn <- Text.pack <$> sendM (putStr "Enter your given/first name: "   >> getLine)
  ln <- Text.pack <$> sendM (putStr "And now your family/last name: " >> getLine)

  createNewPerson fn ln
  fetchEveryone >>= mapM_ (sendM . print)

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
  config <- defaultConfig . Text.pack <$> getEnv "DB_CONN"
  runtime <- initRuntime config (Just store)

  -- Makes the name prompting in 'app' nicer.
  hSetBuffering stdout NoBuffering

  -- Run the app by discharging all of the effects.
  result <- app & runDatabase runtime & runThrow & runM

  -- Print out the result (which should be @Right ()@) and the EKG store:
  print (result :: Either DbError ())
  print =<< Metrics.sampleAll store
