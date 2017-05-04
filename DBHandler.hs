{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module DBHandler where

import Yesod
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Calculation
    lOperand Int
    rOperand Int
    operator String
    result Double
    deriving Show
|]

-- Not used, but would allow a Calculation to be turned into a JSON object
instance ToJSON Calculation where
  toJSON (Calculation lO rO op res) = object ["lOperand" .= lO,
                                             "rOperand" .= rO,
                                             "operator" .= op,
                                             "result" .= res]

-- Insert Calculation into the database
insertDB :: Int -> Int -> String -> Double -> IO ()
insertDB x y o z = runSqlite "calculations.db" $ do
    runMigration migrateAll
    calcID <- insert $ Calculation x y o z
    liftIO $ print calcID
    calc <- get calcID
    liftIO $ print calc
    return ()

-- Return a list of Calculations retrieveed from the database
selectDB :: IO[Calculation]
selectDB = runSqlite "calculations.db" $ do
    runMigration migrateAll
    -- This was the only way I ended up being able to retrieve every entry
    calcsEnts <- selectList ([CalculationResult >=. 0] ||. [CalculationResult <. 0]) []
    let calcs = map getCalculations calcsEnts
    liftIO $ return (calcs)
    where
      getCalculations (Entity _ x) = x

--can be mapped over a list of Calculation's to return a list of JSON objects
-- getJsonCalcs a = toJSON a
