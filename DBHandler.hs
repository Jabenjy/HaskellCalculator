{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
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

instance ToJSON Calculation where
  toJSON (Calculation lO rO op res) = object ["lOperand" .= lO,
                                             "rOperand" .= rO,
                                             "operator" .= op,
                                             "result" .= res]

insertDB :: Int -> Int -> String -> Double -> IO ()
insertDB x y o z = runSqlite "calculations.db" $ do
    runMigration migrateAll
    calcID <- insert $ Calculation x y o z
    liftIO $ print calcID

    calc <- get calcID
    liftIO $ print calc

    return ()

selectDB :: IO[Calculation]
selectDB = runSqlite "calculations.db" $ do
    runMigration migrateAll

    calcsEnts <- selectList ([CalculationResult >=. 0] ||. [CalculationResult <. 0]) []
    let calcs = map getCalculations calcsEnts
    liftIO $ return (calcs)
    where
      getCalculations (Entity _ x) = x

-- getJsonCalcs a = toJSON a
