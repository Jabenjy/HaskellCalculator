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
import Foundation
import Yesod
import Network.HTTP.Types
-- import Database.Persist
-- import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control

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
    -- html <- printCalculations calcs
    -- liftIO $ print calcs
    -- result <- printCalculations calcs
    -- liftIO $ sendResponseStatus status201 result
    where
      getCalculations (Entity _ x) = x
    -- let calcsJson = map getJsonCalcs calcs
    -- printCalculations calcs

-- printCalculations :: [Calculation] -> Handler Html
-- printCalculations calcs = defaultLayout $ do
--       setTitle "History"
--       toWidget[whamlet|
--       <div>
--         <dl>
--           $forall Calculation a b c d <- calcs
--             <dt>#{show a}
--       |]


printCalculations calcs = defaultLayout $ do
    setTitle "History"
    toWidget[whamlet|
    <div>
      $forall Calculation a b c d <- calcs
        <dl>
          <dt>#{show a}
          <dt>#{show b}
          <dt>#{show c}
          <dt>#{show d}
    |]

getJsonCalcs a = toJSON a

-- getLOpa (CalcDB x _ _ _) = x
-- getROpa (CalcDB _ x _ _) = x
-- getOper (CalcDB _ _ x _) = x
-- getRes (CalcDB _ _ _ x) = x


--
-- -- fetchDB :: IO ()
-- fetchDB = do
--     runMigration migrateAll
--     calcs <- runDB $ selectList [CalcDBResult >. 0] []
--     liftIO $ print calcs
