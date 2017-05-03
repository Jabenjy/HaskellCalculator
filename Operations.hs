{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Operations where

import Foundation
import Yesod.Core
import Calc
import DBHandler

getAddR :: Int -> Int -> Handler TypedContent
getAddR x y = selectRep $ do
    provideRep $ defaultLayout $ do
        setTitle "Addition"
        [whamlet|#{x} + #{y} = #{z}|]
    --provideJson $ c
        liftIO $ insertDB x y "+" z
  where
    z = (fromIntegral x :: Double) + (fromIntegral y :: Double)
    -- c = Calculation x y Plus a

getSubR :: Int -> Int -> Handler TypedContent
getSubR x y = selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Subtraction"
      [whamlet|#{x} - #{y} = #{z}|]
    --provideJson $ c
      liftIO $ insertDB x y "-" z
  where
    z = (fromIntegral x :: Double) - (fromIntegral y :: Double)
    -- c = Calculation x y Minus a


getDivR :: Int -> Int -> Handler TypedContent
getDivR x y = selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Division"
      [whamlet|#{x} / #{y} = #{z}|]
    -- provideJson $ c
      liftIO $ insertDB x y "/" z
  where
    z = a / b
      where
        a = fromIntegral x :: Double
        b = fromIntegral y :: Double
    -- c = Calculation x y Divide fromDouble(z :: Float)

getMultR :: Int -> Int -> Handler TypedContent
getMultR x y = selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Multiplication"
      [whamlet|#{x} * #{y} = #{z}|]
    --provideJson $ c
      liftIO $ insertDB x y "*" a
  where
    z = x * y
    a = fromIntegral z :: Double
    -- c = Calculation x y Multiply z
