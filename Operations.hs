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
        liftIO $ insertDB x y "+" z
    provideJson $ c
  where
    z = (fromIntegral x :: Double) + (fromIntegral y :: Double)
    c = Calc x y Plus z

getSubR :: Int -> Int -> Handler TypedContent
getSubR x y = selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Subtraction"
      [whamlet|#{x} - #{y} = #{z}|]
      liftIO $ insertDB x y "-" z
    provideJson $ c
  where
    z = (fromIntegral x :: Double) - (fromIntegral y :: Double)
    c = Calc x y Minus z

getDivR :: Int -> Int -> Handler TypedContent
getDivR x y = selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Division"
      [whamlet|#{x} / #{y} = #{z}|]
      liftIO $ insertDB x y "/" z
    provideJson $ c
  where
    z = (fromIntegral x :: Double) / (fromIntegral y :: Double)
    c = Calc x y Divide z

getMultR :: Int -> Int -> Handler TypedContent
getMultR x y = selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Multiplication"
      [whamlet|#{x} * #{y} = #{z}|]
      liftIO $ insertDB x y "*" z
    provideJson $ c
  where
    z = (fromIntegral x :: Double) * (fromIntegral y :: Double)
    c = Calc x y Multiply z
