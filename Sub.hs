{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Sub where

import Calc
import Foundation
import Yesod.Core

getSubR :: Int -> Int -> Handler TypedContent
getSubR x y = selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Subtraction"
      [whamlet|#{x} - #{y} = #{z}|]
    provideJson $ c
  where
    z = x - y
    c = Calculation x y Minus a
      where a = fromIntegral z :: Float
