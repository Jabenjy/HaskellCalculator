{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Div where

import Foundation
import Yesod.Core
import Calc

getDivR :: Int -> Int -> Handler TypedContent
getDivR x y = selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Division"
      [whamlet|#{x} / #{y} = #{z}|]
    provideJson $ c
  where
    z = a / b
      where
        a = fromIntegral x :: Float
        b = fromIntegral y :: Float
    c = Calculation x y Divide z
