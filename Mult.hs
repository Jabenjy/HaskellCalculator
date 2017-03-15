{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Mult where

import Foundation
import Yesod.Core
import Calc

getMultR :: Int -> Int -> Handler TypedContent
getMultR x y = selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Multiplication"
      [whamlet|#{x} * #{y} = #{z}|]
    provideJson $ c
  where
    z = x * y
    c = Calculation x y Multiply a
      where a = fromIntegral z :: Float
