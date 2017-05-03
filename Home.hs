{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Home where

import Foundation
import Yesod.Core
import DBHandler
import Text.Blaze


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Minimal Multifile"
    [whamlet|
        <p>
            <a href=@{AddR 5 7}>HTML addition
        <p>
            <a href=@{AddR 5 7}?_accept=application/json>JSON addition
    |]

getCalcsR :: Handler Html
getCalcsR = defaultLayout $ do
    setTitle "History"
    let calcs = selectDB
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
