{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell#-}
module Home where

import Foundation
import Yesod.Core
import DBHandler
import  Text.Lucius

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
    calcs <- liftIO $ selectDB
    [whamlet|
    <div> Hello
    <div>
      $forall Calculation lOp rOp oper res <- calcs
        <ul>
          <li>#{show lOp}
          <li>#{show oper}
          <li>#{show rOp}
          <li> = #{show res}
    |]
    toWidget $(luciusFile "style.lucius")
