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
            <a href=@{AddR 5 7}>Addition
        <p>
            <a href=@{MultR 5 7}>Multiplication
        <p>
            <a href=@{SubR 5 7}>Subtraction
        <p>
            <a href=@{DivR 5 7}>Division
        <p>
            <a href=@{CalcsR}>History
    |]

getCalcsR :: Handler Html
getCalcsR = defaultLayout $ do
    setTitle "Calculations"
    calcs <- liftIO $ selectDB
    [whamlet|
    <div> Here are all of the calculations stored in the database:
    <div>
      $forall Calculation lOp rOp oper res <- calcs
        <ul>
          <li>#{show lOp}
          <li>#{oper}
          <li>#{show rOp}
          <li> = #{show res}
    |]
    toWidget $(luciusFile "style.lucius")
