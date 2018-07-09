{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))
import System.Random
import System.IO.Unsafe
import System.ReadEnvVar (readEnvDef)
import qualified PDP11 as PDP
import qualified Assembler as PA
import qualified Simulator as PS
import Web.FormUrlEncoded(FromForm(..), ToForm(..))

version :: String
version = "0.6.3.0"

data Code = Code
 {
   program   :: String
 , randomize :: String
 } deriving (Eq, Show, Generic)

instance FromForm Code

type API = "run" :> ReqBody '[FormUrlEncoded] Code :> Post '[HTML] H.Html
           :<|> Get '[HTML] H.Html

startApp :: IO ()
startApp = do
  port <- readEnvDef "PORT" 8080
  putStrLn $ ";;; start server at " ++ show port
  run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = (\d ->  return (resultPage d)) :<|> return homePage

homePage :: H.Html
homePage = H.docTypeHtml $ do
  H.head $ do
    H.title . H.toHtml $ "PDP11 simulator (version " ++ version ++ ")"
    H.link ! A.rel "stylesheet" ! A.href "https://use.fontawesome.com/releases/v5.1.0/css/all.css"
    H.style ! A.type_ "text/css" $ "<!-- \n\
\html {background:url(https://1.bp.blogspot.com/-4VU3tl2Esrg/WGnPOsl5IRI/AAAAAAABA1s/ep6WFxjyMWo9FqTMqhlgukBC18Lww4ZHACLcB/s800/computer_supercomputer_red.png) right top fixed no-repeat;\n\
\      background-size:200px;}\n\
\table,tr,td,th {border:1px black solid;border-collapse:collapse;font-family:monospace;}\n\
\textarea {margin:8px;padding:8px;}\n\
\th {text-align:center;}\n\
\td {text-align:right;font-family:monospace;width:26px;padding:2px;}\n\
\.opcode {text-align:left;width:110px;padding-left:4px;}\n\
\pre code {font-family:monospace;}\n\
\-->"
  H.body $ do
    H.h1 "Welcome to a PDP11 simulator"
    H.h1 $ do
      H.i ! A.class_ "fas fa-keyboard" ! A.style "padding-right:4pt;" $ " "
      H.span "Assembly Code"
    H.p $
      H.form ! A.method "POST" ! A.action "run" $ do
      H.p $ H.textarea ! A.name "program" ! A.cols "40" ! A.rows "10" $ "SUB R0, R0\nMOV #10, (R0)+\nMOV #7, (R0)+\nMOV #2018, (R0)+\nJMP #102\n"
      H.p $ do
        H.button ! A.type_ "submit" ! A.name "action" ! A.value "send" ! A.style "margin-left: 180px;" $ do
          H.i ! A.class_ "fas fa-play" ! A.style "padding-right:4pt;" $ " "
          H.span "RUN"
        H.input ! A.type_ "hidden" ! A.name "randomize" ! A.value "RandomizeOff"
    H.hr
    H.p ! A.style "text-align: right" $ "powered by Servant, a type-safe web server written in Haskell."
    H.p ! A.style "text-align: right;" $ H.toMarkup ("version " ++ version ++ " by nrzk, nagasaki-u.")
    H.p ! A.style "text-align: right;" $ "The BG image is by いらすとや."

resultPage :: Code -> H.Html
resultPage (Code str randomize') = H.docTypeHtml $ do
  let rnd = randomize' == "randomize on"
  H.head $ do
    H.link ! A.rel "stylesheet" ! A.href "https://use.fontawesome.com/releases/v5.1.0/css/all.css"
    H.style ! A.type_ "text/css" $ "<!-- \n\
\table,tr,td,th {border:1px black solid;border-collapse:collapse;font-family:monospace;}\n\
\textarea {margin:8px;padding:8px;}\n\
\th {text-align:center;}\n\
\td {text-align:right;font-family:monospace;width:26px;padding:2px;}\n\
\.opcode {text-align:left;width:140px;padding-left:4px;}\n\
\pre code {font-family:monospace;}\n\
\-->"
    H.title "A PDP11 simulator"
  H.body $ do
    H.h1 $ do
      H.i ! A.class_ "fas fa-keyboard" ! A.style "padding-right:4pt;" $ " "
      H.toMarkup $ "Assembly Code (ver. " ++ PA.version ++ ")"
    H.p $ do
      H.form ! A.method "POST" ! A.action "run" $ do
        H.p $ H.textarea ! A.name "program" ! A.cols "40" ! A.rows "10" $ H.toMarkup str
        H.p $ do
          H.select ! A.id "randomize" ! A.name "randomize" $ do
            H.optgroup ! A.label "Memory Randomization" $ do
              if rnd
                then do
                  H.option ! A.selected  "randomize on" $   "randomize on"
                  H.option "randomize off"
                else do
                  H.option "randomize on"
                  H.option ! A.selected "randomize off" $ "randomize off"
          H.button ! A.type_ "submit" ! A.name "action" ! A.value "send" ! A.style "margin-left: 180px;" $ do
            H.i ! A.class_ "fas fa-play" ! A.style "padding-right:4pt;" $ " "
            H.span "UPDATE"
    let prg = shaping str
    if null prg
      then do
        H.p "You sent an empty program."
        H.a ! A.href "/" $ "RESET"
      else do
        case showBinaryCode 100 <$> PA.assemble prg of
          Left str -> return ()
          Right l -> do
            H.h1 $ do
              H.i ! A.class_ "fas fa-download" ! A.style "padding-right:4pt;" $ " "
              H.toMarkup $ "Binary Representation (ver. " ++ PDP.version ++ ")"
            H.pre ! A.style "width:520px;background:#f8f8f8;border:1px solid #777;margin:8px;padding:8px;" $ H.code $ H.toMarkup (concatMap (++ "\n") l)
        H.h1 $ do
          H.i ! A.class_ "fas fa-microchip" ! A.style "padding-right:4pt;" $ " "
          H.toMarkup $ "Execution Trace (ver. " ++ PS.version ++ ")"
        let mac = unsafePerformIO $ makeMachine rnd
        case repl mac prg of
          Left str -> H.pre ! A.style "background: #fee;" $ H.toMarkup str
          Right lst -> do
            H.table $ do
              H.tr $ do
                H.th ! A.rowspan "2" $ "PC"
                H.th ! A.rowspan "2" $ "Instruction"
                H.th ! A.colspan "12" ! A.style "background:#efe;" $ "Memory 11 - 0"
                H.th ! A.colspan "8"  ! A.style "background:#eef;" $ "Register 7 - 0"
                H.th ! A.colspan "4"  ! A.style "background:#fee;" $ "PSW"
              H.tr $ do
                mapM_ (\m -> H.th ! A.style "background:#efe;" $ (H.toMarkup (show m))) (reverse [0 .. 11])
                mapM_ (\r -> H.th ! A.style "background:#eef;" $ (H.toMarkup ("R" ++ show r))) (reverse [0 .. 7])
                mapM_ (\r -> H.th ! A.style "background:#fee;" $ r) ["N", "Z", "V", "C"]
              mapM_ (\(ms, rs, psw, addr, asm) ->
                       H.tr ! A.style "border: 1pt;" $ do
                        H.td ! A.class_ "PC" $ H.toMarkup (if addr == -1 then "" else show addr)
                        H.td ! A.class_ "opcode" $ H.toMarkup (if addr == -1 then "" else show asm)
                        mapM_ (\m -> H.td ! A.style "background:#efe;" $ (H.toMarkup (show m))) (reverse (take 12 ms))
                        mapM_ (\r -> H.td ! A.style "background:#eef;" $ (H.toMarkup (show r))) (reverse (take 8 rs))
                        mapM_ (\f -> H.td ! A.style "background:#fee;" $ (H.toMarkup (show f))) psw
                    ) lst
              H.tr ! A.style "background;#fcc;" $ H.td ! A.colspan "26" ! A.style "color:red;text-align:center;" $
                if 64 <= length lst
                  then do
                    H.i ! A.class_ "fas fa-hourglass-end" ! A.style "padding-right:4pt;" $ " "
                    H.span "Your time slice expires."
                  else do
                    H.i ! A.class_ "fas fa-power-off" ! A.style "padding-right:4pt;" $ " "
                    H.span $ "Your program terminated because the PC pointed to an illegal address."
    H.p ! A.style "text-align: right;" $ do
      H.a ! A.href "mailto:incoming+cisl407/classroom/pdp11-server@incoming.gitlab.com" $ H.toMarkup ("Mail a bug on version " ++ version)

shaping :: String -> String
shaping str = unlines . filter (not . null) . map trim . lines $ str
  where trim = reverse . dropWhile (`elem` [' ', '\t', '\r']) . reverse . dropWhile (`elem` [' ', '\t'])

repl :: PDP.Machine -> String -> Either String [([Int], [Int], [Int], Int, PDP.ASM)]
repl machine str = map PDP.dump <$> PS.runSimulator 64 machine <$> PA.assemble str

makeMachine :: Bool -> IO PDP.Machine
makeMachine False = return PDP.initialMachine
makeMachine True = do
  r1 <- getStdRandom (randomR (0, 128))
  r2 <- getStdRandom (randomR (0, 128))
  r3 <- getStdRandom (randomR (0, 256))
  r4 <- getStdRandom (randomR (0, 300))
  r5 <- getStdRandom (randomR (0, 2000))
  return $ PDP.makePDP11 [ 0, 0
                         , mod r1 256, div r1 256
                         , mod r2 256, div r2 256
                         , mod r3 256, div r3 256
                         , mod r4 256, div r4 256
                         , mod r5 256, div r5 256
                         ] [0,0,0,0,0,0,0,100]

showBinaryCode :: Int -> [PDP.ASM] -> [String]
showBinaryCode start prg = zipWith merge [start, start + 2 ..] $  concatMap (printer . PDP.toBitBlocks) prg
  where printer (oc:ols) = l1 : map show ols
          where l1 = show oc ++ "         # " ++ show (PDP.decodeWord (PDP.asInt oc) (ols' !! 0, ols' !! 1))
                ols' = map PDP.asInt ols ++ [0, 0]
        merge n s = show n ++ "        " ++ s
