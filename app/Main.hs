{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant
import Text.Megaparsec (runParser)
import CubeSolver.CubeParser (parseCubeState)
import qualified Data.Text as T
import CubeSolver.CFOP.CFOP (cfop)
import Control.Monad.State (evalState, MonadIO (liftIO))
import qualified Data.ByteString as BS
import CubeSolver.PDFCube (generatePDFSolution)

main :: IO ()
main = runServer

type CubeSolverApi = "api" :> "cubesolver" :> "solve" :> QueryParam "cube" String :> Get '[OctetStream] (Headers '[Header "Content-Disposition" String, Header "Content-Type" String] BS.ByteString)
                :<|> "api" :> "cubesolver" :> "validate" :> QueryParam "cube" String :> Get '[PlainText] String

solveCube :: Maybe String -> Handler (Headers '[Header "Content-Disposition" String, Header "Content-Type" String] BS.ByteString)
solveCube Nothing = return $ addHeader "attachment; filename=\"solution_manual.pdf\"" $ addHeader "application/pdf" "No parameter given\n"
solveCube (Just unparsedCube) = do
    let parsedResult = runParser parseCubeState "" (T.pack unparsedCube)
    case parsedResult of
        Left _ -> return $ addHeader "attachment; filename=\"solution_manual.pdf\"" $ addHeader "application/pdf" "Parsing failed\n"
        Right cubeState -> do
            liftIO $ generatePDFSolution (evalState cfop cubeState) cubeState 
            res <- liftIO $ BS.readFile "solution_manual.pdf"
            return $ addHeader "attachment; filename=\"solution_manual.pdf\"" $ addHeader "application/pdf" res

validateCube :: Maybe String -> Handler String
validateCube Nothing = return "false"
validateCube (Just unparsedCube) = do
    let parsedResult = runParser parseCubeState "" (T.pack unparsedCube)
    case parsedResult of
        Left _ -> return "false"
        Right _ -> return "true"

server :: Server CubeSolverApi
server = solveCube :<|> validateCube

api :: Proxy CubeSolverApi
api = Proxy

app :: Application
app = serve api server

runServer :: IO ()
runServer = do
    putStrLn "Server starting on http://localhost:8080/"
    run 8080 app
