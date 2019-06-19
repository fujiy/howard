module Howard.REPL where

import           Control.Monad
import           Data.Maybe
import qualified Data.Text                as T
import           System.Console.Haskeline

import           Howard.Expr
import           Howard.Syntax
import           Howard.TypeCheck

repl :: IO ()
repl = do
    putStrLn "Howard Interpreter."
    runInputT defaultSettings main

  where
    main :: InputT IO ()
    main = do
        let env = mempty
        env <- loadDecl env "Type : Set2 := Set1"
        env <- loadDecl env "data True : Type |= true : True"
        env <- loadDecl env "data False : Type |= "
        loop env

    loop :: Envir -> InputT IO ()
    loop env = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just input | last input == '.' -> do
                env' <- loadDecl env $ init input
                loop env'
            Just input -> do
                evalExpr env input
                loop env

    loadDecl :: Envir -> String -> InputT IO Envir
    loadDecl env input =
        case parseDecl $ T.pack input of
            Left err -> do
                outputStrLn err
                return env
            Right d  -> do
                maybe (return ()) outputStrLn $ printDecl d
                case typeCheckDecl env d of
                    Left es -> do
                        mapM_ (outputStrLn . printTypeError) es
                        return env
                    Right [env'] -> return $ env <> env'
                    Right ts -> do
                        outputStrLn "Ambiguous"
                        return env

    evalExpr :: Envir -> String -> InputT IO ()
    evalExpr env input =
        case parseExpr $ T.pack input of
            Left err ->
                outputStrLn err
            Right e  -> do
                maybe (return ()) outputStrLn $ printExpr e
                case typeCheckExpr env e of
                    Left es -> mapM_ (outputStrLn . printTypeError) es
                    Right [t] -> do
                        outputStr " : "
                        maybe (return ()) outputStrLn $ printExpr t
                    Right ts -> do
                        outputStrLn "Ambiguous of"
                        mapM_ outputStrLn $ mapMaybe printExpr ts


