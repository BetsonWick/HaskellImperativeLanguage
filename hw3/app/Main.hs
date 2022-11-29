import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      inp <- getInputLine "% "
      case inp of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          evaluated <- eval $ exprParsed $ parse input
          case evaluated of
            Left e -> outputStrLn $ show e
            Right res -> outputStrLn $ show res
          loop
