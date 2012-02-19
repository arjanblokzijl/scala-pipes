{-#LANGUAGE ScopedTypeVariables#-}

import Control.Pipe.Common
import Control.Monad
import Control.Monad.Trans
import Control.Applicative


prompt :: Producer Int IO a
prompt = forever $ do
    lift $ putStrLn "Enter a number: "
    n <- read <$> lift getLine
    yield n

take' :: Int -> Pipe a a IO ()
take' n = do
    replicateM_ n $ do
        x <- await
        yield x
    lift $ putStrLn "You shall not pass!"

fromList :: (Monad m) => [a] -> Pipe Zero a m ()
fromList = mapM_ yield

printer :: (Show a) => Pipe a Zero IO b
printer = forever $ do
    x <- await
    lift $ print x


pipeline :: Pipeline IO ()
pipeline = printer <+< take' 3 <+< fromList [1..]
