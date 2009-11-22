{-
 Too Much Milk - Solution 1:
   If you find that there is no milk and there is no note on the door of
   the fridge, then leave a note on the fridge's dooor, go and buy milk,
   put the milk in the fridge, and remove your note.

(bob) No notes.
(alice) No notes.
(bob) I go and buy a milk.
(alice) I go and buy a milk.
(alice) I bought a milk.
(bob) I bought a milk.
-}

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Data.IORef
import System.Random

main :: IO ()
main = do
  fridge <- newFridge
  forkIO $ alice fridge
  bob fridge
  getChar
  return ()

alice :: Fridge -> IO ()
alice f = do
  checkNote f "alice" $ do
    checkMilk f "alice" $ do
      leaveNote f
      buyMilk f "alice"
      removeNote f

bob :: Fridge -> IO ()
bob f = do
  checkNote f "bob" $ do
    checkMilk f "bob" $ do
      leaveNote f
      buyMilk f "bob"
      removeNote f

data Fridge = Fridge { note :: Note
                     , milk :: Milk }

type Note = IORef (Maybe ())
type Milk = IORef Int

newFridge :: IO Fridge
newFridge = Fridge <$> newIORef Nothing
                   <*> newIORef 0

checkNote :: Fridge -> String -> IO () -> IO ()
checkNote fridge name cont = do
  n <- readIORef (note fridge)
  case n of
    Just () -> putStrLn ("(" ++ name ++ ") There is a note!")
    _       -> putStrLn ("(" ++ name ++ ") No notes.") >> cont

checkMilk :: Fridge -> String -> IO () -> IO ()
checkMilk fridge name cont = do
  m <- readIORef (milk fridge)
  case m of
    0 -> cont
    _ -> putStrLn $ name ++ " find a milk! (" ++ show m ++ ")"

leaveNote :: Fridge -> IO ()
leaveNote = flip writeIORef (Just ()) . note

removeNote :: Fridge -> IO ()
removeNote = flip writeIORef Nothing . note

buyMilk :: Fridge -> String -> IO ()
buyMilk fridge name = do
  (r, _) <- randomR (1, 5) <$> newStdGen
  putStrLn $ "(" ++ name ++ ") I go and buy a milk."
  threadDelay (r*10^6)
  putStrLn $ "(" ++ name ++ ") I bought a milk."
  modifyIORef (milk fridge) (+1)
