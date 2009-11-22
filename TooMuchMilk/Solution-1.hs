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
import Text.Printf

main :: IO ()
main = do
  fridge <- newFridge
  forkIO $ runReaderT alice (fridge, "alice")
  runReaderT bob (fridge, "bob")
  getChar
  return ()

type Job = ReaderT (Fridge, String) IO

alice :: Job ()
alice = do
  checkNote $ do
    checkMilk $ do
      leaveNote
      buyMilk
      removeNote

bob :: Job ()
bob = do
  checkNote $ do
    checkMilk $ do
      leaveNote
      buyMilk
      removeNote

data Fridge = Fridge { note :: Note
                     , milk :: Milk }

type Note = IORef (Maybe ())
type Milk = IORef Int

newFridge :: IO Fridge
newFridge = Fridge <$> newIORef Nothing
                   <*> newIORef 0

-- checkNote :: Job () -> Job ()
checkNote cont = do
  (fridge, name) <- ask
  n <- liftIO $ readIORef $ note fridge
  case n of
    Just () ->  liftIO $ putStrLn $ printf "(%s) There is a note!" name
    _       -> (liftIO $ putStrLn $ printf "(%s) No notes." name) >> cont

-- checkMilk :: Job () -> Job ()
checkMilk cont = do
  (fridge, name) <- ask
  m <- liftIO $ readIORef $ milk fridge
  case m of
    0 -> (liftIO $ putStrLn $ printf "(%s) No milks." name) >> cont
    _ ->  liftIO $ putStrLn $ printf "(%s) There is a milk! (%d)" name m

leaveNote :: Job ()
leaveNote = do
  (fridge, _) <- ask
  liftIO $ writeIORef (note fridge) (Just ())

removeNote :: Job ()
removeNote = do
  (fridge, _) <- ask
  liftIO $ writeIORef (note fridge) Nothing

buyMilk :: Job ()
buyMilk = do
  (fridge, name) <- ask
  liftIO $ do
    (r, _) <- randomR (1, 5) <$> newStdGen
    putStrLn $ printf "(%s) I go and buy a milk." name
    threadDelay (r*10^6)
    putStrLn $ printf "(%s) I bought a milk." name
    modifyIORef (milk fridge) (+1)
