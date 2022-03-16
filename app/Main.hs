module Main where

import Graphics.UI.Gtk hiding (get)
import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.Trans (liftIO)
import Control.Concurrent.Lifted (fork, threadDelay, killThread, ThreadId)
import Control.Monad.Trans.State.Lazy
import Data.Vector (Vector)

import GUIData

data Env = Env  {
                      mainMenu      :: MainMenu
                    , animThread    :: MVar ThreadId
                    , animFrameId   :: MVar Int
                    , frames        :: MVar (Vector Pixbuf)
                    , isPaused      :: MVar Bool
                    , ori           :: Pixbuf   -- just as example / for demonstration purposes
                    , v2            :: Pixbuf   -- 
                }

main :: IO()
main = do
    initGUI
    mainMenu <- loadMainMenu
    rangeSetRange (time_scale mainMenu) (0::Double) (999::Double)
    widgetShowAll $ main_window mainMenu

    ori <- pixbufNewFromFile "app/images/germany_counties.svg"
    v2  <- pixbufNewFromFile "app/images/germany_counties_green.svg"
    
    -- global variables
    animThread  <- newEmptyMVar
    animFrameId <- newMVar 0
    frames      <- newEmptyMVar
    isPaused    <- newMVar True

    let env = Env mainMenu animThread animFrameId frames isPaused ori v2

    on (play_button mainMenu) buttonActivated $ evalStateT startStopAnimation env
    on (main_window mainMenu) objectDestroy mainQuit
    mainGUI


-- abbreviations for readability
takeMVarE var       = get >>= (\env -> liftIO $ takeMVar (var env))
putMVarE  var val   = get >>= (\env -> liftIO $ putMVar  (var env) val)
readMVarE var       = get >>= (\env -> liftIO $ readMVar (var env))
swapMVarE var val   = get >>= (\env -> liftIO $ void $ swapMVar (var env) val)


startStopAnimation :: StateT Env IO ()
startStopAnimation = do
    paused <- takeMVarE isPaused

    if paused then do
        putMVarE isPaused False
        (fork $ doAnimation True) >>= putMVarE animThread
    
    else do
        takeMVarE animThread >>= killThread
        putMVarE isPaused True


-- TODO: get pixbuf from frames
doAnimation :: Bool -> StateT Env IO () -- "Bool ->" just for demo
doAnimation isGreen = do
    paused <- readMVarE isPaused
    unless paused $ do
        id <- takeMVarE animFrameId
        putMVarE animFrameId (id + 1)

        env <- get
        let buffer = if isGreen then v2 env else ori env
        display id buffer
        
        threadDelay 10000
        when (id < 999) $ doAnimation (not isGreen)


display :: Int -> Pixbuf -> StateT Env IO ()
display n buffer = do
    env <- get
    let menu = mainMenu env
    liftIO $ postGUIAsync $ do
        imageSetFromPixbuf (map_image menu) buffer
        set (time_scale menu) [rangeValue := fromIntegral n]
    -- TODO: set time_label