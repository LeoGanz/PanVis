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
                    , animThread    :: MVar (Maybe ThreadId)
                    , animFrameId   :: MVar Int
                    , frames        :: MVar (Maybe (Vector Pixbuf))
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

    ori <- pixbufNewFromFile "images/germany_counties.svg"
    v2  <- pixbufNewFromFile "images/germany_counties_green.svg"
    
    -- global variables
    animThread  <- newMVar Nothing
    animFrameId <- newMVar 0
    frames      <- newMVar Nothing
    isPaused    <- newMVar True

    let env = Env mainMenu animThread animFrameId frames isPaused ori v2

    on (play_button mainMenu) buttonActivated $ evalStateT startStopAnimation env
    on (main_window mainMenu) objectDestroy mainQuit
    mainGUI


startStopAnimation :: StateT Env IO ()
startStopAnimation = do
    env <- get
    paused <- liftIO $ takeMVar $ isPaused env

    if paused then do
        -- set paused state to false
        liftIO $ void $ putMVar (isPaused env) False

        -- start animation and store its threadId
        threadId <- fork $ doAnimation True
        liftIO $ void $ swapMVar (animThread env) $ Just threadId
    
    else do
        -- kill old animation thread
        maybeThreadId <- liftIO $ takeMVar $ animThread env
        forM_ maybeThreadId killThread
        liftIO $ putMVar (animThread env) Nothing

        -- set paused state to true
        liftIO $ void $ putMVar (isPaused env) True


-- TODO: get pixbuf from frames
doAnimation :: Bool -> StateT Env IO () -- "Bool ->" just for demo
doAnimation isGreen = do
    env <- get
    paused <- liftIO $ readMVar $ isPaused env
    unless paused $ do
        id <- liftIO $ takeMVar $ animFrameId env
        liftIO $ putMVar (animFrameId env) (id + 1)

        let buffer = if isGreen then v2 env else ori env
        display id buffer
        
        threadDelay 10000
        when (id < 999) $ doAnimation (not isGreen)


display :: Int -> Pixbuf -> StateT Env IO ()
display n buffer = do
    env <- get
    let menu = mainMenu env
    liftIO $ postGUIAsync $ imageSetFromPixbuf (map_image menu) buffer
    liftIO $ postGUIAsync $ set (time_scale menu) [rangeValue := fromIntegral n]
    -- TODO: set time_label