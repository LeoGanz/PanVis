module GUIData where

import Graphics.UI.Gtk


data MainMenu = MainMenu  {
                              main_window   :: Window
                            , main_fixed    :: Fixed
                            , map_box       :: Box
                            , map_image     :: Image
                            , time_box      :: Box
                            , play_button   :: Button
                            , play_image    :: Image
                            , time_scale    :: Scale
                            , time_adjust   :: Adjustment
                            , time_label    :: Label
                        }


loadMainMenu = do
    builder <- builderNew
    builderAddFromFile builder "app/convis.glade"
    main_window <- builderGetObject builder castToWindow      "main_window"
    main_fixed  <- builderGetObject builder castToFixed       "main_fixed"
    map_box     <- builderGetObject builder castToBox         "map_box"
    map_image   <- builderGetObject builder castToImage       "map_image"
    time_box    <- builderGetObject builder castToBox         "time_box"
    play_button <- builderGetObject builder castToButton      "play_button"
    play_image  <- builderGetObject builder castToImage       "play_image"
    time_scale  <- builderGetObject builder castToScale       "time_scale"
    time_adjust <- builderGetObject builder castToAdjustment  "time_adjust"
    time_label  <- builderGetObject builder castToLabel       "time_label"
    return $ MainMenu
                main_window
                main_fixed
                map_box
                map_image
                time_box
                play_button
                play_image
                time_scale
                time_adjust
                time_label