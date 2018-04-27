import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import System.IO
import System.Exit

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ myConfig xmproc

myTerminal = "urxvt"
myWebBrowser = "firefox"

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myConfig xmproc =
    let c = def {
          terminal        = myTerminal
        , modMask         = mod1Mask
        , manageHook      = manageDocks <+> manageHook def
        , layoutHook      = avoidStruts $ layoutHook def
        , handleEventHook = docksEventHook <+> handleEventHook def
        , startupHook     = docksStartupHook <+> startupHook def
        , logHook         = dynamicLogWithPP xmobarPP
            { ppOutput        = hPutStrLn xmproc
            , ppTitle         = xmobarColor "#999999" "" . shorten 50
            }
        , borderWidth     = 1
        }
    in additionalKeysP c $ myKeys c

myKeys :: XConfig a -> [(String, X ())]
myKeys c =
    [ ("M-<Return>",    spawn myWebBrowser)
    , ("M-S-<Return>",  spawn $ terminal c)
    , ("M-p",           spawn "rofi -show run")
    , ("M-b",           sendMessage ToggleStruts)
    , ("M-q",           spawn "killall xmobar; xmonad --recompile; xmonad --restart")
    , ("M-S-q",         io exitSuccess)
    ]
