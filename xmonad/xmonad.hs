import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Master
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.WindowNavigation as WN
import XMonad.StackSet as S
import XMonad.Operations as O
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import System.IO
import System.Exit
import qualified Data.Map as M

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
        , layoutHook      = avoidStruts myLayout
        , handleEventHook = docksEventHook <+> handleEventHook def
        , startupHook     = docksStartupHook <+> startupHook def
        , logHook         = dynamicLogWithPP xmobarPP
            { ppOutput        = hPutStrLn xmproc
            , ppTitle         = xmobarColor "#999999" "" . shorten 50
            }
        , normalBorderColor  = "#000000"
        , focusedBorderColor = "#cc0000"
        , borderWidth     = 4
        }
    in additionalKeysP c $ myKeys c

myKeys :: XConfig a -> [(String, X ())]
myKeys c =
    [ ("M-<Return>"     , spawn myWebBrowser)
    , ("M-S-<Return>"   , spawn $ terminal c)
    , ("M-p"            , spawn "rofi -show run")
    , ("M-b"            , sendMessage ToggleStruts)
    , ("M-q"            , spawn "killall xmobar; xmonad --recompile; xmonad --restart")
    , ("M-S-q"          , io exitSuccess)
    , ("M-h"            , sendMessage $ WN.Go L)
    , ("M-s"            , sendMessage $ WN.Go R)
    , ("M-n"            , sendMessage $ WN.Go U)
    , ("M-t"            , sendMessage $ WN.Go D)
    , ("M-S-h"          , sendMessage Expand)
    , ("M-S-s"          , sendMessage Shrink)
    , ("M-S-<Space>"    , O.windows S.swapMaster)
    , ("M-S-n"          , O.windows S.swapUp)
    , ("M-S-t"          , O.windows S.swapDown)
    ]

myLayout = WN.configurableNavigation (WN.navigateColor "#440000") $
    WN.windowNavigation $
        noBorders Full
    ||| Tall 1 (1/100) (3/4)
    ||| Mirror (Tall 1 (1/100) (3/4))
    ||| emptyBSP
