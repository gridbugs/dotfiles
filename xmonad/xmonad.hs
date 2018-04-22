import XMonad
import XMonad.Hooks.DynamicLog

main = xmonad myConfig

myConfig = def
    { terminal    = "urxvt"
  , modMask     = mod4Mask
  , borderWidth = 2
  }

