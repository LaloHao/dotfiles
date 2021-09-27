module XMonad.Hao (scratch, scratchpads) where
import qualified XMonad.StackSet                 as W
import           XMonad.ManageHook
import XMonad.Util.NamedScratchpad

scratch = namedScratchpadAction scratchpads

scratchpads = [
    NS "editor" "emacsclient -c" (className =? "Emacs") nonFloating
  , NS "browser" "google-chrome-unstable" (className =? "Google-chrome-unstable") nonFloating
  , NS "audio" "pavucontrol" (className =? "Pavucontrol") floatCenter
  , NS "terminal" "termonad" (className =? "Termonad") nonFloating
  , NS "directory" "dolphin" (className =? "dolphin")  nonFloating
  , NS "bluetooth" "blueman-manager" (className =? ".blueman-manager-wrapped")  floatCenter
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    floatCenter = customFloating $ W.RationalRect (1/4) (1/4) (1/4) (1/4)
