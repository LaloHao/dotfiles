{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import           XMonad                                 hiding (config,workspaces)
import           Data.Monoid                                   (All(All))
import           Data.Maybe                                    (isJust, maybeToList)
import           Control.Monad                                 (when, liftM2, join, forM_, void)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet                 as W
import           XMonad.ManageHook
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.EZConfig
import           XMonad.Actions.CycleWS                        (shiftTo, moveTo, Direction1D(..), WSType(..))
import           XMonad.Actions.Commands
import           XMonad.Actions.ShowText
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Hooks.ManageHelpers
import           XMonad.Actions.CopyWindow
import qualified Data.Map                        as M
import           XMonad.Actions.Navigation2D
import           XMonad.Util.Run                               (spawnPipe)
import           XMonad.Util.NamedWindows                      (getName, unName)
import           Data.Traversable                              ( traverse )
import           Data.List                                     ((\\), findIndex, isSuffixOf)
import qualified XMonad.Util.ExtensibleState     as XS
import           XMonad.Util.Timer
import           Data.Time
import           System.IO
import           XMonad.Actions.UpdateFocus
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.PerWorkspace                    (onWorkspace)
import           XMonad.Layout.Magnifier         as Mag
import           XMonad.Util.Loggers
import           XMonad.Layout.LayoutCombinators               (JumpToLayout(..))
import           Control.Arrow                                 ((***))
import           Data.Bifunctor                                (bimap)
import           XMonad.Actions.FloatSnap
import qualified XMonad.Util.WorkspaceCompare    as W
import qualified Data.Text                       as T
import           XMonad.Layout.Spacing
import           Data.Word
import           XMonad.Hooks.FadeInactive
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Hooks.InsertPosition
import XMonad.Hao
import XMobar.Task

data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
  initialValue = TID 0

clockStartupHook = do
  timer <- startTimer 1
  XS.put $ TID timer
  return ()

tasksStartupHook = do
  -- tasks <- XS.get :: X TaskList
  -- let t = task 0 TaskRunning "La tarea" 10
  -- XS.put $ TaskList [t]
  return ()

-- hola
clockEventHook :: Handle -> Event -> X All
clockEventHook xmobar e = do               -- e is the event we've hooked
  (TID t) <- XS.get                 -- get the recent Timer id
  now <- liftIO getCurrentTime
  -- (HID xmobar) <- XS.get                 -- get the recent Timer id
  handleTimer t e $ do              -- run the following if e matches the id
    -- (TaskList tasks) <- XS.get
    XS.modify $ \(TaskList tasks) -> TaskList (updateTaskTimer now <$> tasks)
    startTimer 1 >>= XS.put . TID   -- restart the timer, store the new id
    when (stdout /= xmobar) $
      logHook_ xmobar
    -- logHook $ (config xmobar clockEventHook)          -- get the loghook and run it
    return Nothing                  -- return required type
  return $ All True                 -- return required type

-- $ unwords [
--     "xmobar",
--     "--font='xft:Hack:pixelsize=16:antialias=true'",
--     "--add-font='xft:Hack:pixelsize=40:antialias=true'",
--     "-c",
--     "'[Run UnsafeStdinReader, Run UnsafeStdinReader]'",
--     "--template='%UnsafeStdinReader%}{%UnsafeStdinReader%'",
--     "--iconroot=/home/hao/Pictures/icons/16x16",
--     "--position='Static { xpos = 0, ypos = 0, width = 2560, height = 60 }'"
--     ]

main = do
  background <- spawnPipe "feh --bg-scale /home/hao/Pictures/background/background.png"
  xmobar     <- spawnPipe "xmobar ~/.xmobarrc"
  -- kbd1       <- spawnPipe "kmonad ~/dev/dotfiles/keyboard/mechanical.kbd"
  -- kbd2       <- spawnPipe "kmonad ~/dev/dotfiles/keyboard/wireless.kbd"
  picom      <- spawnPipe "picom"
  hSetBuffering xmobar (BlockBuffering (Just 1024))
  xmonad $ docks $ config xmobar

config xmobar = def
  { modMask = mod4Mask
  , logHook = logHook_ xmobar
  -- , clientMask = clientMask def .|. leaveWindowMask
  , borderWidth = 2
  , focusedBorderColor = "#00ff00"
  , normalBorderColor = "#ff0000"
  , layoutHook = layout
  , handleEventHook =  handleEventHook_ <+> clockEventHook xmobar
  , startupHook = startupHook_
  , focusFollowsMouse = True
  , manageHook = manageHook_
  , terminal = "termonad"
  , mouseBindings = mouseBindings_ } `additionalKeys` [
    ((mod3Mask,    xK_t), scratch "terminal")
  , ((mod3Mask,    xK_e), scratch "editor")
  , ((mod3Mask,    xK_Return), scratch "terminal")
  , ((mod3Mask,    xK_d), scratch "directory")

  , ((mod4Mask .|. shiftMask,    xK_b), toggleWindowSpacingEnabled)

  , ((mod4Mask,    xK_KP_Add), sendMessage MagnifyMore)
  , ((mod4Mask,    xK_KP_Subtract), sendMessage MagnifyLess)
  , ((mod4Mask,    xK_KP_Multiply), sendMessage Mag.Toggle >> sendMessage (JumpToLayout "Magnifier"))

  , ((mod4Mask,    xK_e), scratch "editor")
  , ((mod4Mask,    xK_Return), scratch "terminal")
  , ((mod4Mask,    xK_d), scratch "directory")
  , ((mod4Mask,    xK_KP_Enter), scratch "terminal")

  , ((mod4Mask,    xK_a), startTask)
  , ((mod4Mask,    xK_s), stopTask)
  , ((mod4Mask .|. shiftMask,    xK_a), createTask)
  , ((mod4Mask              ,    xK_equal), incTask)
  , ((mod4Mask .|. shiftMask,    xK_c), clearTasks)

  , ((mod4Mask,    xK_m), scratch "browser")
  , ((mod4Mask .|. shiftMask,    xK_Return), scratch "terminal")

  , ((mod4Mask,    xK_q), flashText def 1 "Nope")
  , ((mod4Mask .|. shiftMask,    xK_q), runCommand' "restart-wm")

  , ((mod3Mask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
  , ((mod3Mask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
  , ((mod3Mask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
  , ((mod3Mask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
  , ((mod3Mask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
  , ((mod3Mask,               xK_l     ), sendMessage Expand) -- %! Expand the master area
  -- 0xfe08 -- Pause key
  -- 0xff13 Pause
  -- 0xff14 Scroll Lock
  -- , ((0, 0xff13), liftIO $ hPutChar xmobar '\n' >> hFlush xmobar)
  , ((0, 0xff13), scratch "bluetooth")
  , ((0, 0xff14), scratch "audio")
  , ((mod4Mask, xK_n), flashText def 1 "->" >> nextWS)
  , ((mod4Mask, xK_p), flashText def 1 "<-" >> prevWS)
  , ((mod4Mask .|. shiftMask, xK_n), shiftNextWS)
  , ((mod4Mask .|. shiftMask, xK_p), shiftPrevWS)
  , ((mod4Mask .|. controlMask, xK_y), commands >>= runCommand)
  , ((mod4Mask .|. shiftMask, xK_h), toggleShowInAllWorkspaces)
  , ((mod4Mask, xK_t), withFocused toggleFloat)
  , (toggleStrutsKey, sendMessage ToggleStruts)
  ]

mouseBindings_ XConfig {XMonad.modMask = modm} = M.fromList [
    ((modm,               button1), move (Just 50) (Just 50))
  , ((modm .|. shiftMask, button1), moveResize [L,R,U,D] (Just 50) (Just 50))
  , ((modm,               button3), resize [L,R,U,D] (Just 50) (Just 50))
  ] where move a b w = XMonad.focus w  >> Flex.mouseWindow Flex.position w
          moveResize a b c w = XMonad.focus w >> mouseMoveWindow w >> afterDrag (snapMagicResize a b c w)
  -- ] where move a b w = XMonad.focus w  >> mouseMoveWindow w >> afterDrag (snapMagicMove a b w)
  --         moveResize a b c w = XMonad.focus w >> mouseMoveWindow w >> afterDrag (snapMagicResize a b c w)
          -- resize a b c w = XMonad.focus w >> mouseResizeWindow w >> afterDrag (snapMagicResize a b c w)
          resize a b c w = XMonad.focus w >> Flex.mouseWindow Flex.resize w

logHook_ xmobar = do
  fadeInactiveLogHook 0.8
  liftIO $ hFlush xmobar
  dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ pp xmobar

xmobarIcon i = "<icon=" ++ i ++ ".xpm/>"

-- https://bbs.archlinux.org/viewtopic.php?id=123299
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-NamedWindows.html#v:unName
logTitles2 :: X (Maybe String)
logTitles2 = do
  windows <- gets windowset
  let
    ordered = W.index windows
    focused = W.peek windows
  formatted <- traverse getName ordered
  -- let filtered = filter (/= "gsimplecal") formatted
  return $ Just $ concatMap (formatWindow focused) formatted
  where formatWindow f w = do
          let name = (shorten 48 . show) w
          if Just (unName w) == f
             then ppActive name
             else ppInactive name

green = "#438565"
gray = "#3b444c"
grayishBlue = "#405e78"
facebookBlueDark = "#0676e8"
facebookBlueLight = "#139ff8"
blue = facebookBlueLight
ppActive = wrap left right . xmobarColor "white" blue . wrap " " " "
  where left  = xmobarFont "1" $ xmobarColor blue "" "◀"
        right = xmobarFont "1" $ xmobarColor blue "" "▶"
ppInactive = wrap left right . xmobarColor "white" gray . wrap " " " "
  where left  = xmobarFont "1" $ xmobarColor gray "" "◀"
        right = xmobarFont "1" $ xmobarColor gray "" "▶"

xmobarFont fn = wrap start end
  where start = wrap "<fn=" ">" fn
        end   = "</fn>"

esLocale = defaultTimeLocale
  {
    wDays =
      [ ("Domingo"   , "Dom")
      , ("Lunes"     , "Lun")
      , ("Martes"    , "Mar")
      , ("Miercoles" , "Mie")
      , ("Jueves"    , "Jue")
      , ("Viernes"   , "Vie")
      , ("Sabado"    , "Sab") ]
  , months =
      [ ("Enero"      , "Ene")
      , ("Febrero"    , "Feb")
      , ("Marzo"      , "Mar")
      , ("Abril"      , "Abr")
      , ("Mayo"       , "May")
      , ("Junio"      , "Jun")
      , ("Julio"      , "Jul")
      , ("Agosto"     , "Ago")
      , ("Septiembre" , "Sep")
      , ("Octubre"    , "Oct")
      , ("Noviembre"  , "Nov")
      , ("Diciembre"  , "Dic") ]
  }

logTasks :: X (Maybe String)
logTasks = do
  (TaskList tasks) <- XS.get
  if length tasks > 0 then do
    let Task {_taskName = name, _taskTime = time} = head tasks
    return $ Just $ unwords [name, show time]
  else
    return $ Just ""

logTime :: X (Maybe String)
logTime = do
  time <- liftIO getZonedTime
  let formattedTime = formatTime esLocale "%A %e de %B %Y %r" time
  return $ Just formattedTime

logWorkspaces :: X (Maybe String)
logWorkspaces = do
  ws     <- gets windowset
  sort   <- W.getSortByIndex
  let
    sorted  = sort (W.workspaces ws)
    current = W.workspace (W.current ws)
    wsPred  = (||) <$> ((== W.tag current) . W.tag) <*> valid_
    filtered = filter wsPred sorted
    -- (Just currentIdx) = findWsIndex current filtered
    unformatted = wrap " " " " . unwords $ W.tag <$> filtered
    idx = W.tag current
    before = T.pack $ wrap " " " " idx
    after = T.pack $ ppActive idx
    formatted = T.unpack . T.replace before after $ T.pack unformatted
  -- flash $ T.unpack $ T.concat [before, " -> ", after]
  return $ Just formatted
  -- where ppActive   = xmobarColor green "" . wrap "<" ">"
  where ppActive   = xmobarFont "2" . xmobarColor green "" . wrap " " " "
        ppInactive = id

findWsIndex :: WindowSpace -> [WindowSpace] -> Maybe Int
findWsIndex ws wss = (1+) <$> findIndex ((== W.tag ws) . W.tag) wss

ppTime t = xmobarAction "gsimplecal" "1" colored
  where colored = xmobarColor "green" "" t

pp h = xmobarPP {
    ppOutput = \o -> hPutStrLn h o >> hFlush h
  -- , ppLayout = const ""
  , ppLayout = id
  , ppWsSep  = ""
  , ppSep    = ""
  , ppExtras = [logWorkspaces, logTitles2, logTasks, logTime]
  , ppOrder  = \(_:_:_:ws:ts:tasks:time:_) -> [" ", snowflake, ws, ts,"\n"]++[tasks, " "]++[ppTime time," "]
  }
  where
    snowflake = xmobarIcon "nix-snowflake"

toggleStrutsKey = (mod4Mask, xK_b)

toggleShowInAllWorkspaces :: X ()
toggleShowInAllWorkspaces = do
  s <- gets windowset
  whenJust (W.peek s) $
    \w -> if W.member w $ delete'' w s
          then killAllOtherCopies
          else windows copyToAll
    where delete'' w = W.modify Nothing (W.filter (/= w))

navigation = mkConfig arrowKeys
  where
    mkConfig k = additionalNav2DKeys k actions False
    arrowKeys  = (xK_Up, xK_Left, xK_Down, xK_Right)
    vimKeys    = (xK_k,  xK_h,    xK_j,    xK_l)
    actions    = [(mod4Mask,               windowGo  ),
                  (mod4Mask .|. shiftMask, windowSwap)]

startupHook_ = startupHook def <+> clockStartupHook <+> tasksStartupHook
handleEventHook_ = handleEventHook def <+> setTransparentHook <+> windowedFullscreenFixEventHook <+> handleTimerEvent
-- layout = ((onWorkspace "6" simpleFloat) . smartBorders . avoidStruts) (zoom ||| (fullscreenFull $ layoutHook def))

layout =
  -- onWorkspace "6" simpleFloat $
  -- spaced $
  avoidStruts $
  smartBorders $
  (tall ||| Mirror tall ||| full)
  where
    tall   = spaced $ Tall 1 (3/100) (1/2)
    full   = Full
    zoom   = magnifier (Tall 1 (3/100) (1/2))
    spaced = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True

manageHook_ = composeAll [
    -- manageHook def
    insertPosition Below Newer
  , namedScratchpadManageHook scratchpads
  -- , isFloating -->
  , role =? "pop-up" --> doFloat
  , isDialog        --> doFloat
  , isDialog        --> doF W.swapUp
  -- , (className =? "Gimp" <&&> fmap ("dialog" `isSuffixOf`) role) --> doFloat
  , (className =? "Emacs"  <&&> title =? "Helm") --> hasBorder False >> doFloat
  , className =? "Cuadrado"  --> defaultFloating
  , className =? "Pavucontrol"  --> doRectFloat center
  , className =? ".blueman-manager-wrapped"  --> doRectFloat center
  , className =? "matplotlib"  --> shift "6"
  , className =? "Gsimplecal"  --> hasBorder False >> doRectFloat (W.RationalRect (43/50) (1/50) (1/10) (1/10))
  , title =? "Hello World"  --> doFloat
  -- ,  isFullscreen --> doFullFloat,
  -- , fullscreenManageHook
  , manageDocks
  ] where
      shift = doF . liftM2 (.) W.greedyView W.shift
      role  = stringProperty "WM_WINDOW_ROLE"

center = W.RationalRect (1/4) (1/4) (2/4) (2/4)

toggleFloat w =
  windows (\s -> if M.member w (W.floating s)
            then W.sink w s
            else W.float w center s)

commands = defaultCommands

noNS = (/= "NSP") . W.tag
nonEmpty = isJust . W.stack
valid_ = (&&) <$> noNS <*> nonEmpty
valid  = return valid_
nextWS = moveTo Next (WSIs valid)
prevWS = moveTo Prev (WSIs valid)
shiftNextWS = shiftTo Next (WSIs valid)
shiftPrevWS = shiftTo Prev (WSIs valid)

flash = flashText def 1
windowedFullscreenFixEventHook :: Event -> X All
windowedFullscreenFixEventHook (ClientMessageEvent _ _ _ dpy win typ (_:dats)) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullscreen <- getAtom "_NET_WM_STATE_FULLSCREEN"
  when (typ == wmstate && fromIntegral fullscreen `elem` dats) $
    withWindowAttributes dpy win $ \attrs ->
      liftIO $ do
        resizeWindow dpy win (fromIntegral $ wa_width attrs - 1) (fromIntegral $ wa_height attrs)
        resizeWindow dpy win (fromIntegral $ wa_width attrs) (fromIntegral $ wa_height attrs)
  return $ All True
windowedFullscreenFixEventHook _ = return $ All True

-- setTransparentHook :: Event -> X All
-- setTransparentHook ConfigureEvent{ev_event_type = createNotify, ev_window = id} = do
--   setOpacity id opacity
--   return (All True) where
--     opacity = toWord32 0.8
setTransparentHook AnyEvent{ev_event_type = focusIn, ev_window = id} = do
  setOpacity id 1
  return (All True) where
-- setTransparentHook CrossingEvent{ev_event_type = leaveNotify, ev_window = id} = do
--   setOpacity id opacity
--   return (All True) where
--     opacity = toWord32 0.8
setTransparentHook _ = return (All True)

-- setOpacity :: Window -> Integer -> X ()
-- setOpacity id op = spawn $ "xprop -id " ++ show id ++ " -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY " ++ show op

-- toWord32 :: RealFrac a => a -> Integer
-- toWord32 a = floor $ fromIntegral (maxBound :: Word32) * a
