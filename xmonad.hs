import XMonad 
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Layout.NoBorders
import XMonad.Layout.HintedGrid
import XMonad.Layout.Roledex
import XMonad.Layout.Reflect
import XMonad.Layout.Circle
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.IM
import Data.Ratio ((%))

myLayoutHook = avoidStruts(smartBorders(tiled)) ||| noBorders(Full) ||| noBorders(Mirror tiled) ||| avoidStruts(smartBorders( Grid False ||| Circle  ||| Roledex ) )
	where 
		tiled = Tall nmaster delta ratio
		nmaster = 1
		ratio = 1/2
		delta = 3/100

myManageHook :: [ManageHook]
myManageHook =  
    [ className =? "vmware" --> doFloat
	, resource  =? "Do"     --> doIgnore 
	]

main = xmonad gnomeConfig
    { layoutHook = myLayoutHook,
      startupHook = setWMName "LG3D",
      focusFollowsMouse = False,
	  manageHook = manageHook gnomeConfig <+> composeAll myManageHook }
