-----------------------------------------------------------------------------
-- |
-- Module      :  GSMenu.Config
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  stable
-- Portability :  unportable
--
-- Default bindings and settings for easy editing
--
-----------------------------------------------------------------------------

module GSMenu.Config
    ( defaultGPConfig
    , defaultGPNav
    ) where

import qualified Data.Map as M

import Graphics.X11.Xlib

import GSMenu.Pick


defaultGPConfig :: GPConfig a
defaultGPConfig = GPConfig {
                    gp_bordercolor = "white"
                  , gp_cellheight = 50
                  , gp_cellwidth = 130
                  , gp_cellpadding = 10
                  , gp_font = "xft:Sans-8"
                  , gp_inputfont = "xft:Monospace-14"
                  , gp_keymap = defaultGPNav
                  , gp_originFractX = 1/2
                  , gp_originFractY = 1/2
                  }

defaultGPNav :: KeyMap a
defaultGPNav = M.fromList
    [ ((0,xK_Left)           ,move (-1,0))
    , ((controlMask,xK_b)    ,move (-1,0))
    , ((0,xK_Right)          ,move (1,0))
    , ((controlMask,xK_f)    ,move (1,0))
    , ((0,xK_Down)           ,move (0,1))
    , ((controlMask,xK_n)    ,move (0,1))
    , ((0,xK_Up)             ,move (0,-1))
    , ((controlMask,xK_p)    ,move (0,-1))
    , ((0,xK_BackSpace)      ,backspace)
    , ((controlMask,xK_e)    ,exclude)
    , ((controlMask,xK_i)    ,include)
    ]
