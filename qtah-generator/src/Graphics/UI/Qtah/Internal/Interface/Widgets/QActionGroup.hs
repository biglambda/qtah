{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QActionGroup (
  qtModule,
  c_QActionGroup,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerPtrQAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QActionGroup $ map QtExportSignal signals

this = c_QActionGroup
#include "../Mk.hs.inc"

c_QActionGroup =
  addReqIncludes [includeStd "QActionGroup"] $
  makeClass (ident "QActionGroup") Nothing
  [ c_QObject ]
  [ _mkCtor "new" [TPtr $ TObj c_QObject]
  ]
  [ -- TODO actions
    _mkMethod' "addAction" "addAction" [TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
  , _mkMethod' "addAction" "addNewAction" [TObj c_QString] $ TPtr $ TObj c_QAction
    -- TODO addNewActionWithIcon
  , _mkConstMethod "checkedAction" [] $ TPtr $ TObj c_QAction
  , _mkConstMethod "isEnabled" [] TBool
  , _mkConstMethod "isExclusive" [] TBool
  , _mkConstMethod "isVisible" [] TBool
  , _mkMethod "removeAction" [TPtr $ TObj c_QAction] TVoid
  , _mkMethod "setDisabled" [TBool] TVoid
  , _mkMethod "setEnabled" [TBool] TVoid
  , _mkMethod "setExclusive" [TBool] TVoid
  , _mkMethod "setVisible" [TBool] TVoid
  ]

signals =
  [ _mkSignal "hovered" c_ListenerPtrQAction
  , _mkSignal "triggered" c_ListenerPtrQAction
  ]
