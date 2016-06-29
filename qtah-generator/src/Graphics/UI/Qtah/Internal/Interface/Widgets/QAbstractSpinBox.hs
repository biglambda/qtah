-- This file is part of Qtah.
--
-- Copyright 2016 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractSpinBox (
  aModule,
  c_QAbstractSpinBox,
  e_ButtonSymbols,
  e_CorrectionMode,
  e_StepEnabledFlag,
  bs_StepEnabled,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportClass, ExportEnum),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkBoolHasProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, intT, objT, ptrT, refT, voidT)
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractSpinBox"] $
  QtExport (ExportClass c_QAbstractSpinBox) :
  map QtExportSignal signals ++
  (map QtExport . collect)
  [ test (qtVersion >= [4, 2]) $ ExportEnum e_ButtonSymbols
  , just $ ExportEnum e_CorrectionMode
  , just $ ExportEnum e_StepEnabledFlag
  , just $ ExportBitspace bs_StepEnabled
  ]

c_QAbstractSpinBox =
  addReqIncludes [includeStd "QAbstractSpinBox"] $
  makeClass (ident "QAbstractSpinBox") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  ] $
  collect
  [ just $ mkMethod "clear" [] voidT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "hasAcceptableInput" [] boolT
  , just $ mkConstMethod "fixup" [refT $ objT c_QString] voidT
  , just $ mkMethod "interpretText" [] voidT
  , just $ mkMethod "selectAll" [] voidT
  , just $ mkMethod "stepBy" [intT] voidT
  , just $ mkMethod "stepDown" [] voidT
  , just $ mkMethod "stepUp" [] voidT
  , just $ mkConstMethod "text" [] $ objT c_QString
    -- TODO validate
  ] ++
  (mkProps . collect)
  [ test (qtVersion >= [4, 2]) $ mkBoolIsProp "accelerated"
  , just $ mkProp "alignment" $ bitspaceT bs_Alignment
  , test (qtVersion >= [4, 2]) $ mkProp "buttonSymbols" $ enumT e_ButtonSymbols
  , just $ mkProp "correctionMode" $ enumT e_CorrectionMode
  , test (qtVersion >= [4, 3]) $ mkBoolHasProp "frame"
  , just $ mkProp "keyboardTracking" boolT
  , just $ mkBoolIsProp "readOnly"
  , test (qtVersion >= [5, 3]) $ mkBoolIsProp "groupSeparatorShown"
  , just $ mkProp "specialValueText" $ objT c_QString
  , just $ mkProp "wrapping" boolT
  ]

signals =
  [ makeSignal c_QAbstractSpinBox "editingFinished" c_Listener
  ]

e_ButtonSymbols =
  addReqIncludes [includeStd "QAbstractSpinBox"] $
  makeQtEnum (ident1 "QAbstractSpinBox" "ButtonSymbols") [includeStd "QAbstractSpinBox"]
  [ (0, ["up", "down", "arrows"])
  , (1, ["plus", "minus"])
  , (2, ["no", "buttons"])
  ]

e_CorrectionMode =
  makeQtEnum (ident1 "QAbstractSpinBox" "CorrectionMode") [includeStd "QAbstractSpinBox"]
  [ (0, ["correct", "to", "previous", "value"])
  , (1, ["correct", "to", "nearest", "value"])
  ]

(e_StepEnabledFlag, bs_StepEnabled) =
  makeQtEnumBitspace (ident1 "QAbstractSpinBox" "StepEnabledFlag") "StepEnabled"
  [includeStd "QAbstractSpinBox"]
  [ (0x0, ["step", "none"])
  , (0x1, ["step", "up", "enabled"])
  , (0x2, ["step", "down", "enabled"])
  ]