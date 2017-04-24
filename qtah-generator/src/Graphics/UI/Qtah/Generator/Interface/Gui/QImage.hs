-- This file is part of Qtah.
--
-- Copyright 2016-2017 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Generator.Interface.Gui.QImage (
  aModule,
  c_QImage,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_FillRule)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QImage"]
  [ QtExport $ ExportClass c_QImage ]

c_QImage =
  addReqIncludes [includeStd "QImage"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QImage") Nothing [c_QPaintDevice] $
  collect
  [ enumT e_InvertMode
  , enumT e_Format
  , just $ mkCtor "new" []
  , just $ mkCtor "newWithSize"        [objT c_QSize                    , enumT e_Format]
  , just $ mkCtor "newWithWidthHeight" [                      intT, intT, enumT e_Format]
  , just $ mkCtor "newWithData"        [        ptrT ucharT , intT, intT, enumT e_Format, objT c_QImageCleanupFunction, ptrT voidT]
  , just $ mkCtor "newWithConstData"   [constT (ptrT ucharT), intT, intT, enumT e_Format, objT c_QImageCleanupFunction, ptrT voidT]
  , just $ mkCtor "newWithData"        [        ptrT ucharT , intT, intT, enumT e_Format, objT c_QImageCleanupFunction, ptrT voidT]
  , just $ mkCtor "newWithConstData"   [constT (ptrT ucharT), intT, intT, enumT e_Format, objT c_QImageCleanupFunction, ptrT voidT]
  -- TODO XMP
  -- explicit QImage(const QString &fileName, const char *format = Q_NULLPTR);
  -- QImage(const QImage &);
  -- inline QImage(QImage &&other) Q_DECL_NOEXCEPT : QPaintDevice(), d(Q_NULLPTR) { qSwap(d, other.d); }
  -- TODO operator=
  , just $ mkConstMethod "isNull"      []              boolT
  , just $ mkConstMethod "devType"     []              intT
  , just $ mkConstMethod' "operator==" [objT c_QImage] boolT
  , just $ mkConstMethod' "operator!=" [objT c_QImage] boolT
  -- TODO  operator QVariant() const;
  , just $ mkMethod "detach" [] voidT
  , just $ mkMethod "isDetached" [] boolT
  , just $ mkConstMethod' "copy" "copyWithRect" [objT c_QRect] (objT c_QImage)
  , just $ mkConstMethod' "copy" "copyWithRaw"  [intT, intT, intT, intT] (objT c_QImage)
  , just $ mkConstMethod "format" [] (enumT e_Format)
  -- TODO QImage convertToFormat(Format f, Qt::ImageConversionFlags flags = Qt::AutoColor) const Q_REQUIRED_RESULT;
  -- TODO QImage convertToFormat(Format f, const QVector<QRgb> &colorTable, Qt::ImageConversionFlags flags = Qt::AutoColor) const Q_REQUIRED_RESULT;
  , just $ mkConstMethod "width"  [] intT
  , just $ mkConstMethod "height" [] intT
  , just $ mkConstMethod "size"   [] (objT c_QSize)
  , just $ mkConstMethod "rect"   [] (objT c_QRect)
  , just $ mkConstMethod "depth"  [] intT
  , just $ mkConstMethod "colorCount"    [] intT
  , just $ mkConstMethod "bitPlaneCount" [] intT

  , just $ mkConstMethod "color"        [intT]                (objT c_QRgb)
  , just $ mkMethod      "setColor"     [intT i, objT c_QRgb] voidT
  , just $ mkMethod      "setColorCount"[intT]                voidT
  , just $ mkConstMethod "allGray"      []                    boolT
  , just $ mkConstMethod "isGrayscale"  []                    boolT
  , just $ mkConstMethod "bits"         []                    (ptrT ucharT)
  , just $ mkConstMethod "constBits"    []                    (constT $ ptrT ucharT)

  , just $ mkConstMethod "byteCount"    []

  , just $ mkMethod "scanLine" [intT] (ptrT ucharT)
  , just $ mkConstMethod "constScanLine" [intT] (constT (ptrT ucharT)
  , just $ mkConstMethod "bytesPerLine" [] intT

  , just $ mkConstMethod "valid" [intT, intT]             boolT
  , just $ mkConstMethod "valid" [constT $ objT c_QPoint] boolT

  , just $ mkConstMethod  "pixelIndex"                       [intT, intT]           intT
  , just $ mkConstMethod' "pixelIndex" "pixelIndexWithPoint" [constT objT c_QPoint] intT
  , just $ mkConstMethod  "pixel"                            [intT, intT]           (objT c_QRgb)
  , just $ mkConstMethod' "pixel"      "pixelWithPoint"      [constT objT c_QPoint] (objT c_QRgb)

  , just $ mkMethod  "setPixel"                              [intT, intT, uintT            ] voidT
  , just $ mkMethod' "setPixel"        "setPixelWithPoint"   [constT $ objT c_QPoint, uintT] voidT

  , just $ mkConstMethod "pixelColor" [intT, intT            ] (objT c_QColor)
  , just $ mkConstMethod "pixelColor" [constT $ objT c_QPoint] (objT c_QColor)

  , just $ mkMethod  "setPixelColor"                          [intT, intT, constT $ objT c_QColor]             voidT
  , just $ mkMethod' "setPixelColor" "setPixelColorWithPoint" [constT $ objT c_QPoint, constT $ objT c_QColor] voidT

  -- TODO  QVector<QRgb> colorTable() const;
  -- #if QT_VERSION >= QT_VERSION_CHECK(6,0,0)
  --     void setColorTable(const QVector<QRgb> &colors);
  -- #else
  --     void setColorTable(const QVector<QRgb> colors);
  -- #endif

  , just $ mkConstMethod  "devicePixelRatio"    []      qreal
  , just $ mkMethod       "setDevicePixelRatio" [qreal] voidT

  , just $ mkMethod  "fill"                 [uintT]                   voidT
  , just $ mkMethod' "fill" "fillWithColor" [constT $ objT c_QColor ] voidT
  -- TODO , just $ mkMethod       "fill"                [Qt::GlobalColor color  ] voidT

  , just $ mkConstMethod "hasAlphaChannel" []                       boolT
  , just $ mkMethod      "setAlphaChannel" [constT $ objT c_QImage] voidT
  , just $ mkConstMethod "alphaChannel"    [] (objT QImage)
  -- TODO , just $ mkConstMethod "createAlphaMask" [Qt::ImageConversionFlags flags = Qt::AutoColor] (objT QImage)
  -- #ifndef QT_NO_IMAGE_HEURISTIC_MASK
  --  QImage createHeuristicMask(bool clipTight = true) const;
  -- #endif
  {- QImage createMaskFromColor(QRgb color, Qt::MaskMode mode = Qt::MaskInColor) const;

    inline QImage scaled(int w, int h, Qt::AspectRatioMode aspectMode = Qt::IgnoreAspectRatio,
                        Qt::TransformationMode mode = Qt::FastTransformation) const
        { return scaled(QSize(w, h), aspectMode, mode); }
    QImage scaled(const QSize &s, Qt::AspectRatioMode aspectMode = Qt::IgnoreAspectRatio,
                 Qt::TransformationMode mode = Qt::FastTransformation) const;
    QImage scaledToWidth(int w, Qt::TransformationMode mode = Qt::FastTransformation) const;
    QImage scaledToHeight(int h, Qt::TransformationMode mode = Qt::FastTransformation) const;
    QImage transformed(const QMatrix &matrix, Qt::TransformationMode mode = Qt::FastTransformation) const;
    static QMatrix trueMatrix(const QMatrix &, int w, int h);
    QImage transformed(const QTransform &matrix, Qt::TransformationMode mode = Qt::FastTransformation) const;
    static QTransform trueMatrix(const QTransform &, int w, int h);
#if defined(Q_COMPILER_REF_QUALIFIERS) && !defined(QT_COMPILING_QIMAGE_COMPAT_CPP)
    QImage mirrored(bool horizontally = false, bool vertically = true) const &
        { return mirrored_helper(horizontally, vertically); }
    QImage &&mirrored(bool horizontally = false, bool vertically = true) &&
        { mirrored_inplace(horizontally, vertically); return qMove(*this); }
    QImage rgbSwapped() const &
        { return rgbSwapped_helper(); }
    QImage &&rgbSwapped() &&
        { rgbSwapped_inplace(); return qMove(*this); }
#else
    QImage mirrored(bool horizontally = false, bool vertically = true) const;
    QImage rgbSwapped() const;
#endif
 -}
  , just $ mkMethod "invertPixels" [enumT e_InvertMode] voidT

  , just $ mkMethod' "load"         "loadWithIODevice"          [ptrT $ objT c_QIODevice   , constT (ptrT charT)] boolT
  , just $ mkMethod  "load"                                     [constT $ objT c_QString   , constT (ptrT charT)] boolT
  , just $ mkMethod  "loadFromData"                             [constT (ptrT ucharT), intT, constT (ptrT charT)] boolT
  , just $ mkMethod' "loadFromData" "loadFromDataWithByteArray" [constT (objT c_QByteArray), constT (ptrT charT)] boolT


  , just $ mkConstMethod  "save"                    [constT $ objT c_QString,   constT $ ptrT charT, intT] boolT
  , just $ mkConstMethod' "save" "saveWithIODevice" [constT $ objT c_QIODevice, constT $ ptrT charT, intT] boolT

  , just $ mkStaticMethod  "fromData"                         [constT (ptrT ucharT), intT, constT (ptrT charT)] (objT c_QImage)
  , just $ mkStaticMethod' "fromData" "fromDataWithByteArray" [constT (objT c_QByteArray), constT (ptrT charT)] (objT c_QImage)

  , just $ mkConstMethod  "serialNumber" [] intT

  , just $ mkConstMethod "cacheKey" [] qint64

  , just $ mkConstMethod "paintEngine" [] (objT c_QPaintEngine)

  , just $ mkConstMethod "dotsPerMeterX"    []                       intT
  , just $ mkConstMethod "dotsPerMeterY"    []                       intT
  , just $ mkMethod      "setDotsPerMeterX" [intT]                   voidT
  , just $ mkMethod      "setDotsPerMeterY" [intT]                   voidT
  , just $ mkConstMethod "offset"           []                       (objT c_QPoint)
  , just $ mkMethod      "setOffset"        [constT $ objT c_QPoint] voidT

  , just $ mkConstMethod "textKeys" []                                                 (objT c_QStringList)
  , just $ mkConstMethod "text"     [constT $ objT c_QString]                          (objT c_QString)
  , just $ mkMethod      "setText"  [constT $ objT c_QString, constT $ objT c_QString] voidT

  , just $ mkConstMethod "pixelFormat" [] (objT c_QPixelFormat)
  -- , just $ mkStaticMethod  QPixelFormat toPixelFormat(QImage::Format format) Q_DECL_NOTHROW;
  -- , just $ mkStaticMethod  QImage::Format toImageFormat(QPixelFormat format) Q_DECL_NOTHROW;

   -- Platform spesific conversion functions
    -- #if defined(Q_OS_DARWIN) || defined(Q_QDOC)
    --CGImageRef toCGImage() const Q_DECL_CF_RETURNS_RETAINED;
    -- #endif
{-
protected:
    virtual int metric(PaintDeviceMetric metric) const Q_DECL_OVERRIDE;
    QImage mirrored_helper(bool horizontal, bool vertical) const;
    QImage rgbSwapped_helper() const;
    void mirrored_inplace(bool horizontal, bool vertical);
    void rgbSwapped_inplace();
    QImage convertToFormat_helper(Format format, Qt::ImageConversionFlags flags) const;
    bool convertToFormat_inplace(Format format, Qt::ImageConversionFlags flags);
    QImage smoothScaled(int w, int h) const;

private:
    friend class QWSOnScreenSurface;
    QImageData *d;

    friend class QRasterPlatformPixmap;
    friend class QBlittablePlatformPixmap;
    friend class QPixmapCacheEntry;

public:
    typedef QImageData * DataPtr;
    inline DataPtr &data_ptr() { return d; }
-}


e_InvertMode =
  makeQtEnum (ident1 "QImage" "InvertMode")
  [includeStd "QImage"]
  [(0, ["InvertRgb"])
  ,(1, ["InvertRgba"])
  ]

e_Format =
  makeQtEnum (ident1 "QImage" "Format")
  [includeStd "QImage"]
  [ (0   , ["Format","Invalid"])
  , (1   , ["Format","Mono"])
  , (2   , ["Format","MonoLSB"])
  , (3   , ["Format","Indexed8"])
  , (4   , ["Format","RGB32"])
  , (5   , ["Format","ARGB32"])
  , (6   , ["Format","ARGB32","Premultiplied"])
  , (7   , ["Format","RGB16"])
  , (8   , ["Format","ARGB8565","Premultiplied"])
  , (9   , ["Format","RGB666"])
  , (10  , ["Format","ARGB6666","Premultiplied"])
  , (11  , ["Format","RGB555"])
  , (12  , ["Format","ARGB8555","Premultiplied"])
  , (13  , ["Format","RGB888"])
  , (14  , ["Format","RGB444"])
  , (15  , ["Format","ARGB4444","Premultiplied"])
  , (16  , ["Format","RGBX8888"])
  , (17  , ["Format","RGBA8888"])
  , (18  , ["Format","RGBA8888","Premultiplied"])
  , (19  , ["Format","BGR30"])
  , (20  , ["Format","A2BGR30","Premultiplied"])
  , (21  , ["Format","RGB30"])
  , (22  , ["Format","A2RGB30","Premultiplied"])
  , (23  , ["Format","Alpha8"])
  , (24  , ["Format","Grayscale8"])
  --  Format_Grayscale16,
  , (25 , ["NImageFormats"])
  ]
