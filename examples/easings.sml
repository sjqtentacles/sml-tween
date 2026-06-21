(* sml-tween demo: plots a grid of easing curves (Tween.ease sampled over
   [0,1]) into separate cells and writes assets/easings.png. Overshooting
   easings (Back/Elastic/Bounce) are visible against the 0 and 1 guide lines. *)

fun rgba (r, g, b, a) : Image.rgba8 =
  { r = Word8.fromInt r, g = Word8.fromInt g
  , b = Word8.fromInt b, a = Word8.fromInt a }

val cols = 4
val rows = 3
val cellW = 128
val cellH = 168
val width = cols * cellW
val height = rows * cellH

val padX = 18
val padTop = 16
val plotW = cellW - 2 * padX
val plotH = 132

(* Value window: a little headroom so overshoot curves stay on-cell. *)
val vmin = ~0.38
val vmax = 1.38

val curves =
  [ ("linear",     Tween.Linear)
  , ("quad io",    Tween.QuadInOut)
  , ("cubic in",   Tween.CubicIn)
  , ("cubic out",  Tween.CubicOut)
  , ("sine io",    Tween.SineInOut)
  , ("expo out",   Tween.ExpoOut)
  , ("circ io",    Tween.CircInOut)
  , ("back io",    Tween.BackInOut)
  , ("elastic o",  Tween.ElasticOut)
  , ("bounce o",   Tween.BounceOut)
  , ("quart io",   Tween.QuartInOut)
  , ("quint in",   Tween.QuintIn) ]

fun toI v = let val n = Real.round (v * 255.0)
            in if n < 0 then 0 else if n > 255 then 255 else n end
fun hueColor h =
  let val { r, g, b } = Color.hsvToRgb { h = h, s = 0.62, v = 1.0 }
  in rgba (toI r, toI g, toI b, 255) end

val box     = rgba (40, 46, 58, 255)
val guide   = rgba (58, 66, 82, 255)

fun valToY (y0, v) =
  let val norm = (v - vmin) / (vmax - vmin)
  in y0 + Real.round ((1.0 - norm) * real plotH) end

fun drawCell (img, k, easing) =
  let
    val i = k mod cols
    val j = k div cols
    val x0 = i * cellW + padX
    val y0 = j * cellH + padTop
    val color = hueColor (real k * 30.0)
    (* cell frame + 0/1 guide lines *)
    val c = Raster.rect img { x = x0, y = y0, w = plotW, h = plotH } box
    val y1line = valToY (y0, 1.0)
    val y0line = valToY (y0, 0.0)
    val c = Raster.line c { x0 = x0, y0 = y1line, x1 = x0 + plotW, y1 = y1line } guide
    val c = Raster.line c { x0 = x0, y0 = y0line, x1 = x0 + plotW, y1 = y0line } guide
    (* sample the easing curve *)
    val n = 96
    fun pt s =
      let val t = real s / real n
      in (x0 + Real.round (t * real plotW), valToY (y0, Tween.ease easing t)) end
    val pts = List.tabulate (n + 1, pt)
    val c = Raster.polyline c pts color
    val c = Raster.polyline c (map (fn (x, y) => (x, y + 1)) pts) color
  in
    c
  end

val img =
  let
    val base = Raster.blank (width, height) (rgba (20, 23, 29, 255))
    fun loop (k, c) =
      if k >= length curves then c
      else loop (k + 1, drawCell (c, k, #2 (List.nth (curves, k))))
  in
    loop (0, base)
  end

val () =
  let
    val os = BinIO.openOut "assets/easings.png"
  in
    BinIO.output (os, Image.encodePng img);
    BinIO.closeOut os;
    print "wrote assets/easings.png\n"
  end
