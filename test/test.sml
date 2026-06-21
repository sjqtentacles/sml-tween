structure Tests =
struct
  open Tween

  (* Harness lacks real checks; epsilon comparison helper. *)
  fun checkReal name (expected, actual) =
    if Real.abs (expected - actual) < 1E~6
    then Harness.check name true
    else Harness.check (name ^ " (" ^ Real.toString expected ^ " <> " ^ Real.toString actual ^ ")") false

  fun checkRealEps eps name (expected, actual) =
    if Real.abs (expected - actual) < eps
    then Harness.check name true
    else Harness.check (name ^ " (" ^ Real.toString expected ^ " <> " ^ Real.toString actual ^ ")") false

  val allEasings =
    [ ("Linear", Linear)
    , ("QuadIn", QuadIn), ("QuadOut", QuadOut), ("QuadInOut", QuadInOut)
    , ("CubicIn", CubicIn), ("CubicOut", CubicOut), ("CubicInOut", CubicInOut)
    , ("QuartIn", QuartIn), ("QuartOut", QuartOut), ("QuartInOut", QuartInOut)
    , ("QuintIn", QuintIn), ("QuintOut", QuintOut), ("QuintInOut", QuintInOut)
    , ("SineIn", SineIn), ("SineOut", SineOut), ("SineInOut", SineInOut)
    , ("ExpoIn", ExpoIn), ("ExpoOut", ExpoOut), ("ExpoInOut", ExpoInOut)
    , ("CircIn", CircIn), ("CircOut", CircOut), ("CircInOut", CircInOut)
    , ("BackIn", BackIn), ("BackOut", BackOut), ("BackInOut", BackInOut)
    , ("ElasticIn", ElasticIn), ("ElasticOut", ElasticOut), ("ElasticInOut", ElasticInOut)
    , ("BounceIn", BounceIn), ("BounceOut", BounceOut), ("BounceInOut", BounceInOut)
    ]

  (* sampled t-values 0.0, 0.1, ..., 1.0 *)
  val ts = List.tabulate (11, fn i => real i / 10.0)

  fun nonDecreasing f =
    let
      val vals = List.map f ts
      fun go (a :: (rest as b :: _)) = a <= b + 1E~9 andalso go rest
        | go _ = true
    in go vals end

  fun section1_linear () =
    let in
      Harness.section "Linear easing";
      checkReal "ease Linear 0.0 = 0.0" (0.0, ease Linear 0.0);
      checkReal "ease Linear 1.0 = 1.0" (1.0, ease Linear 1.0);
      checkReal "ease Linear 0.5 = 0.5" (0.5, ease Linear 0.5)
    end

  fun section2_boundaries () =
    let in
      Harness.section "Boundary contract for all 30 easings";
      List.app (fn (nm, e) =>
        (checkRealEps 1E~6 ("ease " ^ nm ^ " 0.0 ~= 0.0") (0.0, ease e 0.0);
         checkRealEps 1E~6 ("ease " ^ nm ^ " 1.0 ~= 1.0") (1.0, ease e 1.0)))
        allEasings
    end

  fun section3_monotonicity () =
    let in
      Harness.section "Monotonicity";
      Harness.check "QuadIn non-decreasing" (nonDecreasing (ease QuadIn));
      Harness.check "QuadOut non-decreasing" (nonDecreasing (ease QuadOut));
      Harness.check "CubicIn non-decreasing" (nonDecreasing (ease CubicIn));
      Harness.check "CubicOut non-decreasing" (nonDecreasing (ease CubicOut))
    end

  fun section4_tweenlerp () =
    let in
      Harness.section "tween and lerp";
      checkReal "lerp 0.0 10.0 0.5 = 5.0" (5.0, lerp 0.0 10.0 0.5);
      checkReal "lerp 2.0 4.0 0.0 = 2.0" (2.0, lerp 2.0 4.0 0.0);
      checkReal "lerp 2.0 4.0 1.0 = 4.0" (4.0, lerp 2.0 4.0 1.0);
      checkReal "tween Linear 0.0 10.0 0.5 = 5.0" (5.0, tween Linear 0.0 10.0 0.5);
      checkReal "tween QuadIn 0.0 10.0 0.5 = 2.5" (2.5, tween QuadIn 0.0 10.0 0.5)
    end

  fun section5_knownvalues () =
    let in
      Harness.section "Known easing values";
      checkRealEps 1E~3 "ease QuadIn 0.5 ~= 0.25" (0.25, ease QuadIn 0.5);
      checkRealEps 1E~3 "ease QuadOut 0.5 ~= 0.75" (0.75, ease QuadOut 0.5);
      checkRealEps 1E~3 "ease SineIn 0.5 ~= 0.2929" (0.2929, ease SineIn 0.5)
    end

  fun section6_sample () =
    let
      val realLerp = fn a => fn b => fn t => a + (b - a) * t
      val frames =
        [ { at = 0.0, easing = Linear, value = 0.0 }
        , { at = 1.0, easing = Linear, value = 10.0 }
        , { at = 2.0, easing = Linear, value = 20.0 }
        ]
      fun s t = sample realLerp frames t
    in
      Harness.section "sample over keyframe timeline";
      checkReal "sample t=0.0 = 0.0" (0.0, s 0.0);
      checkReal "sample t=0.5 ~= 5.0" (5.0, s 0.5);
      checkReal "sample t=1.0 = 10.0" (10.0, s 1.0);
      checkReal "sample t=2.0 = 20.0" (20.0, s 2.0);
      checkReal "sample t<0 = first value (0.0)" (0.0, s (~1.0));
      checkReal "sample t>2 = last value (20.0)" (20.0, s 3.0)
    end

  fun section7_bounds () =
    let
      (* finer sample set including overshoot regions *)
      val fine = List.tabulate (101, fn i => real i / 100.0)
      fun within e =
        List.all (fn t => let val v = ease e t in v >= ~0.3 andalso v <= 1.3 end) fine
    in
      Harness.section "Bounce/elastic stay within [-0.3, 1.3]";
      Harness.check "ElasticIn in [-0.3,1.3]" (within ElasticIn);
      Harness.check "ElasticOut in [-0.3,1.3]" (within ElasticOut);
      Harness.check "ElasticInOut in [-0.3,1.3]" (within ElasticInOut);
      Harness.check "BounceIn in [-0.3,1.3]" (within BounceIn);
      Harness.check "BounceOut in [-0.3,1.3]" (within BounceOut);
      Harness.check "BounceInOut in [-0.3,1.3]" (within BounceInOut)
    end

  fun run () =
    ( Harness.reset ()
    ; section1_linear ()
    ; section2_boundaries ()
    ; section3_monotonicity ()
    ; section4_tweenlerp ()
    ; section5_knownvalues ()
    ; section6_sample ()
    ; section7_bounds ()
    ; Harness.run ()
    )
end
