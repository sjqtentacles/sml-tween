structure Tween :> TWEEN =
struct
  datatype easing
    = Linear
    | QuadIn | QuadOut | QuadInOut
    | CubicIn | CubicOut | CubicInOut
    | QuartIn | QuartOut | QuartInOut
    | QuintIn | QuintOut | QuintInOut
    | SineIn | SineOut | SineInOut
    | ExpoIn | ExpoOut | ExpoInOut
    | CircIn | CircOut | CircInOut
    | BackIn | BackOut | BackInOut
    | ElasticIn | ElasticOut | ElasticInOut
    | BounceIn | BounceOut | BounceInOut

  type 'a frame = { at : real, easing : easing, value : 'a }

  val pi = Math.pi

  fun pow (x, n) = Math.pow (x, n)

  (* Back overshoot constants (Penner standard). *)
  val backC1 = 1.70158
  val backC2 = backC1 * 1.525
  val backC3 = backC1 + 1.0

  (* Elastic period constants. *)
  val elasticC4 = (2.0 * pi) / 3.0
  val elasticC5 = (2.0 * pi) / 4.5

  (* The textbook Penner elastic curves overshoot to ~1.373 / ~-0.373.
     We damp the oscillating term so the curve stays within the
     documented [-0.3, 1.3] envelope while preserving the exact
     endpoints (0->0, 1->1) and the characteristic spring shape. *)
  val elasticAmp = 0.75

  fun bounceOut t =
    let
      val n1 = 7.5625
      val d1 = 2.75
    in
      if t < 1.0 / d1 then n1 * t * t
      else if t < 2.0 / d1 then
        let val t = t - 1.5 / d1 in n1 * t * t + 0.75 end
      else if t < 2.5 / d1 then
        let val t = t - 2.25 / d1 in n1 * t * t + 0.9375 end
      else
        let val t = t - 2.625 / d1 in n1 * t * t + 0.984375 end
    end

  fun bounceIn t = 1.0 - bounceOut (1.0 - t)

  fun ease e t =
    case e of
      Linear => t
    | QuadIn => t * t
    | QuadOut => 1.0 - (1.0 - t) * (1.0 - t)
    | QuadInOut =>
        if t < 0.5 then 2.0 * t * t
        else 1.0 - pow (~2.0 * t + 2.0, 2.0) / 2.0
    | CubicIn => t * t * t
    | CubicOut => 1.0 - pow (1.0 - t, 3.0)
    | CubicInOut =>
        if t < 0.5 then 4.0 * t * t * t
        else 1.0 - pow (~2.0 * t + 2.0, 3.0) / 2.0
    | QuartIn => t * t * t * t
    | QuartOut => 1.0 - pow (1.0 - t, 4.0)
    | QuartInOut =>
        if t < 0.5 then 8.0 * t * t * t * t
        else 1.0 - pow (~2.0 * t + 2.0, 4.0) / 2.0
    | QuintIn => t * t * t * t * t
    | QuintOut => 1.0 - pow (1.0 - t, 5.0)
    | QuintInOut =>
        if t < 0.5 then 16.0 * t * t * t * t * t
        else 1.0 - pow (~2.0 * t + 2.0, 5.0) / 2.0
    | SineIn => 1.0 - Math.cos ((t * pi) / 2.0)
    | SineOut => Math.sin ((t * pi) / 2.0)
    | SineInOut => ~(Math.cos (pi * t) - 1.0) / 2.0
    | ExpoIn =>
        if Real.== (t, 0.0) then 0.0 else pow (2.0, 10.0 * t - 10.0)
    | ExpoOut =>
        if Real.== (t, 1.0) then 1.0 else 1.0 - pow (2.0, ~10.0 * t)
    | ExpoInOut =>
        if Real.== (t, 0.0) then 0.0
        else if Real.== (t, 1.0) then 1.0
        else if t < 0.5 then pow (2.0, 20.0 * t - 10.0) / 2.0
        else (2.0 - pow (2.0, ~20.0 * t + 10.0)) / 2.0
    | CircIn => 1.0 - Math.sqrt (1.0 - pow (t, 2.0))
    | CircOut => Math.sqrt (1.0 - pow (t - 1.0, 2.0))
    | CircInOut =>
        if t < 0.5 then (1.0 - Math.sqrt (1.0 - pow (2.0 * t, 2.0))) / 2.0
        else (Math.sqrt (1.0 - pow (~2.0 * t + 2.0, 2.0)) + 1.0) / 2.0
    | BackIn => backC3 * t * t * t - backC1 * t * t
    | BackOut =>
        1.0 + backC3 * pow (t - 1.0, 3.0) + backC1 * pow (t - 1.0, 2.0)
    | BackInOut =>
        if t < 0.5 then
          (pow (2.0 * t, 2.0) * ((backC2 + 1.0) * 2.0 * t - backC2)) / 2.0
        else
          (pow (2.0 * t - 2.0, 2.0) * ((backC2 + 1.0) * (t * 2.0 - 2.0) + backC2) + 2.0) / 2.0
    | ElasticIn =>
        if Real.== (t, 0.0) then 0.0
        else if Real.== (t, 1.0) then 1.0
        else ~(elasticAmp * pow (2.0, 10.0 * t - 10.0)) * Math.sin ((t * 10.0 - 10.75) * elasticC4)
    | ElasticOut =>
        if Real.== (t, 0.0) then 0.0
        else if Real.== (t, 1.0) then 1.0
        else elasticAmp * pow (2.0, ~10.0 * t) * Math.sin ((t * 10.0 - 0.75) * elasticC4) + 1.0
    | ElasticInOut =>
        if Real.== (t, 0.0) then 0.0
        else if Real.== (t, 1.0) then 1.0
        else if t < 0.5 then
          ~(elasticAmp * pow (2.0, 20.0 * t - 10.0) * Math.sin ((20.0 * t - 11.125) * elasticC5)) / 2.0
        else
          (elasticAmp * pow (2.0, ~20.0 * t + 10.0) * Math.sin ((20.0 * t - 11.125) * elasticC5)) / 2.0 + 1.0
    | BounceIn => bounceIn t
    | BounceOut => bounceOut t
    | BounceInOut =>
        if t < 0.5 then (1.0 - bounceOut (1.0 - 2.0 * t)) / 2.0
        else (1.0 + bounceOut (2.0 * t - 1.0)) / 2.0

  fun lerp start stop t = start + (stop - start) * t

  fun tween e a b t = lerp a b (ease e t)

  (* insertion sort by #at ascending; stable, portable across MLton/Poly/ML *)
  fun sortFrames (frames : 'a frame list) =
    let
      fun insert (x, []) = [x]
        | insert (x, y :: ys) =
            if #at x <= #at y then x :: y :: ys
            else y :: insert (x, ys)
    in
      List.foldr (fn (x, acc) => insert (x, acc)) [] frames
    end

  fun sample lerpFn frames time =
    let
      val sorted = sortFrames frames
    in
      case sorted of
        [] => raise Empty
      | (first :: _) =>
          if time <= #at first then #value first
          else
            let
              val last = List.last sorted
            in
              if time >= #at last then #value last
              else
                let
                  (* find segment [prev, cur] with prev.at <= time < cur.at *)
                  fun find (prev :: (rest as cur :: _)) =
                        if time < #at cur then (prev, cur)
                        else find rest
                    | find _ = (last, last)
                  val (prev, cur) = find sorted
                  val span = #at cur - #at prev
                  val localT =
                    if Real.== (span, 0.0) then 0.0
                    else (time - #at prev) / span
                in
                  lerpFn (#value prev) (#value cur) (ease (#easing cur) localT)
                end
            end
    end
end
