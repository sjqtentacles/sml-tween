signature TWEEN =
sig
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

  (* ease e t : maps [0,1] -> roughly [0,1]; back/elastic/bounce overshoot *)
  val ease : easing -> real -> real

  (* lerp start stop t = start + (stop - start) * t *)
  val lerp : real -> real -> real -> real

  (* tween e a b t = lerp a b (ease e t) *)
  val tween : easing -> real -> real -> real -> real

  type 'a frame = { at : real, easing : easing, value : 'a }

  (* sample lerpFn frames time : interpolate over a keyframe timeline *)
  val sample : ('a -> 'a -> real -> 'a) -> 'a frame list -> real -> 'a
end
