import "../lib/github.com/diku-dk/cpprandom/random"

module dist = uniform_real_distribution f64 minstd_rand

let noise_1d rng (n: i64): (minstd_rand.rng, [n]f64) =
  let xs = (iota n)
  let f r _ = (dist.rand (-1.0,1.0) r)
  let rngs = minstd_rand.split_rng n rng
  let (rngs, ys) = unzip (map2 f rngs xs)
  let rng = minstd_rand.join_rng rngs
  in (rng, ys)

let noise_2d rng (n: i64) (m: i64): (minstd_rand.rng, [n][m]f64) =
  let xs = (iota n)
  let f r _ = noise_1d r m
  let rngs = minstd_rand.split_rng n rng
  let (rngs, ys) = unzip (map2 f rngs xs)
  let rng = minstd_rand.join_rng rngs
  in (rng, ys)

let smooth_noise_1d rng (lenX: i64) (period: i64) (octave: i64): [lenX]f64 = 
  let strech = 2 ** octave |> (period /)
  let strechF: f64 = f64.i64 strech
  let noise = (noise_1d rng (lenX/strech)).1
  in (rotate 1 noise
    |> zip2 noise
    |> map (\(a,b) -> map (\x -> f64.i64 x |> flip (/) strechF |> f64.lerp a b) (iota strech))
    |> flatten_to lenX):> [lenX]f64 
  
let perlin_1d rng (lenX: i64) (period: i64) (octaves: i64): [lenX]f64 =
  let empty_arr = map (\_ -> 0.0f64) (iota lenX)
  in foldl (\acc -> \octave -> 
    smooth_noise_1d rng lenX period octave 
    |> map (\x -> x / (2 ** (octave) |> f64.i64))
    |> zip2 acc 
    |> map (\(a,b) -> a + b)
  ) (empty_arr) (iota octaves)




-- MAIN

let graph2d [lenX] (height: i64) (xs: [lenX]f64): [height][lenX]f64 =
  let h_float = f64.i64 height
  let half_h = h_float / 2.0
  let amp = h_float / 16.0
  in map (\v -> map (\y -> f64.i64 y |> (\y2 -> 
    if (y2 > (half_h - v * amp)) then 0.1 else 0.9)) (iota height)
  ) xs |> transpose

let f64_lum_to_u8_rgb_arr [n][m] (arr: [n][m]f64): [n][m][3]u8 =
  map (\y -> map (\x -> let i = x |> (* 256.0) |> u8.f64 |> (\z -> z - 1) in [i, i, i]) y) arr

let main (screenX: i64) (screenY: i64): [screenY][screenX][3]u8 =
  let rng = minstd_rand.rng_from_seed [2]
  let noise = perlin_1d rng screenX 128 8
  in graph2d screenY noise
    |> f64_lum_to_u8_rgb_arr

