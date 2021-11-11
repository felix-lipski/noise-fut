import "../lib/github.com/diku-dk/cpprandom/random"

module dist = uniform_real_distribution f64 minstd_rand

let noise_1d rng (n: i64): (minstd_rand.rng, [n]f64) =
  let xs = (iota n)
  let f r _ = (dist.rand (0.0,0.5) r)
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

let smooth_noise_1d rng (lenX: i64) (period: i64) (octave: i64): (minstd_rand.rng, [lenX]f64) = 
  let strech = 2 ** octave |> (period /)
  let strechF: f64 = f64.i64 strech
  let (rng_ret, noise) = (noise_1d rng (lenX/strech))
  let lerp (a,b) i = f64.i64 i 
                       |> flip (/) strechF 
                       |> f64.lerp a b
  let ret = (rotate 1 noise
    |> zip2 noise
    |> map (\ab -> map (lerp ab) (iota strech))
    |> flatten_to lenX):> [lenX]f64 
  in (rng_ret, ret)
  
let smooth_noise_2d rng (lenX: i64) (period: i64) (octave: i64): (minstd_rand.rng, [lenX][lenX]f64) = 
  let strech = 2 ** octave |> (period /)
  let strechF: f64 = f64.i64 strech
  let (rng_ret, noise) = (noise_2d rng (lenX/strech) (lenX/strech))
  let lerp (a,b) i = f64.i64 i 
                       |> flip (/) strechF 
                       |> f64.lerp a b
  let foo (list_2d: []f64): [lenX]f64 = (rotate 1 list_2d
    |> zip2 list_2d
    |> map (\ab -> map (lerp ab) (iota strech))
    |> flatten_to lenX) :> [lenX]f64
  let ret = (map foo noise
    |> transpose
    |> map foo) :> [lenX][lenX]f64 
  in (rng_ret, ret)

let collapse_octaves [octaves] (os: [octaves]f64): f64 = 
  zip os (iota octaves) 
  |> map (\(o,i) -> o / 2**(f64.i64 i)) |> reduce (+) 0.0f64

let perlin_octaves_1d rng (lenX: i64) (period: i64) (octaves: i64): (minstd_rand.rng, [lenX][octaves]f64) =
  let (rngs, ys) = minstd_rand.split_rng octaves rng
    |> zip (iota octaves)
    |> map (\(o,r) -> smooth_noise_1d r lenX period o) 
    |> unzip
  let rng = minstd_rand.join_rng rngs
  in (rng, transpose ys)
  
let perlin_octaves_2d rng (lenX: i64) (period: i64) (octaves: i64): (minstd_rand.rng, [lenX][lenX][octaves]f64) =
  let (rngs, ys) = minstd_rand.split_rng octaves rng
    |> zip (iota octaves)
    |> map (\(o,r) -> smooth_noise_2d r lenX period o) 
    |> unzip
  let rng = minstd_rand.join_rng rngs
  in (rng, transpose ys |> map transpose)

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

let main (lenX: i64): [lenX][lenX][3]u8 =
  let rng = minstd_rand.rng_from_seed [2]
  let noise1d = map collapse_octaves (perlin_octaves_1d rng lenX 128 1).1
  -- let noise2d = ((perlin_octaves_2d rng lenX 128 2).1)[0]
  let noise2d = map (map collapse_octaves) (perlin_octaves_2d rng lenX 32 5).1
  -- let noise2d = map (map (\_ -> 0.5)) (perlin_octaves_2d rng lenX 32 5).1
  -- in graph2d lenX noise
  -- in (smooth_noise_2d rng lenX 128 1).1
  in noise2d
    |> f64_lum_to_u8_rgb_arr

