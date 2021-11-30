import "../lib/github.com/diku-dk/cpprandom/random"

module dist = uniform_real_distribution f64 minstd_rand

let noise_1d rng (amp: f64) (n: i64): (minstd_rand.rng, [n]f64) =
  let xs = (iota n)
  let f r _ = (dist.rand (0,amp/2) r)
  let rngs = minstd_rand.split_rng n rng
  let (rngs, ys) = unzip (map2 f rngs xs)
  let rng = minstd_rand.join_rng rngs
  in (rng, ys)

let noise_2d rng (amp: f64) (n: i64) (m: i64): (minstd_rand.rng, [n][m]f64) =
  let xs = (iota n)
  let f r _ = noise_1d r amp m
  let rngs = minstd_rand.split_rng n rng
  let (rngs, ys) = unzip (map2 f rngs xs)
  let rng = minstd_rand.join_rng rngs
  in (rng, ys)

let lerp_list lenX strech lis2d = 
  let lerp_strech strechF (a,b) i = f64.i64 i 
                     |> flip (/) strechF 
                     |> f64.lerp a b
  let strechF: f64 = f64.i64 strech
  in (rotate 1 lis2d 
    |> zip2 lis2d 
    |> map (\ab -> map (lerp_strech strechF ab) (iota strech)) 
  |> flatten_to lenX):> [lenX]f64

let smooth_noise_1d rng (lenX: i64) (period: i64) (octave: i64): (minstd_rand.rng, [lenX]f64) = 
  let strech = 2 ** octave |> (period /)
  let (rng_ret, noise) = (noise_1d rng (1.0 / 2**(f64.i64 octave)) (lenX/strech))
  let foo = lerp_list lenX strech
  let ret = foo noise
  in (rng_ret, ret)
  
let smooth_noise_2d rng (lenX: i64) (period: i64) (octave: i64): (minstd_rand.rng, [lenX][lenX]f64) = 
  let strech = 2 ** octave |> (period /)
  let (rng_ret, noise) = (noise_2d rng (1.0 / 2**(f64.i64 octave)) (lenX/strech) (lenX/strech))
  let foo = lerp_list lenX strech
  let ret = (map foo noise
    |> transpose
    |> map foo) :> [lenX][lenX]f64 
  in (rng_ret, ret)

let perlin_1d rng (lenX: i64) (period: i64) (octaves: i64): (minstd_rand.rng, [lenX]f64) =
  let (rngs, ys) = minstd_rand.split_rng octaves rng
    |> zip (iota octaves)
    |> map (\(o,r) -> smooth_noise_1d r lenX period o) 
    |> unzip
  let rng = minstd_rand.join_rng rngs
  let ys = ys 
    |> reduce (map2 (+)) (replicate lenX 0.0f64)
  in (rng, ys)
  
let perlin_2d rng (lenX: i64) (period: i64) (octaves: i64): (minstd_rand.rng, [lenX][lenX]f64) =
  let (rngs, ys) = minstd_rand.split_rng octaves rng
    |> zip (iota octaves)
    |> map (\(o,r) -> smooth_noise_2d r lenX period o) 
    |> unzip
  let rng = minstd_rand.join_rng rngs
  let ys = ys 
    |> reduce (map2 (map2 (+))) (replicate lenX (replicate lenX 0.0f64))
  in (rng, ys)

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
  let noise1d = (perlin_1d rng lenX 32 6).1
  let noise2d = (perlin_2d rng lenX 32 5).1
  -- in graph2d lenX noise1d |> f64_lum_to_u8_rgb_arr
  in noise2d |> f64_lum_to_u8_rgb_arr

