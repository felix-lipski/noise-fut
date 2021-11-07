import "../lib/github.com/diku-dk/cpprandom/random"

module dist = uniform_real_distribution f64 minstd_rand

let noise1d rng (n: i64) =
  let xs = (iota n)
  let f r _ = (dist.rand (0.0,1.0) r)
  let rngs = minstd_rand.split_rng n rng
  let (rngs, ys) = unzip (map2 f rngs xs)
  let rng = minstd_rand.join_rng rngs
  in (rng, ys)

let noise2d rng (n: i64) (m: i64): (minstd_rand.rng, [n][m]f64) =
  let xs = (iota n)
  let f r _ = noise1d r m
  let rngs = minstd_rand.split_rng n rng
  let (rngs, ys) = unzip (map2 f rngs xs)
  let rng = minstd_rand.join_rng rngs
  in (rng, ys)

let f64_lum_to_u8_rgb_arr [n][m] (arr: [n][m]f64): [n][m][3]u8 = map (\y -> map (\x -> let i = x |> (* 256.0) |> u8.f64 |> (\z -> z - 1) in [i, i, i]) y) arr

let main (screenX: i64) (screenY: i64): [screenY][screenX][3]u8 =
           let rng = minstd_rand.rng_from_seed [123]
           let c = (dist.rand (0.0,1.0) rng).1
           let a: f64 = 0.5
           in (noise2d rng screenY screenX).1 |> f64_lum_to_u8_rgb_arr
