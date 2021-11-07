
let f64_lum_to_u8_rgb_arr [n][m] (arr: [n][m]f64): [n][m][3]u8 = map (\y -> map (\x -> let i = x |> (* 256.0) |> u8.f64 |> (\z -> z - 1) in [i, i, i]) y) arr

let main (screenX: i64) (screenY: i64) (depth: i32)
         (xmin: f32) (ymin: f32) (xmax: f32) (ymax: f32): [screenY][screenX][3]u8 =
           let a: f64 = 0.5
           in map (\y -> map (\x -> a) (iota screenX)) (iota screenY) |> f64_lum_to_u8_rgb_arr
