{
  nth_term = fun x n =>
    if n == 0 then
      x
    else
      let i = 2 * n + 1 in
      let sign = if n % 2 == 1 then -1 else 1 in
      sign * (%pow% x i) / i,

  sum = fun x total n iterCount =>
    if n == iterCount - 1 then
      total
    else
      sum x (total + (nth_term x n)) (n + 1) iterCount,

  atan = fun x iterCount => sum x 0 0 iterCount,

  pi = fun iterCount =>
    16 * (atan (1 / 5) iterCount) - 4 * (atan (1 / 239) iterCount),

  run = pi,
}
