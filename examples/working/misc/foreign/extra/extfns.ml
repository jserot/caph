let mult [x;y] = x * y
let incr [x] = x + 1

let _ = Foreign.register "mult" mult
let _ = Foreign.register "incr" incr
