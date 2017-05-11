# get sum of rainfall over given days
get_rd_sum <- function(x, nday = 5){
  i <- nday - 1
  c(colSums(laply(0:i, function(y) lag(x, y)))[-i:-1], rep(NA, i))
}
