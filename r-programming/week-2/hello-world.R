add2 <- function (x, y) {x + y}

above10 <- function (x) {x[x > 10]}

cube <- function(x, n) { x^3 }

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
