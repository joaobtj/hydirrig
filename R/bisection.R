#' Bisection
#'
#' @param f function
#' @param a max
#' @param b min
#' @param i_max max iterations
#' @param tol Convergence tolerance
bisection <- function(f, a = 100, b = 1e-10, i_max = 100, tol = 1e-10) {
  if (a <= 0) stop("a must be greater than zero")

  f_a <- f(a)

  if (f_a > 0) {
    return(NULL)
    stop("Unable to resolve with this initial condition")
  }

  i <- 0
  repeat {
    x <- (a + b) / 2

    # Calculate f(x)
    f_x <- f(x)

    # Determine the side
    if (f_a * f_x < 0) {
      b <- x
      f_b <- f_x
    } else {
      a <- x
      f_a <- f_x
    }

    i <- i + 1 # Increase

    # Stop maximum loterance
    if (abs(f_x) < tol) {
      break
    }

    # Stop number maximum iterations
    if (i >= i_max) {
      x <- NULL
      stop("max iterations")
    }
  }
  return(list(root = x, iterations = i))
}
