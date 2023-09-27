#' Head loss over the length of pipe
#'
#' \code{headLoss} calculate the head loss by friction in pipes by different
#' methods
#'
#' @param d  Diameter in milimeters
#' @param q  Flow rate in cubic meters per second
#' @param q_unit Flow measurement unit. Default is cubic meters per second (m3/s).
#'               Also allowed liters per hour (l/h) and cubic meters per hour (m3/h)
#' @param l  Length of pipe in meters
#' @param rc Roughness coefficient. Absolute roughness (in meters) for
#'           Universal equation.
#' @param v  Kinematic viscosity of fluid in square meters per second. By
#'           default use the value for water at 20 Celsius degree
#'           (\code{v = 1.01e-6 m^2/s}). Unnecessary for empirical
#'           equations.
#' @param g  Gravitational acceleration. By default use the value
#'           \code{g = 9.81 m/s^2}
#' @param x1  Initial parameter of f for Newton-Raphson.
#'            By default \code{x1=0.06}.
#'            Unnecessary for empirical or explicit equations.
#'
#' @return hf Head loss in meters
#' @export
#' @examples
#' head_loss(d = 25, q = 31e-6, l = 100, rc = 1e-4)
#' head_loss(d = 25, q = 111.6, q_unit="l/h", l = 100, rc = 1e-4)
head_loss <- function(d, q, q_unit="m3/s", l, rc = 0.002, v = 1.01e-6, g = 9.81, x1 = 0.06) {
  # standardization of units
  q=flow_unit(q, q_unit)
  d=d/1000
  # Reynolds number
  re <- (4 * q) / (pi * d * v)

  # head loss calculation
  if (re < 2000) {
    ## laminar flow
    regime <- "laminar"
    f <- 64 / re
  } else if (re <= 4000) {
    ## transition: interpolation (Dunlop, 1991)
    regime <- "transition"
    y3 <- -0.86859 * log(rc / (3.7 * d) + 5.74 / 4000^0.9)
    y2 <- rc / (3.7 * d) + 5.74 / re^0.9
    fa <- y3^(-2)
    fb <- fa * (2 - 0.00514215 / (y2 * y3))
    r <- re / 2000
    x1 <- 7 * fa - fb
    x2 <- 0.128 - 17 * fa + 2.5 * fb
    x3 <- -0.128 + 13 * fa - 2 * fb
    x4 <- r * (0.032 - 3 * fa + 0.5 * fb)
    f <- (x1 + r * (x2 + r * (x3 + x4)))
  } else {
    ## turbulent
    regime <- "turbulent"

    ## Colebrook-White
    x1 <- (1 / x1)^0.5
    w <- (rc / (3.7 * d)) + ((2.51 * x1) / re)
    h <- (2.18 / (((rc * re) / (3.7 * d)) + (2.51 * x1)))

    repeat {
      x2 <- x1 - (((x1 + (2 * log10(w))) / (1 + h)))
      x <- abs(x2 - x1)
      x1 <- x2
      if (x < 0.00001) break
    }

    f <- 1 / x1^2
  }

  hf <- (16 * f * q^2 * l) / (2 * g * pi^2 * d^5)

  return(list(
    "hf" = hf,
    "regime" = regime,
    "f" = f
  ))
}
