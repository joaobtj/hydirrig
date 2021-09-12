#' Reynolds Number
#'
#' \code{re} calculates the Reynolds number
#'
#' @inheritParams head_loss
#' @param v Kinematic viscosity of fluid in square meters per second.
#'
#' @return re Reynolds number
#' @export
#' @examples
#' re(d = 0.025, q = 0.000000001, v = 1.01e-6)
#' re(d = 0.050, q = 0.0006, v = 1.01e-6)
re <- function(d, q, v) {
  re <- (4 * q) / (pi * d * v)
  return(re)
}
