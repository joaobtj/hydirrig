#' Emmiter equation
#'
#' \code{coef_emitter} Calculates the coefficients of the emitter pressure-flow power equation
#'
#' @param h Pressure
#' @param q Flow rate
#' @importFrom stats lm
#' @importFrom stats coef
#' @export
#' @examples
#' h <- c(10, 20, 30)
#' q <- c(7.9, 11.4, 14.1)
#' coef_emitter(h, q)
coef_emitter <- function(h, q) {
  m <- lm(log(q) ~ log(h))

  return(list(
    coef_em = unname(exp(coef(m)[1])),
    exp_em = unname(coef(m)[2])
  ))
}
