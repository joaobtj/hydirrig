#' Backstep procedure
#'
#' \code{backstep} Calculate the pressure head and flow rate profile along
#' a lateral line
#'
#' @inheritParams head_loss
#' @param h_fim Pressure at the end of the line in mca
#' @param d Pipe diameter in milimeters
#' @param s Emitter spacing in meters
#' @param n Number of emitters
#' @param dec Slope on the lateral line
#' @param coef_em Emission coefficient
#' @param exp_em Emission exponent
#' @importFrom utils head
#' @export
#' @examples
#' backstep(
#'   h_fim = 10, d = 12, s = 1, n = 5, dec = 0, coef_em = 8.84e-7,
#'   exp_em = 0.50
#' )
#' backstep(
#'   h_fim = 10
#'   ,
#'   d = 12
#'   ,
#'   s = c(0.3, 0.3, 1)
#'   ,
#'   n = 12
#'   ,
#'   dec = 0
#'   ,
#'   coef_em = 8.84e-7*3600000
#'   ,
#'   exp_em = 0.50
#'   ,
#'   q_unit="l/h"
#' )
backstep <- function(h_fim, d, s, n, dec, coef_em, exp_em, ...) {
  # check if there are more than two emmiters: n>=2
  if (n < 2) stop("The number of emitters (n) must be 2 or more")
  # 0<exp_em<1
  if (exp_em >= 1 || exp_em < 0) stop("exp_em must be between 0 and 1")


  # standardization of units
  if(!exists("q_unit")) q_unit="m3/s"
  coef_em=flow_unit(coef_em, q_unit)


  ## EXPAND THE SPACING
  if (!(n %% length(s)) == 0) stop("n must not be a multiple of the vector dimension s")
  se <- utils::head(rep(s, n / length(s)), -1)

  # Vectors
  h_em <- q_em <- q_sec <- vector("numeric", n)
  hf <- vector("numeric", n - 1)

  # Last emitter / i=n
  h_em[n] <- h_fim
  q_em[n] <- coef_em * h_em[n]^exp_em
  q_sec[n] <- q_em[n]

  # Loop
  for (i in n:2) {
    hf[i - 1] <- head_loss(d=d, q=flow_unit(q_sec[i], q_unit, operator = "mult"), l=se[i-1],...)$hf
    #hf[i - 1] <- do.call(head_loss, list(d=d, q=q_sec[i], l=se[i - 1]))$hf
    h_em[i - 1] <- h_em[i] + (dec * se[i - 1]) + hf[i - 1]
    q_em[i - 1] <- coef_em * h_em[i - 1]^exp_em
    q_sec[i - 1] <- q_sec[i] + q_em[i - 1]
  }

  return(list(
    "hf" = hf,
    "h_em" = h_em,
    "q_em" = flow_unit(q_em, q_unit, operator = "mult"),
    "q_sec" = flow_unit(q_sec, q_unit, operator = "mult")
  ))
}
