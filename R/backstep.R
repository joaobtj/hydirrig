#' Backstep procedure
#'
#' \code{backstep} Calculate the pressure head and flow rate profile along
#' a lateral line
#'
#' @inheritParams head_loss
#' @param h_fim Pressure at the end of the line in mca
#' @param d Pipe diameter in meters
#' @param s Emitter spacing in meters
#' @param n Number of emitters
#' @param dec Slope on the lateral line
#' @param coef_em Emission coefficient
#' @param exp_em Emission exponent
#' @importFrom utils head
#' @export
#' @examples
#' backstep(
#'   h_fim = 10, d = 0.012, s = 1, n = 5, dec = 0, coef_em = 8.84e-7,
#'   exp_em = 0.50, rc = 2e-6
#' )
#' backstep(
#'   h_fim = 10, d = 0.012, s = c(0.3, 0.3, 1), n = 12, dec = 0,
#'   coef_em = 8.84e-7, exp_em = 0.50, rc = 2e-6
#' )
backstep <- function(h_fim, d, s, n, dec, coef_em, exp_em, rc) {
  # check if there are more than two emmiters: n>=2
  if (n < 2) stop("The number of emitters (n) must be 2 or more")
  # 0<exp_em<1
  if (exp_em >= 1 || exp_em < 0) stop("exp_em must be between 0 and 1")

  ## EXPAND THE SPACING
  if (!(n %% length(s)) == 0) stop("n must not be a multiple of the vector dimension s")
  se <- utils::head(rep(s, n / length(s)), -1)

  # Vectors
  h_em <- q_em <- q_sec <- vector("numeric", n)
  hf <- vector("numeric", n - 1)

  # Last emitter / i=n
  h_em[n] <- h_fim
  q_em[n] <- coef_em * h_em[n]^exp_em #
  q_sec[n] <- q_em[n]

  # Loop
  for (i in n:2) {
    hf[i - 1] <- do.call(head_loss, list(d, q_sec[i], se[i - 1], rc))$hf
    h_em[i - 1] <- h_em[i] + (dec * se[i - 1]) + hf[i - 1]
    q_em[i - 1] <- coef_em * h_em[i - 1]^exp_em
    q_sec[i - 1] <- q_sec[i] + q_em[i - 1]
  }

  return(list(
    "hf" = hf,
    "h_em" = h_em,
    "q_em" = q_em,
    "q_sec" = q_sec
  ))
}
