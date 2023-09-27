#' Lateral coefficients
#'
#' \code{coef_lateral} calculates lateral line coefficients
#'
#' @inherit lateral_profile
#' @param sim Limites da simulação
#' @importFrom stats lm
#' @export
#' @examples
#' args_coef_lateral <- list(
#'   d_lateral = 12, s_lateral = 0.20, s_ini_lateral = 1,
#'   n_lateral = 200, dec_lateral = -0.01, coef_em = 1.67e-7, exp_em = 0.52,
#'   q_unit="m3/s"
#' )
#' do.call(coef_lateral, args_coef_lateral)
coef_lateral <- function(d_lateral, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral, coef_em, exp_em, q_unit=q_unit, sim = list(1, 100, 1)) {
  h_fim <- do.call(seq, sim)
  h_ini <- vector("numeric", length(h_fim))
  q_ini <- vector("numeric", length(h_fim))

  for (i in seq_along(h_fim)) {
    res_x <- lateral_profile(h_fim_lateral=h_fim[i], d_lateral=d_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral,
                             n_lateral=n_lateral, dec_lateral=dec_lateral, coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)
    h_ini[i] <- res_x$h_ini
    q_ini[i] <- res_x$q_ini
  }

  # Power equation: Qini=a*Hini^b
  eq_q <- stats::lm(log(q_ini) ~ log(h_ini))

  ## polinomial
  eq_h <- stats::lm(h_fim ~ poly(h_ini, 20, raw = TRUE))

  return(list(
    "eq_q" = eq_q,
    "eq_h" = eq_h
  ))
}
