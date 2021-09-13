#' Pressure and flow profile in a lateral line
#'
#' \code{lateral_profile} Calculates the pressure and flow profile on the lateral line
#'
#' @param h_fim_lateral Pressure at the end of the lateral line, mca
#' @param d_lateral Diameter of the lateral line, meter
#' @param s_lateral Spacing between emitters on the lateral line, meter
#' @param s_ini_lateral Spacing of the initial stretch of the lateral line, meter
#' @param n_lateral Number of issuers on the lateral line
#' @param dec_lateral Lateral line slope, decimal
#' @param coef_em Coefficient of the flow-pressure equation of the emitter
#' @param exp_em Exponent of the flow-pressure equation of the emitter
#' @param rc Absolute roughness relative to pipe material, meter
#' @export
#' @examples
#' args_lateral_profile <- list(
#'   h_fim_lateral = 10, d_lateral = 0.012, s_lateral = 0.20, s_ini_lateral = 1,
#'   n_lateral = 200, dec_lateral = -0.01, coef_em = 1.67e-7, exp_em = 0.52, rc = 1e-4
#' )
#' do.call(lateral_profile, args_lateral_profile)
lateral_profile <- function(h_fim_lateral, d_lateral, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral, coef_em, exp_em, rc) {

  # initial section
  ll <- do.call(backstep, list(h_fim_lateral, d_lateral, s_lateral, n_lateral, dec_lateral, coef_em, exp_em, rc))
  q_ini_lateral <- ll$q_sec[1]
  hf_sec <- c(do.call(head_loss, list(d_lateral, q_ini_lateral, s_ini_lateral, rc))$hf, ll$hf)
  h_ini_lateral <- ll$h_em[1] + hf_sec[1]

  return(list(
    h_ini_lateral = h_ini_lateral,
    h_fim_lateral = h_fim_lateral,
    q_ini_lateral = q_ini_lateral,
    q_mean_lateral = mean(ll$q_em),
    q_var_lateral = (max(ll$q_em) - min(ll$q_em)) / max(ll$q_em) * 100,
    h_em = ll$h_em,
    q_em = ll$q_em,
    hf_sec = hf_sec,
    q_sec = ll$q_sec
  ))
}
