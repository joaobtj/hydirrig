#' Pressure and flow profile in a lateral line
#'
#' \code{lateral_profile} Calculates the pressure and flow profile on the lateral line
#'
#' @param h_fim_lateral Pressure at the end of the lateral line, mca
#' @param d_lateral Diameter of the lateral line, milimeter
#' @param s_lateral Spacing between emitters on the lateral line, meter
#' @param s_ini_lateral Spacing of the initial stretch of the lateral line, meter
#' @param n_lateral Number of issuers on the lateral line
#' @param dec_lateral Lateral line slope, decimal
#' @param coef_em Coefficient of the flow-pressure equation of the emitter
#' @param exp_em Exponent of the flow-pressure equation of the emitter
#' @export
#' @examples
#' args_lateral_profile <- list(
#'   h_fim_lateral = 10
#'   ,
#'   d_lateral = 12
#'   ,
#'    s_lateral = 0.20
#'    ,
#'    s_ini_lateral = 1
#'    ,
#'
#'   n_lateral = 20
#'   ,
#'   dec_lateral = -0.00
#'   ,
#'   coef_em = 1.67e-7
#'   ,
#'    exp_em = 0.52
#'
#'
#'
#' )
#'
#' do.call(lateral_profile, args_lateral_profile)
lateral_profile <- function(h_fim_lateral, d_lateral, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral, coef_em, exp_em,q_unit=q_unit) {

  # initial section
  ll <- backstep(h_fim=h_fim_lateral, d=d_lateral, s=s_lateral, n=n_lateral,
                 dec=dec_lateral, coef_em=coef_em, exp_em=exp_em,q_unit=q_unit)
  q_ini_lateral <- ll$q_sec[1]
  hf_sec <- c(head_loss(d=d_lateral, q=q_ini_lateral, l=s_ini_lateral, q_unit=q_unit)$hf, ll$hf)
  h_ini_lateral <- ll$h_em[1] + hf_sec[1]

  return(
    list(
    h_ini_lateral = h_ini_lateral,
    h_fim_lateral = h_fim_lateral,
    q_ini_lateral = q_ini_lateral,
    q_mean_lateral = mean(ll$q_em),
    q_var_lateral = (max(ll$q_em) - min(ll$q_em)) / max(ll$q_em) * 100,
    h_em = ll$h_em,
    q_em = ll$q_em,
    hf_sec = hf_sec,
    q_sec = ll$q_sec
  )
  )
}
