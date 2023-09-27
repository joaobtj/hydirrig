
#' Pressure and flow profile in a subunit
#'
#' \code{subunit_profile} Calculates the pressure and flow profile in a subunit
#'
#' @inherit lateral_profile
#' @param h_fim_manifold Pressure at the end of the manifold line, mca
#' @param d_manifold Diameter of the manifold line, meter
#' @param s_manifold Spacing between lateral lines in the manifold line, meter
#' @param s_ini_manifold Spacing of the initial section of the manifold line, meter
#' @param n_manifold Number of lateral lines in the manifold line
#' @param dec_manifold Slope of the manifold line, decimal
#' @export
#' @examples
#' args_subunit_profile <- list(
#'   h_fim_manifold = 10,
#'   d_manifold = 40, s_manifold = 6, s_ini_manifold = 10, n_manifold = 8, dec_manifold = 0.02,
#'   d_lateral = 16, s_lateral = 1, s_ini_lateral = 4, n_lateral = 40, dec_lateral = -0.01,
#'   coef_em = 6.41e-7*3600000, exp_em = 0.54, q_unit="l/h"
#' )
#' do.call(subunit_profile, args_subunit_profile)
subunit_profile <- function(h_fim_manifold, d_manifold, s_manifold, s_ini_manifold = s_manifold, n_manifold, dec_manifold, d_lateral, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral, coef_em, exp_em, q_unit=q_unit) {
  cc <- do.call(coef_lateral, list(d_lateral, s_lateral, s_ini_lateral, n_lateral, dec_lateral, coef_em, exp_em, q_unit=q_unit))

  coef_manifold <- unname(exp(stats::coef(cc$eq_q)[1]))
  exp_manifold <- unname(stats::coef(cc$eq_q)[2])
  res_man <- do.call(lateral_profile, list(h_fim_manifold, d_manifold, s_manifold, s_ini_manifold, n_manifold, dec_manifold, coef_manifold, exp_manifold, q_unit=q_unit))


  res_lateral <- vector("list", length(res_man$h_em))
  for (i in seq_along(res_man$h_em)) {
    options(warn = -1) # turn off warnings
    h_fim_lateral <- unname(stats::predict(cc$eq_h, newdata = data.frame(h_ini = res_man$h_em)))[i]
    options(warn = 1) # turn warnings back on
    res_lateral[[i]] <- do.call(lateral_profile, list(h_fim_lateral, d_lateral, s_lateral, s_ini_lateral, n_lateral, dec_lateral, coef_em, exp_em, q_unit=q_unit))
    # do.call pode ser passada a lista de argumentos fora do loop?
  }

  return(list(
    h_ini_manifold = res_man$h_ini,
    q_ini_manifold = res_man$q_ini,
    hf_manifold = res_man$hf,
    h_ini_lateral = res_man$h_em,
    q_ini_lateral = res_man$q_em,
    h_lateral = sapply(res_lateral, "[[", "h_em"),
    q_lateral = sapply(res_lateral, "[[", "q_em")
  ))
  # retornar a variação de vazão e etc (ver lateral_profile)
  # melhorar a visualização do perfil (para muitos valores)
  # fazer uma s3 para print
}
