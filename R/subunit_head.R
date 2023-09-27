
#' Pressure required on the manifold line
#'
#' \code{subunit_head} Calculates the pressure at the beginning of the manifold
#'  line for a given average flow rate
#'
#' @inherit subunit_profile
#' @param q_req_subunit Average flow required on the subunit, m3/s
#' @param ... additional parameters
#' @export
#' @examples
#' args_subunit_head <- list(
#'   q_req_subunit = 2.2222e-6*3600000,
#'   d_manifold = 40, s_manifold = 6, s_ini_manifold = 10, n_manifold = 8, dec_manifold = 0.02,
#'   d_lateral = 16, s_lateral = 1, s_ini_lateral = 4, n_lateral = 40, dec_lateral = -0.01,
#'   coef_em = 6.41e-7*3600000, exp_em = 0.54, q_unit="l/h"
#' )
#' do.call(subunit_head, args_subunit_head)
subunit_head <- function(q_req_subunit, d_manifold, s_manifold, s_ini_manifold = s_manifold, n_manifold, dec_manifold, d_lateral, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral, coef_em, exp_em, q_unit=q_unit) {
  f <- function(h_fim_manifold) {
    q_req_subunit - mean(subunit_profile(h_fim_manifold=h_fim_manifold,
                                         d_manifold=d_manifold, s_manifold=s_manifold, s_ini_manifold=s_ini_manifold,
                                         n_manifold=n_manifold, dec_manifold=dec_manifold,
                                         d_lateral=d_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral, n_lateral=n_lateral,
                                         dec_lateral=dec_lateral, coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)$q_lateral)
  }

  h_fim <- bisection(f, a = 100, b = 1e-10, i_max = 100, tol = 1e-10)


  res_x <- subunit_profile(h_fim_manifold=h_fim$root,
                           d_manifold=d_manifold, s_manifold=s_manifold, s_ini_manifold=s_ini_manifold, n_manifold=n_manifold, dec_manifold=dec_manifold,
                           d_lateral=d_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral, n_lateral=n_lateral, dec_lateral=dec_lateral,
                           coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)

  return(list(
    d_manifold = d_manifold,
    h_ini_manifold = res_x$h_ini_manifold,
    h_fim_manifold = h_fim$root,
    q_ini_manifold = res_x$q_ini_manifold,
    q_mean_subunit = mean(res_x$q_lateral),
    q_var_subunit = (max(res_x$q_lateral) - min(res_x$q_lateral)) / max(res_x$q_lateral) * 100,
    iterations = h_fim$iter
  ))
}
