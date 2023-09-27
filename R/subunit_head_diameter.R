
#' Pressure and diameter on the manifold line
#'
#' \code{subunit_head_diameter} calculates manifold line diameter and
#' pressure for a given average flow rate and a given flow rate variation
#'
#' @inherit subunit_profile
#' @inherit subunit_head
#' @inherit subunit_diameter
#' @export
#' @examples
#' args_subunit_head_diameter <- list(
#'   q_req_subunit = 2.2222e-6*3600000, q_var_subunit = 10,
#'   s_manifold = c(0.3, 0.3, 2), s_ini_manifold = 10, n_manifold = 12, dec_manifold = 0.02,
#'   d_lateral = 16, s_lateral = 1, s_ini_lateral = 4, n_lateral = 40, dec_lateral = -0.01,
#'   coef_em = 6.41e-7*3600000, exp_em = 0.54, q_unit="l/h"
#' )
#' do.call(subunit_head_diameter, args_subunit_head_diameter)
subunit_head_diameter <- function(q_req_subunit, q_var_subunit = 10, s_manifold, s_ini_manifold = s_manifold, n_manifold, dec_manifold, d_lateral, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral, coef_em, exp_em, q_unit=q_unit) {
  i <- 0
  h_fim_manifold <- 100
  d_manifold <- 1000
  repeat{
    res_h <- subunit_head(q_req_subunit=q_req_subunit,
                          d_manifold=d_manifold, s_manifold=s_manifold, s_ini_manifold=s_ini_manifold, n_manifold=n_manifold, dec_manifold=dec_manifold,
                          d_lateral=d_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral, n_lateral=n_lateral, dec_lateral=dec_lateral,
                          coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)
    i <- i + res_h$iterations
    h_fim_manifold <- res_h$h_fim_manifold

    res_d <- subunit_diameter(q_var_subunit=q_var_subunit,
                              h_fim_manifold=h_fim_manifold, s_manifold=s_manifold, s_ini_manifold=s_ini_manifold, n_manifold=n_manifold, dec_manifold=dec_manifold,
                              d_lateral=d_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral, n_lateral=n_lateral, dec_lateral=dec_lateral,
                              coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)
    i <- i + res_d$iterations
    d_manifold <- res_d$d_manifold

    # i <- i + 1

    if (all(
      abs(q_req_subunit - res_d$q_mean_subunit) < 1e-10,
      abs(q_var_subunit - res_d$q_var) < 1e-10
    )) {
      break
    }
    # Stop number maximum iterations
    if (i >= 10000) {
      return(NULL)
      stop("max iterations")
    }
  }


  res_x <- subunit_profile(
    h_fim_manifold=h_fim_manifold,
    d_manifold=d_manifold, s_manifold=s_manifold, s_ini_manifold=s_ini_manifold, n_manifold=n_manifold, dec_manifold=dec_manifold,
    d_lateral=d_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral, n_lateral=n_lateral, dec_lateral=dec_lateral,
    coef_em=coef_em, exp_em=exp_em, q_unit=q_unit
  )



  return(list(
    d_manifold = d_manifold,
    h_ini_manifold = res_x$h_ini_manifold,
    h_fim_manifold = h_fim_manifold,
    q_ini_manifold = res_x$q_ini_manifold,
    q_mean_subunit = mean(res_x$q_lateral),
    q_var_subunit = (max(res_x$q_lateral) - min(res_x$q_lateral)) / max(res_x$q_lateral) * 100,
    iterations = i
  ))
}
