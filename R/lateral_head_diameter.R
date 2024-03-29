#' Pressure and diameter on the lateral line
#'
#' \code{lateral_head_diameter} calculates lateral line diameter and
#' pressure for a given average flow rate and a given flow rate variation
#'
#' @inheritParams lateral_profile
#' @inheritParams lateral_head
#' @inheritParams lateral_diameter
#' @export
#' @examples
#' args_lateral_head_diameter <- list(
#'   q_req_lateral = 5.5555e-7*3600000
#'   ,
#'   q_var_lateral = 10
#'   ,
#'    s_lateral = 0.20
#'    ,
#'     s_ini_lateral = 1
#'     ,
#'
#'   n_lateral = 12
#'   ,
#'    dec_lateral = -0.00
#'    ,
#'     coef_em = 1.67e-7*3600000
#'     ,
#'      exp_em = 0.52
#'      ,
#'      q_unit="l/h"
#' )
#' do.call(lateral_head_diameter, args_lateral_head_diameter)
lateral_head_diameter <- function(q_req_lateral, q_var_lateral = 10, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral, coef_em, exp_em, q_unit=q_unit) {
  i <- 0
  h_fim_lateral <- 100
  d_lateral <- 1000
  repeat{
    res_h <- lateral_head(
      q_req_lateral=q_req_lateral, d_lateral=d_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral,
      n_lateral=n_lateral, dec_lateral=dec_lateral,
      coef_em=coef_em, exp_em=exp_em, q_unit=q_unit
    )
    i <- i + res_h$iterations
    h_fim_lateral <- res_h$h_fim_lateral

    res_d <- lateral_diameter(
      q_var_lateral=q_var_lateral, h_fim_lateral=h_fim_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral,
      n_lateral=n_lateral, dec_lateral=dec_lateral,
      coef_em=coef_em, exp_em=exp_em, q_unit=q_unit
    )
    i <- i + res_d$iterations
    d_lateral <- res_d$d_lateral

    # i <- i + 1

    if (all(
      abs(q_req_lateral - res_d$q_mean_lateral) < 1e-10,
      abs(q_var_lateral - res_d$q_var_lateral) < 1e-10
    )) {
      break
    }
  }


  res_x <- lateral_profile(h_fim_lateral=h_fim_lateral, d_lateral=d_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral,
                           n_lateral=n_lateral, dec_lateral=dec_lateral, coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)


  return(list(
    d_lateral = d_lateral,
    h_ini_lateral = res_x$h_ini_lateral,
    h_fim_lateral = h_fim_lateral,
    q_ini_lateral = res_x$q_ini_lateral,
    q_mean_lateral = mean(res_x$q_em),
    q_var_lateral = (max(res_x$q_em) - min(res_x$q_em)) / max(res_x$q_em) * 100,
    iterations = i
  ))
}
