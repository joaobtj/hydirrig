#' Pressure required on the lateral line
#'
#' \code{lateral_head} Calculates the pressure at the beginning of the lateral
#'  line for a given average flow rate
#'
#' @inheritParams lateral_profile
#' @param q_req_lateral Average flow required on the lateral line, m3/s
#' @param q_unit=q_unit additional parameters
#' @export
#' @examples
#' args_lateral_head <- list(
#'   q_req_lateral = 5.5555e-7*3600000
#'   ,
#'    d_lateral = 12
#'    ,
#'     s_lateral = 0.20
#'     ,
#'      s_ini_lateral = 1
#'      ,
#'
#'   n_lateral = 200
#'   ,
#'    dec_lateral = -0.00
#'    ,
#'     coef_em = 1.67e-7*3600000
#'     ,
#'      exp_em = 0.52
#'      ,
#'       q_unit="l/h"
#' )
#' do.call(lateral_head, args_lateral_head)
lateral_head <- function(q_req_lateral, d_lateral, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral, coef_em, exp_em, q_unit=q_unit) {
  f <- function(h_fim_lateral) {
    q_req_lateral -
      mean(lateral_profile(h_fim_lateral=h_fim_lateral, d_lateral=d_lateral, s_lateral=s_lateral, s_ini_lateral=s_ini_lateral,
                           n_lateral=n_lateral, dec_lateral=dec_lateral, coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)$q_em)
  }



  h_fim_lateral <- bisection(f, a = 100, b = 1e-10, i_max = 100, tol = 1e-10)

  if (is.null(h_fim_lateral)) {
    stop("Unable to resolve with this initial condition",
      call. = FALSE
    )
  }

  res_x <- lateral_profile(h_fim_lateral=h_fim_lateral$root, d_lateral=d_lateral, s_lateral=s_lateral,
                           s_ini_lateral=s_ini_lateral, n_lateral=n_lateral, dec_lateral=dec_lateral,
                           coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)

  return(list(
    d_lateral = d_lateral,
    h_ini_lateral = res_x$h_ini_lateral,
    h_fim_lateral = h_fim_lateral$root,
    q_ini_lateral = res_x$q_ini_lateral,
    q_mean_lateral = mean(res_x$q_em),
    q_var_lateral = (max(res_x$q_em) - min(res_x$q_em)) / max(res_x$q_em) * 100,
    iterations = h_fim_lateral$iterations
  ))
}
