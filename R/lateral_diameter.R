#' Diameter of the lateral line
#'
#' \code{lateral_q_req} calculates the diameter of the lateral line to achieve
#'  a given variation in flow rate
#'
#' @inheritParams lateral_profile
#' @param q_var_lateral Lateral maximum variation in flow rate, percentage
#' @param q_unit=q_unit aditional parameters
#' @export
#' @examples
#' args_lateral_diameter <- list(
#'   q_var_lateral = 10, h_fim_lateral = 10, s_lateral = 0.20, s_ini_lateral = 1,
#'   n_lateral = 200, dec_lateral = -0.01, coef_em = 1.67e-7*360000, exp_em = 0.52, q_unit="l/h"
#' )
#' do.call(lateral_diameter, args_lateral_diameter)
lateral_diameter <- function(q_var_lateral = 10, h_fim_lateral, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral, coef_em, exp_em, q_unit=q_unit) {
  f <- function(d_lateral) {
    r <- lateral_profile(h_fim_lateral=h_fim_lateral, d_lateral=d_lateral, s_lateral=s_lateral,
                         s_ini_lateral=s_ini_lateral, n_lateral=n_lateral, dec_lateral=dec_lateral,
                         coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)
    return((max(r$q_em) - min(r$q_em)) / max(r$q_em) * 100 - q_var_lateral)
  }

  d_lateral <- bisection(f, a=1000, b=1, i_max = 100, tol = 1e-10)

  if (is.null(d_lateral)) {
    stop("Unable to resolve with this initial condition",
      call. = FALSE
    )
  }

  res_x <- lateral_profile(h_fim_lateral=h_fim_lateral, d_lateral=d_lateral$root, s_lateral=s_lateral,
                           s_ini_lateral=s_ini_lateral, n_lateral=n_lateral, dec_lateral=dec_lateral,
                           coef_em=coef_em, exp_em=exp_em, q_unit=q_unit)
  return(list(
    d_lateral = d_lateral$root,
    h_ini_lateral = res_x$h_ini_lateral,
    h_fim_lateral = h_fim_lateral,
    q_ini_lateral = res_x$q_ini_lateral,
    q_mean_lateral = mean(res_x$q_em),
    q_var_lateral = (max(res_x$q_em) - min(res_x$q_em)) / max(res_x$q_em) * 100,
    iterations = d_lateral$iterations
  ))
}
