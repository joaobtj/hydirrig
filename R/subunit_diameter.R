#' Diameter of the manifold line
#'
#' \code{subunit_diameter} calculates the diameter of the manifold line to
#'  achieve a given variation in flow rate
#'
#' @inherit subunit_profile
#' @param q_var_subunit Subunit maximum variation in flow rate, percentage
#' @param ... aditional parameters
#' @export
#' @examples
#' args_subunit_diameter <- list(
#'   q_var_subunit = 10,
#'   h_fim_manifold = 10, s_manifold = 6, s_ini_manifold = 10, n_manifold = 8, dec_manifold = 0.02,
#'   d_lateral = 0.016, s_lateral = 1, s_ini_lateral = 4, n_lateral = 40, dec_lateral = -0.01,
#'   coef_em = 6.41e-7, exp_em = 0.54, rc = 1e-4
#' )
#' do.call(subunit_diameter, args_subunit_diameter)
subunit_diameter <- function(q_var_subunit = 10,
                             h_fim_manifold, s_manifold, s_ini_manifold = s_manifold, n_manifold, dec_manifold,
                             d_lateral, s_lateral, s_ini_lateral = s_lateral, n_lateral, dec_lateral,
                             coef_em, exp_em, rc, ...) {
  f <- function(d_manifold) {
    r <- do.call(subunit_profile, list(h_fim_manifold, d_manifold, s_manifold, s_ini_manifold, n_manifold, dec_manifold, d_lateral, s_lateral, s_ini_lateral, n_lateral, dec_lateral, coef_em, exp_em, rc))
    return((max(r$q_lateral) - min(r$q_lateral)) / max(r$q_lateral) * 100 - q_var_subunit)
  }

  d_manifold <- bisection(f, ...)

  res_x <- do.call(subunit_profile, list(h_fim_manifold, d_manifold$root, s_manifold, s_ini_manifold, n_manifold, dec_manifold, d_lateral, s_lateral, s_ini_lateral, n_lateral, dec_lateral, coef_em, exp_em, rc))

  return(list(
    d_manifold = d_manifold$root,
    h_ini_manifold = res_x$h_ini_manifold,
    h_fim_manifold = h_fim_manifold,
    q_ini_manifold = res_x$q_ini_manifold,
    q_mean_subunit = mean(res_x$q_lateral),
    q_var_subunit = (max(res_x$q_lateral) - min(res_x$q_lateral)) / max(res_x$q_lateral) * 100,
    iterations = d_manifold$iter
  ))
}
