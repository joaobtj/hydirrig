
#' Reference Evapotranspiration - The FAO Penman-Monteith method
#'
#' @param tem_max Maximum air temperature, C
#' @param tem_min Minimum air temperature, C
#' @param umd_max Maximum relative humidity, %
#' @param umd_min Minimum relative humidity, %
#' @param lat Latitude,
#' @param alt Altitude abose sea level
#' @param uv Wind speed, m/s
#' @param uz Height of measurement of wind speed above ground surface. Default is 2 m.
#' @param rs Solar radiation, MJ m^-2^^day^-1
#' @param g Soil heat flux. Default is 0
#' @param date Date
#' @return
#' @export
#' @examples
#' et0_calc(
#'   tem_max = 21.5, tem_min = 12.3, umd_max = 84, umd_min = 63, uv = 2.78, uz = 10,
#'   rs = 22.07, g = 0, lat = 50.8, alt = 100, date = as.Date("2009-7-6")
#' )
#'
et0_calc <- function(tem_max, tem_min, umd_max, umd_min, uv, uz = 2, rs=NULL, g = 0, lat, alt, date) {
  tem_med <- (tem_max + tem_min) / 2
  u2 <- u2_calc(uv, uz)
  gama <- gama_calc(alt)
  es <- es_calc(tem_max, tem_min)
  ea <- ea_calc(tem_max, tem_min, umd_max, umd_min)
  delta <- delta_calc(tem_max, tem_min)
  ra <- ra_calc(lat, date)
  rso <- rso_calc(alt, ra)
  rns <- rns_calc(rs)
  rnl <- rnl_calc(tem_max, tem_min, ea, rs, rso)
  rn <- rns - rnl


  et0 <- (0.408 * delta * (rn - g) + gama * 900 / (tem_med + 273) * u2 * (es - ea)) / (delta + gama * (1 + 0.34 * u2))

  return(et0)
}





## ajuste da velocidade do vento a 2 m de altura
u2_calc <- function(uv, uz) {
  if (uz != 2) {
    u2 <- uv * 4.87 / (log(67.8 * uz - 5.42))
  } else {
    u2 <- uv
  }

  return(u2)
}


## constante psicrométrica
gama_calc <- function(alt) {
  p <- 101.3 * ((293 - 0.0065 * alt) / 293)^5.26
  return(0.665e-3 * p)
}


## pressão de vapor
e0T <- function(t) {
  e0T <- 0.6108 * exp(17.27 * t / (t + 237.3))
}

es_calc <- function(tem_max, tem_min) {
  es <- (e0T(tem_max) + e0T(tem_min)) / 2
}

ea_calc <- function(tem_max, tem_min, umd_max, umd_min) {
  ea <- (e0T(tem_min) * umd_max / 100 + e0T(tem_max) * umd_min / 100) / 2
}


delta_calc <- function(tem_max, tem_min) {
  tem_med <- (tem_max + tem_min) / 2
  delta <- (4098 * (0.6108 * exp(17.27 * tem_med / (tem_med + 237.3)))) / (tem_med + 237.3)^2
}

## radiação
ra_calc <- function(lat, date) {
  gsc <- 0.0820
  lat_rad <- lat * pi / 180
  j <- yday(date)
  dr <- 1 + 0.033 * cos(2 * pi / 365 * j)
  delta <- 0.409 * sin(2 * pi / 365 * j - 1.39)
  ws <- acos(-tan(lat_rad) * tan(delta))
  ra <- 24 * 60 / pi * gsc * dr * (ws * sin(lat_rad) * sin(delta) + cos(lat_rad) * cos(delta) * sin(ws))
}

rso_calc <- function(alt, ra) {
  rso <- (0.75 + 2e-5 * alt) * ra
}

rns_calc <- function(rs) {
  rns <- (1 - 0.23) * rs
}

rnl_calc <- function(tem_max, tem_min, ea, rs, rso) {
  sigma <- 4.903e-9
  rnl <- sigma * (((tem_max + 273.16)^4 + (tem_min + 273.16)^4) / 2) * (0.34 - 0.14 * sqrt(ea)) * (1.35 * rs / rso - 0.35)
}

## Dia juliano
yday <- function(x) {
  return(as.POSIXlt(x)$yday + 1)
}
