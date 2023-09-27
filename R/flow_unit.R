flow_unit <- function(q, q_unit = "m3/s", operator="div") {
  if (q_unit == "m3/s") f <- 1
  if (q_unit == "l/h") f <- 3600000
  if (q_unit == "m3/h") f <- 3600


  if (operator == "div") {
    return(q / f)
  }
  if (operator == "mult") {
    return(q * f)
  }

}
