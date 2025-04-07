#' Split decimal degree into degrees, minutes, and seconds
#'
#' @param x A numeric vector of decimal degrees
#' @return A matrix with columns for degrees, minutes, and seconds
split_decimal_degree <- function(x) {
  # x <- as.numeric(x)
  out <- matrix(NA_real_, nrow = length(x), ncol = 3L)
  colnames(out) <- c("deg", "min", "sec")

  abs_x <- abs(x)
  sign_x <- ifelse(x < 0, -1, 1)

  out[, 1] <- floor(abs_x) * sign_x
  out[, 2] <- min <- floor((abs_x - floor(abs_x)) * 60L)
  out[, 3] <- ((abs_x - floor(abs_x)) - (min / 60)) * 3600

  return(as.data.frame(out))
}

#' Parse parts of latitude strings into degrees, minutes, and seconds
#'
#' @param x A character vector of latitude strings
#' @return A data frame with columns deg, min, and sec
pz_parse_parts_lat <- function(x) {
  # x <- as.character(x)
  lat_dec <- vapply(x, convert_lat, numeric(1))
  return(
    split_decimal_degree(lat_dec)
  )
}

#' Parse parts of longitude strings into degrees, minutes, and seconds
#'
#' @param x A character vector of longitude strings
#' @return A data frame with columns deg, min, and sec
pz_parse_parts_lon <- function(x) {
  # x <- as.character(x)
  lon_dec <- vapply(x, convert_lon, numeric(1))
  return(
    split_decimal_degree(lon_dec)
  )
}
