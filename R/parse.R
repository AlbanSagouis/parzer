#' Parse vector of latitude strings to decimal degrees
#'
#' @param x A character vector of latitude strings
#' @return A numeric vector of parsed decimal degrees
pz_parse_lat <- function(x) {
  # x <- as.character(x)
  vapply(x, convert_lat, numeric(1))
}

#' Parse vector of longitude strings to decimal degrees
#'
#' @param x A character vector of longitude strings
#' @return A numeric vector of parsed decimal degrees
pz_parse_lon <- function(x) {
  # x <- as.character(x)
  vapply(x, convert_lon, numeric(1))
}
