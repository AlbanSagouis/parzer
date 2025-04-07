#' Determine Hemisphere Labels from Lat/Lon Strings
#'
#' @param lon A character vector of longitude strings
#' @param lat A character vector of latitude strings
#' @return A character vector with hemisphere labels like "NW", "SE", etc.
#' @examples
#' pz_hemisphere(c("45W", "170"), c("60N", "35S"))  # -> c("NW", "SE")
pz_hemisphere <- function(lon, lat) {
  n <- length(lat)
  stopifnot("Longitude and latitude vectors must have the same length" = length(lon) == n)

  out <- character(n)

  for (i in seq_len(n)) {
    londir <- ""
    lon_f <- convert_lon(lon[i])

    if (is.na(lon_f)) {
      londir <- ""
    } else {
      if (lon_f > 180) {
        warning(sprintf("longitude value within 180/360 range, got: %s", lon_f))
      }
      londir <- if (lon_f < 0) "W" else "E"
    }

    latdir <- ""
    lat_f <- convert_lat(lat[i])
    if (is.na(lat_f)) {
      latdir <- ""
    } else {
      latdir <- if (lat_f < 0) "S" else "N"
    }

    out[i] <- paste0(latdir, londir)
  }

  out
}
