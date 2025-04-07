#' get hemisphere from long/lat coordinates
#'
#' BEWARE: EXPERIMENTAL
#'
#' @export
#' @param lon (character/numeric/integer) one or more longitude values
#' @param lat (character/numeric/integer) one or more latitude values
#' @details length(lon) == length(lat)
#' @return character vector of quadrants, one of: NE, NW, SE, SW.
#' if one of the coordinate values is invalid, and one is valid, you get
#' a length 1 string. if both coordinate values are bad, you get
#' a zero length string.
#'
#' Warnings are thrown on invalid values
#' @examples
#' # NE
#' parse_hemisphere("74.123E", "45N54.2356")
#' \dontrun{
#' # NW
#' parse_hemisphere(-120, 40.4183318)
#' # SW
#' parse_hemisphere(-120, -40.4183318)
#' # SE
#' parse_hemisphere(120, -40.4183318)
#'
#' # bad inputs, get one of the two strings
#' parse_hemisphere(-181, -40.4183318)
#' parse_hemisphere(-120, -192.4183318)
#'
#' }
parse_hemisphere <- function(lon, lat) {
  lint_inputs(lon, lat, "")
  n <- length(lat)
  stopifnot("Longitude and latitude vectors must have the same length" =
              length(lon) == n)

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

  return(out)
}
