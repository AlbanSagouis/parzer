#' parse coordinates into degrees, minutes and seconds
#'
#' @export
#' @name parse_parts
#' @param str (character) string including longitude or latitude
#' @return data.frame with columns for:
#'
#' - deg (integer)
#' - min (integer)
#' - sec (numeric)
#'
#' NA/NaN given upon error
#'
#' @examples
#' parse_parts_lon("140.4183318")
#' \dontrun{
#' parse_parts_lon("174.6411133")
#' parse_parts_lon("-45.98739874")
#' parse_parts_lon("40.123W")
#'
#' parse_parts_lat("45N54.2356")
#' parse_parts_lat("40.4183318")
#' parse_parts_lat("-74.6411133")
#' parse_parts_lat("-45.98739874")
#' parse_parts_lat("40.123N")
#' parse_parts_lat("N40°25’5.994")
#'
#' # not working, needs format input
#' parse_parts_lat("N455698735")
#'
#' # multiple
#' x <- c("40.123°", "40.123N74.123W", "191.89", 12, "N45 04.25764")
#' parse_parts_lat(x)
#' system.time(parse_parts_lat(rep(x, 10^2)))
#' }
#'
#' @export
#' @rdname parse_parts
parse_parts_lon <- function(str) {
  assert(x = str, y = c("character", "numeric"))
  str <- as.character(x = str)
  result <- scrub(str) |>
    vapply(FUN = convert_lon, numeric(1)) |>
    split_decimal_degree()
  return(result)
}

#' @export
#' @rdname parse_parts
parse_parts_lat <- function(str) {
  assert(x = str, y = c("character", "numeric"))
  str <- as.character(x = str)
  result <- scrub(str) |>
    vapply(FUN = convert_lat, numeric(1)) |>
    split_decimal_degree()
  return(result)
}

#' Split decimal degree into degrees, minutes, and seconds
#'
#' @param x A numeric vector of decimal degrees
#' @return A data frame with columns for degrees, minutes, and seconds
split_decimal_degree <- function(x) {
  abs_x <- abs(x)
  sign_x <- ifelse(x < 0, -1L, 1L)
  deg <- as.integer(floor(abs_x) * sign_x)
  frac_minutes <- (abs_x - floor(abs_x)) * 60
  min <- floor(frac_minutes) |> as.integer()
  sec <- (frac_minutes - min) * 60

  out <- data.frame(deg = deg, min = min, sec = sec)
  return(out)
}
