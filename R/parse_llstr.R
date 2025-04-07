#' Parse a character vector of latitude and longitude strings
#' This function parses geographic coordinate strings in various formats,
#' including compact cardinal forms (e.g., "N4.9196E101.347") and
#' different delimiters such as commas, spaces, semicolons, or dots.
#' It supports both "lat first" and "lon first" conventions if the cardinal
#' direction is provided.
#' @export
#' @param x (character) string with latitude and longitude, one or more in a vector.
#' @return A data.frame with parsed latitude and longitude in decimal degrees.
#' @examples
#' parse_latlon("N 04.1683, E 101.5823")
#' parse_latlon("N04.82344, E101.61320")
#' parse_latlon("N 04.25164, E 101.70695")
#' parse_latlon("N05.03062, E101.75172")
#' parse_latlon("N05.03062,E101.75172")
#' parse_latlon("N4.9196, E101.345")
#' parse_latlon("N4.9196, E101.346")
#' parse_latlon("N4.9196, E101.347")
#' parse_latlon("4.9196N, 101.345E")
#' # no comma
#' parse_latlon("N4.9196 E101.347")
#' parse_latlon("4.9196N 101.345E")
#' # no space
#' parse_latlon("N4.9196E101.347")
#' parse_latlon("4.9196N101.347E")
#'
#' # DMS
#' parse_latlon("N4 51'36\", E101 34'7\"")
#' parse_latlon(c("4 51'36\"S, 101 34'7\"W", "N4 51'36\", E101 34'7\""))
#' # inverted
#' parse_latlon("E101.345, N04.9196")
#' parse_latlon("4.9196N 101.345E")
#' parse_latlon("4.9196N101.345E")

parse_latlon <- function(x) {
  assert(x, "character")
  n <- length(x)

  res <- lapply(x, function(str) {
    str <- stringi::stri_trim_both(str = str)
    str <- stringi::stri_replace_all_regex(str = str, pattern = " +",
                                           replacement = " ")

    nb_commas <- stringi::stri_count_fixed(str = str, pattern = ",")
    nb_spaces <- stringi::stri_count_fixed(str = str, pattern = " ")
    nb_semicolons <- stringi::stri_count_fixed(str = str, pattern = ";")
    nb_dots <- stringi::stri_count_fixed(str = str, pattern = ".")

    # Case 1: Split based on common delimiters
    if (nb_commas == 1) {
      parts <- stringi::stri_split_fixed(str = str, pattern = ",", n = 2)[[1]]
    } else if (nb_commas == 0 && nb_spaces == 1 && nb_semicolons == 0 && (nb_dots == 0 || nb_dots == 2)) {
      parts <- stringi::stri_split_fixed(str = str, pattern = " ", n = 2)[[1]]
    } else if (nb_semicolons == 1) {
      parts <- stringi::stri_split_fixed(str = str, pattern = ";", n = 2)[[1]]
    } else if (nb_dots == 1) {
      parts <- stringi::stri_split_fixed(str = str, pattern = ".", n = 2)[[1]]
    } else if (stringi::stri_detect_regex(str = str, pattern = "[NSEWnsew].*[NSEWnsew]")) {
      # Case 2: Compact strings with two cardinal directions
      # Case 2a: cardinal direction at the beginning of the string
      if (substr(str, 1L, 1L) |> stringi::stri_detect_regex(pattern = "[NSEWnsew]")) {
        parts <- stringi::stri_split_regex(
          str = str,
          pattern = "(?<=[0-9])(?=[NSEWnsew])",
          n = 2
        )[[1]]
      } else if (substr(str, nchar(str), nchar(str)) |>
                 stringi::stri_detect_regex(pattern = "[NSEWnsew]")) {
        # Case 2a: cardinal direction at the beginning of the string
        parts <- stringi::stri_split_regex(
          str = str,
          pattern = "(?<=[NSEWnsew])(?=[0-9])",
          n = 2
        )[[1]]
      }
      if (length(parts) != 2) return(c(NA_character_, NA_character_))
    } else {
      return(c(NA_character_, NA_character_))
    }

    part1 <- stringi::stri_trim_both(str = parts[1])
    part2 <- stringi::stri_trim_both(str = parts[2])

    part1_dir <- stringi::stri_extract_first_regex(str = part1, pattern = "[NSEWnsew]") |> str_tolower()
    part2_dir <- stringi::stri_extract_first_regex(str = part2, pattern = "[NSEWnsew]") |> str_tolower()

    if (part1_dir %in% c("n", "s")) {
      lat <- convert_lat(str = part1)
      lon <- convert_lon(str = part2)
    } else if (part1_dir %in% c("e", "w")) {
      lon <- convert_lon(str = part1)
      lat <- convert_lat(str = part2)
    } else {
      return(c(NA_real_, NA_real_))
    }

    return(c(lat, lon))
  })

  mat <- do.call(rbind, res)
  return(data.frame(lat = mat[, 1], lon = mat[, 2], stringsAsFactors = FALSE))
}
