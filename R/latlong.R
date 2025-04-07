# Coordinate String Conversion from C++ to R
# Based on ropensci/parzer C++ logic
# Dependencies: stringi

# ---- Helper Functions ----

check_lat <- function(lat) {
  lat >= -90 && lat <= 90
}

check_lon <- function(lon) {
  lon >= -180 && lon <= 360
}

strip_alpha <- function(x) {
  stringi::stri_replace_all_regex(str = x, pattern = "[A-Za-z]",
                                  replacement = " ",
                                  vectorize_all = TRUE)
}

digits_only <- function(x) {
  stringi::stri_replace_all_regex(str = x, pattern = "[[:punct:]&&[^.-]]",
                                  replacement = " ",
                                  vectorize_all = TRUE)
}

remove_internal_dashes <- function(x) {
  stringi::stri_replace_all_regex(x, "([0-9]+)-", "$1 ")
}

extract_floats_from_string <- function(str) {
  str <- strip_alpha(str)
  str <- digits_only(str)
  str <- remove_internal_dashes(str)
  parts <- unlist(stringi::stri_split_regex(str, "\\s+"))
  as.numeric(parts[parts != ""])
}

count_direction_matches <- function(s, pattern) {
  length(stringi::stri_extract_all_regex(str = s,
                                         pattern = pattern,
                                         omit_no_match = TRUE,
                                         simplify = TRUE))
}

extract_nsew <- function(s, pattern) {
  m <- stringi::stri_extract_first_regex(s, pattern)
  if (is.na(m)) "" else m
}

str_tolower <- function(s) {
  stringi::stri_trans_tolower(s)
}

plus_minus <- function(x) {
  switch(x,
         "n" = 1, "e" = 1,
         "s" = -1, "w" = -1,
         1)
}

decimal_minute <- function(x) x / 60
decimal_second <- function(x) x / 3600

any_digits <- function(s) {
  has_digits <- stringi::stri_detect_regex(s, "[0-9]")
  if (!isTRUE(has_digits)) warning(paste("no digits detected, got:", s))
  return(has_digits)
}

has_non_direction_letters <- function(s, pattern) {
  if (stringi::stri_detect_regex(str = s,
                                 pattern = paste0("[", pattern, "]"))) {
    warning(paste("invalid direction letter, got:", s))
    TRUE
  } else {
    FALSE
  }
}

has_e_with_trailing_numbers <- function(s) {
  found <- stringi::stri_detect_regex(s, "[0-9]+e[0-9]+")
  if (found) warning(paste("invalid characters, got:", s))
  return(found)
}

invalid_degree_letter <- function(s, allowed) {
  m <- stringi::stri_extract_first_regex(s, "^-?[0-9]{1,3}[\\s+]?[A-Za-z]")
  if (!is.na(m)) {
    !stringi::stri_detect_regex(m, paste0("[", allowed, "]"))
  } else {
    FALSE
  }
}

is_negative <- function(s) {
  stringi::stri_detect_regex(s, "^-")
}

# ---- Main Functions ----
#' Internal main function for latitude conversion
#' @param str A string with a longitude value
convert_lat <- function(str) {
  str <- str_tolower(str)

  if (
    stringi::stri_length(str) == 0 ||
    !any_digits(str) ||
    has_non_direction_letters(str, "abcefghijklmopqrtuvwxyz")
  ) {
    return(NA_real_)
  }

  if (count_direction_matches(str, "[ns]") > 1) {
    warning(paste("invalid cardinal direction, got:", str))
    return(NA_real_)
  }

  if (invalid_degree_letter(str, "nsd")) {
    warning(paste("expected single 'N|S|d' after degrees, got:", str))
    return(NA_real_)
  }

  dir <- extract_nsew(str, "[ns]")
  dir_val <- if (dir != "") plus_minus(dir) else 1
  if (is_negative(str)) dir_val <- -1

  nums <- extract_floats_from_string(str)
  if (length(nums) == 0) return(NA_real_)

  ret <- switch(as.character(length(nums)),
                `1` = abs(nums[1]),
                `2` = abs(nums[1]) + decimal_minute(nums[2]),
                `3` = abs(nums[1]) + decimal_minute(nums[2]) + decimal_second(nums[3]),
                {
                  warning(paste("invalid format, more than 3 numeric slots, got:", str))
                  return(NA_real_)
                })

  ret <- ret * dir_val

  if (!is.na(ret) && !check_lat(ret)) {
    warning(paste("not within -90/90 range, got:", str,
                  "\n  check that you did not invert lon and lat"))
    return(NA_real_)
  }

  return(ret)
}

#' Internal main function for longitude conversion
#' @param str A string with a longitude value
convert_lon <- function(str) {
  str <- str_tolower(str)

  if (
    stringi::stri_length(str) == 0 ||
    !any_digits(str) ||
    has_non_direction_letters(str, "abcfghijklmnopqrstuvxyz") ||
    has_e_with_trailing_numbers(str)
  ) {
    return(NA_real_)
  }

  if (count_direction_matches(str, "[ew]") > 1) {
    warning(paste("invalid cardinal direction, got:", str))
    return(NA_real_)
  }

  if (invalid_degree_letter(str, "ewd")) {
    warning(paste("expected single 'E|W|d' after degrees, got:", str))
    return(NA_real_)
  }

  dir <- extract_nsew(str, "[ew]")
  dir_val <- if (dir != "") plus_minus(dir) else 1
  if (is_negative(str)) dir_val <- -1

  nums <- extract_floats_from_string(str)
  if (length(nums) == 0) return(NA_real_)

  ret <- switch(as.character(length(nums)),
                `1` = abs(nums[[1]]),
                `2` = abs(nums[[1]]) + decimal_minute(nums[[2]]),
                `3` = abs(nums[[1]]) + decimal_minute(nums[[2]]) + decimal_second(nums[[3]]),
                {warning(paste("Invalid format, more than 3 numeric slots, got:",
                               str))
                  NA_real_}
  )

  ret <- ret * dir_val

  if (!is.na(ret) && !check_lon(ret)) {
    warning(paste("not within -180/360 range, got:", str))
    return(NA_real_)
  }

  return(ret)
}
