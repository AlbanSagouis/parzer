assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "),
           call. = FALSE
      )
    }
  }
}

lint_inputs <- function(lon = NULL, lat = NULL, format) {
  assert(lon, "character")
  assert(lat, "character")
  assert(format, "character")
}

# scrub <- function(x) gsub("[^A-Za-z0-9\\.\\ ,'-]|d|g", "'", x)
scrub <- function(x) stringi::stri_replace_all_regex(
  str = x,
  pattern = "[^A-Za-z0-9\\.\\ ,'-]|d|g",
  replacement = "'",
  vectorize_all = TRUE)

stop_form <- function() stop("format handling not ready yet")

