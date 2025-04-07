# parse_llstr

test_that("parse_llstr works", {
  aa <- parse_latlon("N04.9196, E101.345")

  expect_type(aa, "list")
  expect_equal(NCOL(aa), 2)
  expect_equal(NROW(aa), 1)
  expect_named(aa, c("lat", "lon"))
  expect_type(aa$lat, "double")
  expect_type(aa$lon, "double")

  expect_equal(aa, parse_latlon("N 04.9196, E 101.345"))
  expect_equal(aa, parse_latlon("N04.9196, E101.345"))
  expect_equal(aa, parse_latlon("N 04.9196, E 101.345"))
  expect_equal(aa, parse_latlon("N04.9196, E101.345"))
  expect_equal(aa, parse_latlon("N04.9196,E101.345"))
  expect_equal(aa, parse_latlon("N4.9196, E101.345"))
  expect_equal(aa, parse_latlon("4.9196N, 101.345E"))
  # SW
  expect_equal(aa, -parse_latlon("S 04.9196, W 101.345"))
  # no comma
  expect_equal(aa, parse_latlon("N4.9196 E101.345"))
  # no space
  expect_equal(aa, parse_latlon("N4.9196E101.345"))
  # DMS
  expect_equal(aa, parse_latlon("N04 55'10.56\", E101 20'42\""))
  expect_equal(aa, -parse_latlon("S04 55'10.56\", W101 20'42\""))
  # inverted
  expect_equal(aa, parse_latlon("E101.345, N04.9196"))
  expect_equal(aa, parse_latlon("4.9196N 101.345E"))
  expect_equal(aa, parse_latlon("4.9196N101.345E"))

  # A vector of values
  expect_type(parse_latlon(c("4 51'36\"S, 101 34'7\"W",
                             "N4 51'36\", E101 34'7\"")),
              "list")
})

test_that("parse_llstr - fails well", {
  expect_error(parse_latlon())
  expect_error(parse_latlon(mtcars), "x must be of class character")
  expect_error(parse_latlon("", mtcars))

  # error
  expect_warning(parse_latlon("N190, E45"), "not within -90")
})
