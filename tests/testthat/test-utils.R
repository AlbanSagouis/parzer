test_that("scrub", {
  expect_type(scrub, "closure")

  # degree symbol
  expect_equal(scrub("°"), "'")
  expect_equal(scrub("40° 25.0999"), "40' 25.0999")
  expect_equal(as.character(round(parse_lat("40° 25.0999"), 5)), "40.41833")
  expect_equal(as.character(round(parse_lon("110° 33.6667"), 5)), "110.56111")

  # 'masculine oridinal indicator' symbol
  # changes to single quote
  expect_equal(scrub("º"), "'")
  expect_equal(scrub("40º 25.0999"), "40' 25.0999")
  expect_equal(scrub("``º′″"), "'''''")
  expect_equal(scrub(c("N4º51′36″, E101:34′7″","N4:51′36″, E101d34′7″")),
               c("N4'51'36', E101'34'7'", "N4'51'36', E101'34'7'"))
  expect_equal(as.character(round(parse_lat("40º 25.0999"), 5)), "40.41833")
  expect_equal(as.character(round(parse_lon("110º 33.6667"), 5)), "110.56111")
})

# Test check_lat function
test_that("check_lat correctly identifies valid latitude values", {
  expect_true(check_lat(0))       # Should return TRUE for 0
  expect_true(check_lat(45))      # Should return TRUE for valid latitude
  expect_true(check_lat(-90))     # Should return TRUE for lower bound
  expect_true(check_lat(90))      # Should return TRUE for upper bound
  expect_false(check_lat(100))    # Should return FALSE for out of range value
  expect_false(check_lat(-100))   # Should return FALSE for out of range value
})

# Test check_lon function
test_that("check_lon correctly identifies valid longitude values", {
  expect_true(check_lon(0))       # Should return TRUE for 0
  expect_true(check_lon(180))     # Should return TRUE for valid longitude
  expect_true(check_lon(360))     # Should return TRUE for upper bound
  expect_false(check_lon(400))    # Should return FALSE for out of range value
  expect_false(check_lon(-200))   # Should return FALSE for out of range value
})

# Test strip_alpha function
test_that("strip_alpha removes alphabetic characters", {
  expect_equal(strip_alpha("123abc456"), "123   456")   # Should remove alphabets
  expect_equal(strip_alpha("abc"), "   ")               # Should return spaces for alphabetic string
  expect_equal(strip_alpha("1234"), "1234")             # Should leave numeric values unchanged
})

# Test digits_only function
test_that("digits_only removes non-numeric punctuation", {
  expect_equal(digits_only("12.34,56"), "12.34 56")      # Should remove commas
  expect_equal(digits_only("-12-34"), "-12-34")            # Should leave dashes
  expect_equal(digits_only("123456"), "123456")          # Should leave digits unchanged
})

# Test remove_internal_dashes function
test_that("remove_internal_dashes removes dashes after digits", {
  expect_equal(remove_internal_dashes("123-456"), "123 456") # Should replace dash with space
  expect_equal(remove_internal_dashes("123456"), "123456")   # Should leave digits unchanged
})

# Test extract_floats_from_string function
test_that("extract_floats_from_string extracts numeric values from string", {
  expect_equal(extract_floats_from_string("abc 12.34 xyz 56.78"), c(12.34, 56.78)) # Should extract floats
  expect_equal(extract_floats_from_string("12.34, 56.78"), c(12.34, 56.78))         # Should extract floats
  expect_equal(extract_floats_from_string("no numbers here"), numeric(0))            # Should return empty for no numbers
})

# Test count_direction_matches function
test_that("count_direction_matches counts valid direction matches", {
  expect_equal(count_direction_matches("N 45E", "N"), 1)  # Should find one 'N'
  expect_equal(count_direction_matches("S 45W", "W"), 1)  # Should find one 'W'
  expect_equal(count_direction_matches("N 45E, S 90W", "S"), 1)  # Should find one 'S'
  expect_equal(count_direction_matches("N 45E", "X"), 0)  # Should return 0 for non-existent direction
})

# Test extract_nsew function
test_that("extract_nsew correctly extracts the first NSEW direction", {
  expect_equal(extract_nsew("N 45E", "N"), "N")  # Should extract 'N'
  expect_equal(extract_nsew("S 45W", "S"), "S")  # Should extract 'S'
  expect_equal(extract_nsew("W 45E", "W"), "W")  # Should extract 'W'
  expect_equal(extract_nsew("E 45S", "E"), "E")  # Should extract 'E'
  expect_equal(extract_nsew("N 45E", "X"), "")   # Should return empty for no match
})

# Test str_tolower function
test_that("str_tolower converts string to lowercase", {
  expect_equal(str_tolower("TEST"), "test")      # Should convert to lowercase
  expect_equal(str_tolower("Test123"), "test123")# Should handle mixed case
})

# Test plus_minus function
test_that("plus_minus converts direction to numeric", {
  expect_equal(plus_minus("n"), 1)  # Should return 1 for 'n'
  expect_equal(plus_minus("e"), 1)  # Should return 1 for 'e'
  expect_equal(plus_minus("s"), -1) # Should return -1 for 's'
  expect_equal(plus_minus("w"), -1) # Should return -1 for 'w'
  expect_equal(plus_minus("x"), 1)  # Default to 1 for invalid input
})

# Test decimal_minute function
test_that("decimal_minute correctly converts to decimal minutes", {
  expect_equal(decimal_minute(60), 1)     # Should convert 60 to 1
  expect_equal(decimal_minute(120), 2)    # Should convert 120 to 2
  expect_equal(decimal_minute(1), 1/60)   # Should convert 1 to 1/60
})

# Test decimal_second function
test_that("decimal_second correctly converts to decimal seconds", {
  expect_equal(decimal_second(3600), 1)      # Should convert 3600 to 1
  expect_equal(decimal_second(1800), 0.5)    # Should convert 1800 to 0.5
  expect_equal(decimal_second(1), 1/3600)    # Should convert 1 to 1/3600
})

# Test any_digits function
test_that("any_digits detects digits in string", {
  expect_true(any_digits("abc 123"))  # Should return TRUE for string with digits
  expect_false(suppressWarnings(any_digits("abc xyz"))) # Should return FALSE for string without digits
  expect_warning(any_digits("abc xyz"))
})

# Test has_non_direction_letters function
test_that("has_non_direction_letters detects invalid letters", {
  expect_true(suppressWarnings(
    has_non_direction_letters("N 45X", "NSWE"))  # Should return TRUE for valid direction
  )
  expect_false(has_non_direction_letters("M ,45Q", "NSWE")) # Should return FALSE for valid direction
})

# Test has_e_with_trailing_numbers function
test_that("has_e_with_trailing_numbers detects invalid 'e' with numbers", {
  expect_true(suppressWarnings(has_e_with_trailing_numbers("10e20")))    # Should return TRUE for invalid 'e' format
  expect_false(has_e_with_trailing_numbers("10e"))      # Should return FALSE for valid 'e'
})

# Test invalid_degree_letter function
test_that("invalid_degree_letter detects incorrect degree symbols", {
  expect_true(invalid_degree_letter("12A", "NS"))    # Should return TRUE for invalid direction letter
  expect_false(invalid_degree_letter("12N", "NS"))   # Should return FALSE for valid direction letter
})

# Test is_negative function
test_that("is_negative correctly identifies negative numbers", {
  expect_true(is_negative("-12"))  # Should return TRUE for negative numbers
  expect_false(is_negative("12"))  # Should return FALSE for positive numbers
})


# Test convert_lat function
test_that("convert_lat correctly converts latitude strings", {

  # Valid cases
  expect_equal(convert_lat("45N"), 45)         # Should return 45
  expect_equal(convert_lat("45.5N"), 45.5)     # Should return 45.5
  expect_equal(convert_lat("45 30.5N"), 45 + 30.5 / 60)  # Should convert degrees and minutes
  expect_equal(convert_lat("45 30 30N"), 45 + 30 / 60 + 30 / 3600) # Should convert degrees, minutes, and seconds

  # Invalid cases
  expect_equal(suppressWarnings(convert_lat("100N")), NA_real_)  # Should return NA (out of range)
  expect_equal(suppressWarnings(convert_lat("90N")), 90)     # Should return 90.5 (upper bound)
  expect_equal(convert_lat("90S"), -90)    # Should return -90.5 (lower bound)
  expect_equal(suppressWarnings(convert_lat("180E")), NA_real_)  # Invalid latitude
  expect_equal(suppressWarnings(convert_lat("abcd")), NA_real_)  # Should return NA (invalid format)
  expect_equal(suppressWarnings(convert_lat("45N 60E")), NA_real_) # Should return NA (invalid format)

  # Snapshot test for large valid and invalid latitudes
  expect_snapshot({
    convert_lat("90N")
    convert_lat("-90N")
    convert_lat("45 30 30N")
    convert_lat("abcd")
  })
})

# Test convert_lon function
test_that("convert_lon correctly converts longitude strings", {

  # Valid cases
  expect_equal(convert_lon("45E"), 45)         # Should return 45
  expect_equal(convert_lon("45.5E"), 45.5)     # Should return 45.5
  expect_equal(convert_lon("45 30.5E"), 45 + 30.5 / 60)  # Should convert degrees and minutes
  expect_equal(convert_lon("45 30 30E"), 45 + 30 / 60 + 30 / 3600) # Should convert degrees, minutes, and seconds

  # Invalid cases
  expect_equal(suppressWarnings(convert_lon("361E")), NA_real_)  # Should return NA (out of range)
  expect_equal(convert_lon("360E"), 360)   # Upper bound for east longitude
  expect_equal(convert_lon("180W"), -180)  # Lower bound for west longitude
  expect_equal(suppressWarnings(convert_lon("90N")), NA_real_)   # Invalid longitude
  expect_equal(suppressWarnings(convert_lon("abcd")), NA_real_)  # Should return NA (invalid format)
  expect_equal(suppressWarnings(convert_lon("45E 60N")), NA_real_) # Should return NA (invalid format)

  # Snapshot test for large valid and invalid longitudes
  expect_snapshot({
    convert_lon("180E")
    convert_lon("-180W")
    convert_lon("45 30 30E")
    convert_lon("abcd")
  })
})
