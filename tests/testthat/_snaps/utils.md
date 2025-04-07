# convert_lat correctly converts latitude strings

    Code
      convert_lat("90N")
    Output
      [1] 90
    Code
      convert_lat("-90N")
    Output
      [1] -90
    Code
      convert_lat("45 30 30N")
    Output
      [1] 45.50833
    Code
      convert_lat("abcd")
    Condition
      Warning in `any_digits()`:
      no digits detected, got: abcd
    Output
      [1] NA

# convert_lon correctly converts longitude strings

    Code
      convert_lon("180E")
    Output
      [1] 180
    Code
      convert_lon("-180W")
    Output
      [1] -180
    Code
      convert_lon("45 30 30E")
    Output
      [1] 45.50833
    Code
      convert_lon("abcd")
    Condition
      Warning in `any_digits()`:
      no digits detected, got: abcd
    Output
      [1] NA

