extract_numbers_from_string <- function (vec) {
  # https://stackoverflow.com/questions/14543627/extracting-numbers-from-vectors-of-strings
  # pattern is by finding a set of numbers in the start and capturing them
  # This approach makes the decimal point and decimal fraction optional and allows multiple numbers to be extracted:
  # The concern about negative numbers can be address with optional perl style look-ahead:
    # EBE: removed unlist() in order to keep NAs for blank values

  out <- as.numeric(
            regmatches(vec
              , gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*"
                        , vec
                        , perl=TRUE
                        )
            )
          )

  return(out)
}
