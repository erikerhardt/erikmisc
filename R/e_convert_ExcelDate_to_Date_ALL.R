## Convert Excel Dates

#' Convert Excel date to Date
#'
#' Differences between Windows and Mac Serial number date systems
#'   \itemize{
#'   \item <https://support.microsoft.com/en-us/office/date-systems-in-excel-e7fe7167-48a9-4b96-bb53-5612a800b487#ID0EAACAAA=Windows>
#'     \itemize{
#'     \item Excel supports two date systems, the 1900 date system and the 1904 date system.
#'     \item Each date system uses a unique starting date from which all other workbook dates are calculated.
#'     \item All versions of Excel for Windows calculate dates based on the 1900 date system.
#'     \item Excel 2008 for Mac and earlier Excel for Mac versions calculate dates based on the 1904 date system.
#'     \item Excel 2016 for Mac and Excel for Mac 2011 use the 1900 date system, which guarantees date compatibility with Excel for Windows.
#'     }
#'   \item For example, July 5, 2011, can have two different serial numbers, as follows:
#'     \tabular{ll}{
#'     Date System  \tab  Serial number \cr
#'     1900         \tab  40729         \cr
#'     1904         \tab  39267         \cr
#'     }
#'   }
#'
#' @param   dates   list of integers
#' @param   origin  an origin date should correspond to your operating system and version of Excel
#'
#' @return  dates   list of Dates
#' @export
#'
#' @examples
#' dates <- 42917:42919
#' e_convert_ExcelDate_to_Date(dates)
e_convert_ExcelDate_to_Date <-
  function(
    dates
  , origin = "1899-12-30"
  ) {

  as.Date(
    dates
  , origin = origin     # using 1900 Date system
  , tz = "UTC"
  ) |>
  return()
}

#' Convert Date to Excel date
#'
#' Differences between Windows and Mac Serial number date systems
#'   \itemize{
#'   \item <https://support.microsoft.com/en-us/office/date-systems-in-excel-e7fe7167-48a9-4b96-bb53-5612a800b487#ID0EAACAAA=Windows>
#'     \itemize{
#'     \item Excel supports two date systems, the 1900 date system and the 1904 date system.
#'     \item Each date system uses a unique starting date from which all other workbook dates are calculated.
#'     \item All versions of Excel for Windows calculate dates based on the 1900 date system.
#'     \item Excel 2008 for Mac and earlier Excel for Mac versions calculate dates based on the 1904 date system.
#'     \item Excel 2016 for Mac and Excel for Mac 2011 use the 1900 date system, which guarantees date compatibility with Excel for Windows.
#'     }
#'   \item For example, July 5, 2011, can have two different serial numbers, as follows:
#'     \tabular{ll}{
#'     Date System  \tab  Serial number \cr
#'     1900         \tab  40729         \cr
#'     1904         \tab  39267         \cr
#'     }
#'   }
#'
#' @param   dates   list of Dates
#' @param   origin  an origin date should correspond to your operating system and version of Excel
#'
#' @return  dates   list of integers
#' @export
#'
#' @examples
#' dates <- c("2017-07-01", "2017-07-02", "2017-07-03")
#' e_convert_Date_to_ExcelDate(dates)
e_convert_Date_to_ExcelDate <-
  function(
    dates
  , origin = "1899-12-30"
  ) {

  as.numeric(
    as.Date(dates) -
    as.Date(
      0
    , origin = origin   # using 1900 Date system
    , tz = "UTC"
    )
  ) |>
  return()
}

#' Convert Excel datetime to Datetime
#'
#' Differences between Windows and Mac Serial number date systems
#'   \itemize{
#'   \item <https://support.microsoft.com/en-us/office/date-systems-in-excel-e7fe7167-48a9-4b96-bb53-5612a800b487#ID0EAACAAA=Windows>
#'     \itemize{
#'     \item Excel supports two date systems, the 1900 date system and the 1904 date system.
#'     \item Each date system uses a unique starting date from which all other workbook dates are calculated.
#'     \item All versions of Excel for Windows calculate dates based on the 1900 date system.
#'     \item Excel 2008 for Mac and earlier Excel for Mac versions calculate dates based on the 1904 date system.
#'     \item Excel 2016 for Mac and Excel for Mac 2011 use the 1900 date system, which guarantees date compatibility with Excel for Windows.
#'     }
#'   \item For example, July 5, 2011, can have two different serial numbers, as follows:
#'     \tabular{ll}{
#'     Date System  \tab  Serial number \cr
#'     1900         \tab  40729         \cr
#'     1904         \tab  39267         \cr
#'     }
#'   }
#'
#' @param   datetimes   list of numbers
#' @param   origin      an origin date should correspond to your operating system and version of Excel
#'
#' @return  datetimes   list of Datetimes
#' @export
#'
#' @examples
#' datetimes = c(40606.25, 40613.32986, 40615.32986)
#' e_convert_ExcelDatetime_to_Datetime(datetimes)
e_convert_ExcelDatetime_to_Datetime <-
  function(
    datetimes
  , origin = "1899-12-30"
  ) {

  as.POSIXct(
    datetimes * (60 * 60 * 24)
  , origin = origin     # using 1900 Date system
  , tz = "UTC"
  ) |>
  return()
}

#' Convert Datetime to Excel datetime
#'
#' Differences between Windows and Mac Serial number date systems
#'   \itemize{
#'   \item <https://support.microsoft.com/en-us/office/date-systems-in-excel-e7fe7167-48a9-4b96-bb53-5612a800b487#ID0EAACAAA=Windows>
#'     \itemize{
#'     \item Excel supports two date systems, the 1900 date system and the 1904 date system.
#'     \item Each date system uses a unique starting date from which all other workbook dates are calculated.
#'     \item All versions of Excel for Windows calculate dates based on the 1900 date system.
#'     \item Excel 2008 for Mac and earlier Excel for Mac versions calculate dates based on the 1904 date system.
#'     \item Excel 2016 for Mac and Excel for Mac 2011 use the 1900 date system, which guarantees date compatibility with Excel for Windows.
#'     }
#'   \item For example, July 5, 2011, can have two different serial numbers, as follows:
#'     \tabular{ll}{
#'     Date System  \tab  Serial number \cr
#'     1900         \tab  40729         \cr
#'     1904         \tab  39267         \cr
#'     }
#'   }
#'
#' @param   datetimes   list of Datetimes
#' @param   origin      an origin datetime should correspond to your operating system and version of Excel
#'
#' @return  datetimes   list of numbers
#' @export
#'
#' @examples
#' datetimes = c("2011-03-04 06:00:00 UTC", "2011-03-11 07:54:59 UTC", "2011-03-13 07:54:59 UTC")
#' e_convert_Datetime_to_ExcelDatetime(datetimes)
e_convert_Datetime_to_ExcelDatetime <-
  function(
    datetimes
  , origin = "1899-12-30 00:00:00 UTC"
  ) {

  as.numeric(
    as.POSIXct(datetimes, tz = "UTC") -
    as.POSIXct(
      0
    , origin = origin   # using 1900 Date system
    , tz = "UTC"
    )
  ) |>
  return()
}


