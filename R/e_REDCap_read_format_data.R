#' REDCap data, reading and formatting data
#'
#' Read the files in the fn_path and find the most recent file (last
#'  alphabetically, which is by date based on REDCap file name convention of
#'  *_yyyy-mm-dd_hhmm.*) and read and format that given the R formatting file.
#'
#' For each project file:
#' \itemize{
#'   \item Menu > Reports (Data Exports, Reports, and Stats) > All data
#'   \item All data (all records and fields), Export Data
#'   \item R Statistical Software
#'   \item Click both of the R and CSV file icons
#'   \itemize{
#'     \item a. Save to your path
#'     \item b. Name as: (default name) (fn_root)_DATA_yyyy-mm-dd_hhmm.csv, (fn_root)_R_yyyy-mm-dd_hhmm.csv
#'   }
#' }
#'
#' @param fn_path         is the location of the data, assumed to be the current path.
#' @param fn_root         the root name(s) of the file(s), typically the project name(s) (the part before _DATA_*.*)
#' @param fn_suffix_data  typically "_DATA_" to indicate this is the data
#' @param fn_suffix_R     typically "_R_" to indicate this is the R formatting file
#' @param fn_suffix_out   an added name to the outputted saved and formatted data files, maybe you want to add a date, a version number, or a another label
#' @param fn_ext_data     "*.csv" to specify the data filename extension, only csv is supported ("csv" is differnt from "CSV")
#' @param fn_ext_R        "*.r" to specify the R filename extension, ("r" is differnt from "R")
#' @param sw_return_dat   TRUE/FALSE to return the data object from the function
#' @param sw_save_RData   TRUE/FALSE to save an ".RData" file
#'
#' @return out_dat formatted data or \code{invisible(NULL)}, depending on \code{sw_return_dat}
#'
#' @import labelled
#' @importFrom utils glob2rx
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' \dontrun{
#' # dat_temp <-
#' #   e_REDCap_read_format_data(
#' #     fn_path   = "C:/Data/Project"
#' #   , fn_root   = "ProjectName"
#' #   )
#' }
e_REDCap_read_format_data <-
  function(
    fn_path       = getwd()
  , fn_root       = NULL
  , fn_suffix_data = "_DATA_"
  , fn_suffix_R   = "_R_"
  , fn_suffix_out = ""
  , fn_ext_data   = "csv"
  , fn_ext_R      = "r"
  , sw_return_dat = TRUE
  , sw_save_RData = TRUE
  ) {
  ### Obtain data from REDCap manually.
  ## Getting data:
  #     UNM REDCap
  #       https://rc.health.unm.edu/manage/
  #       pw: UNM salud pw
  # For each project file:
  #   1. Menu > Reports (Data Exports, Reports, and Stats) > All data
  #   2. All data (all records and fields), Export Data
  #   3. R Statistical Software
  #   4. Click both of the R and CSV file icons
  #     a. Save to your path
  #     b. Name as: (default name) [fn_root]_DATA_yyyy-mm-dd_hhmm.csv, [fn_root]_R_yyyy-mm-dd_hhmm.csv


  ## DEBUG
  # fn_path   = "D:/Dropbox/StatAcumen/consult/Rpackages/sandbox"
  # fn_root   = c("VCIDatabase", "PCORIDIABETESPROJECT")
  # fn_suffix_data = "_DATA_"
  # fn_suffix_R   = "_R_"
  # fn_suffix_out = ""
  # fn_ext_data = "csv"
  # fn_ext_R   = "r"
  # sw_return_dat = TRUE
  # sw_save_RData = TRUE
  #
  # dat_temp <-
  #   e_REDCap_read_format_data(
  #     fn_path   = "D:/Dropbox/StatAcumen/consult/Rpackages/sandbox"
  #   , fn_root   = c("VCIDatabase", "PCORIDIABETESPROJECT")
  #   , fn_suffix_data = "_DATA_"
  #   , fn_suffix_R   = "_R_"
  #   , fn_suffix_out = ""
  #   , fn_ext_data = "csv"
  #   , fn_ext_R   = "r"
  #   , sw_return_dat = TRUE
  #   , sw_save_RData = TRUE
  #   )
  # dat_temp <-
  #   e_REDCap_read_format_data(
  #     fn_path   = "D:/Dropbox/StatAcumen/consult/Rpackages/sandbox"
  #   , fn_root   = "VCIDatabase"
  #   )


  fn_path_org <- setwd(fn_path)

  ### Update filenames below

  fn_list_data_raw <- dir()

  ## filenames
  # original
  fn_list_in_data     <- paste0(fn_root, fn_suffix_data, "*.", fn_ext_data)
  fn_list_in_R        <- paste0(fn_root, fn_suffix_R   , "*.", fn_ext_R   )
  # cleaned, standard names
  fn_list_out_data    <- paste0(fn_root, fn_suffix_out ,  ".", fn_ext_data)
  fn_list_out_R       <- paste0(fn_root, fn_suffix_out ,  ".", fn_ext_R   )
  fn_list_out_RData   <- paste0(fn_root, fn_suffix_out ,  ".", "RData"    )

  ### REDCap Unicode replacment

  # REDCap introduces unicode which can't be used, so we:
  # 1. Define filenames as input for format file and data file
  # 2. Define filenames to write to after unicode is corrected
  # 3. Preform the unicode replacement.

  if (sw_return_dat) {
    out_dat <- list()
  }


  for (i_file in seq_along(fn_list_in_data)) {
    ## i_file = 1


    # Identify the current dat and format filenames
    #   Do a string compare and take the last one alphabetically (most recent by date/time)
    fn_current_data <- fn_list_data_raw[max(which(grepl(utils::glob2rx(fn_list_in_data[i_file]), fn_list_data_raw)))]
    fn_current_R    <- fn_list_data_raw[max(which(grepl(utils::glob2rx(fn_list_in_R   [i_file]), fn_list_data_raw)))]
    print(paste("Files: ", fn_current_data, fn_current_R))

    current_format <- readLines(fn_current_R)
    current_data <- readLines(fn_current_data)


    ## update R formatting
    # replace unicode in both files, write out new files
    #current_format <- f_replace_unicode()

    # replace Hmisc label() with labelled var_label() in the R formatting file
    current_format <- e_REDCap_replace_var_label(current_format)

    # skip extra lines at top
    line_num_start <- grep("#Setting Labels", current_format)
    writeLines(
        current_format[line_num_start:length(current_format)]
      , fn_list_out_R[i_file]
      )


    ## update data formatting
    # replace unicode in data file
    #current_data <- f_replace_unicode(readLines(fn_current_data))

    # skip extra lines at top
    line_num_start <- 1
    writeLines(
        current_data[line_num_start:length(current_data)]
      , fn_list_out_data[i_file]
      )


    # Read data, format and label the variables

    #library(Hmisc, quietly = TRUE) # needed for label() lines
    #library(labelled, quietly = TRUE) # needed for var_label() lines
    # called "data" because that's the name given from REDCap for the R format file
    data <-
      utils::read.csv(
        fn_list_out_data[i_file]
      , stringsAsFactors = FALSE
      )
    #data <- readr::read_csv(fn_list_out_data[i_file])
    # str(data)

    # apply R formatting
    source(fn_list_out_R[i_file], local = TRUE)
    # str(data)



    if (sw_return_dat) {
      out_dat[[ fn_root[i_file] ]] <-  data
    }

    if (sw_save_RData) {
      save(
          data
        , file = fn_list_out_RData[i_file]
        )
    }

  }

  # set back to original path
  setwd(fn_path_org)

  if (sw_return_dat) {
    return(out_dat)
  } else {
    invisible(NULL)
  }
} # e_REDCap_read_format_data
