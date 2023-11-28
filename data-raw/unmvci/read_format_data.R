#' Updating the REDCap data into the package
#'
#' @param path is the location of the data, assumed to be the current path.  Erik will specify as \code{NULL} to update data in the R package.
#'
#' @return
#' @import labelled
#' @import readr
#' @export
#'
#' @examples
#' read_format_data(path = NULL) # for Erik to update package data
#' read_format_data()            # for others to format data in current path
read_format_data <- function(path = getwd()) {
  ### Obtain data from REDCap manually.
  ## Getting data:
  # REDCap
  #   Cisco connect
  #     Cisco AnyConnect: hscvpn.health.unm.edu, group: hsc-unm
  #       pw: UNM salud pw
  #   https://hsc-ctscapps.health.unm.edu/redcap/index.php?action=myprojects
  #     pw: UNM salud pw
  # For each project file: VCI Database
  #   1. Data Exports, Reports, and Stats
  #   2. All data (all records and fields), Export Data
  #   3. R Statistical Software
  #   4. Click both of the R and CSV file icons
  #     a. Save to: C:\Dropbox\StatAcumen\consult\Authorship\2014_GaryRosenberg_UNM_NIH_VascularDementia_MRI\VCI_RPackage\unmvci\data-raw
  #     b. Name as: (default name) VCIDatabase_DATA_2017-09-26_1256.csv


  # Erik specifies path = NULL to update package data, otherwise will reformat data in current path
  # read_format_data(path = NULL)
  if (is.null(path)) {
    path <- "C:/Dropbox/StatAcumen/consult/Authorship/2014_GaryRosenberg_UNM_NIH_VascularDementia_MRI/VCI_RPackage/unmvci/data-raw"
    path_org <- setwd(path)
    sw_save_to_package <- TRUE
  } else {
    sw_save_to_package <- FALSE
  }

  ### Update filenames below

  fn_list_data_raw <- dir()

  ## filenames
  # original
  fn_dat_list_in     <- c( "VCIDatabase_DATA_20*.csv"
                         )
  fn_format_list_in  <- c( "VCIDatabase_R_20*.r"
                         )

  # cleaned, standard names
  fn_dat_list_out    <- c( "DAT_VCI.csv"
                         )
  fn_format_list_out <- c( "DAT_VCI_format.R"
                         )

  ### REDCap Unicode replacment

  # REDCap introduces unicode which can't be used, so we:
  # 1. Define filenames as input for format file and data file
  # 2. Define filenames to write to after unicode is corrected
  # 3. Preform the unicode replacement.

  for (i_file in 1:length(fn_dat_list_in)) {

    # Identify the current dat and format filenames
    #   Do a string compare and take the last one alphabetically (most recent by date/time)
    fn_dat_current    <- fn_list_data_raw[max(which(grepl(glob2rx(fn_dat_list_in[i_file]   ), fn_list_data_raw)))]
    fn_format_current <- fn_list_data_raw[max(which(grepl(glob2rx(fn_format_list_in[i_file]), fn_list_data_raw)))]
    print(paste("Files: ", fn_dat_current, fn_format_current))

    # replace unicode in both files, write out new files
    format_current <- f_replace_unicode(readLines(fn_format_current))

    # replace Hmisc label() with labelled var_label() in the R formatting file
    format_current <- f_replace_var_label(format_current)

    # Spanish: remove categorical Spanish labels, between first quote and " / "
      # I'd like to be able to remove the Spanish and keep only the English for
      # reporting.  I've written a fairly complicated command over about 2
      # hours to do this.  A few tweaks are needed in the database for it to
      # work for all categorical response questions.
      #
      # Rules:
      #
      # 1. Each time the forward slash is used only to separate Spanish from
      # English, it needs one space before and after it.
      #
      # 2. If there's at least one "Spanish / English", all of the responses
      # need it, that includes "" (a blank should be " / "), and "2" (should be
      # "2 / 2"), etc.
      #
      # https://regex101.com/   this helped find the correct regex syntax
      # first replace [c(\"... / ] with [c(\"], then replace [\",\"... / ] with [\",\"]
      ## DEBUG
      # path <- "C:/Dropbox/StatAcumen/consult/Authorship/2014_GaryRosenberg_UNM_NIH_VascularDementia_MRI/VCI_RPackage/unmvci/data-raw"
      # path_org <- setwd(path)
      # format_current <- readLines("DAT_VCI_format.R")
      # str <- readLines("DAT_VCI_format.R")[740]
      # gsub('c[(]\\\"[^/]+/ ', 'c(\\\"', str)     # c(\"
      # gsub('\\\",\\\"[^/]+/ ', '\\\",\\\"', str) # \",\"
      # gsub('\\\",\\\"[^/]+/ ', '\\\",\\\"', gsub('c[(]\\\"[^/]+/ ', 'c(\\\"', str))
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # EBE - put this next line back in after corrections to the formatting fields in RedCAP
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #format_current <- gsub('\\\",\\\"[^/]+/ ', '\\\",\\\"', gsub('c[(]\\\"[^/]+/ ', 'c(\\\"', format_current))

    # skip extra lines at top
    start.line <- grep("#Setting Labels", format_current)
    writeLines(format_current[start.line:length(format_current)], fn_format_list_out[i_file])

    # replace unicode in data file
    dat_current <- f_replace_unicode(readLines(fn_dat_current))
    # skip extra lines at top
    start.line <- 1
    writeLines(dat_current[start.line:length(dat_current)], fn_dat_list_out[i_file])


    ### Read data, format and label the variables

    #library(Hmisc, quietly = TRUE) # needed for label() lines
    #library(labelled, quietly = TRUE) # needed for var_label() lines
    # called "data" because that's the name given from REDCap for the R format file
    data <- read.csv(fn_dat_list_out[i_file], stringsAsFactors = FALSE)
    #data <- readr::read_csv(fn_dat_list_out[i_file])



    ## 5/22/2018
    # comment all the "label" lines since we don't use this and it causes issues
    # fn_format_list_out[i_file] |>
    #   readLines() |>
    #   stringi::stri_replace_all_fixed("label(", "## label(") |>
    #   writeLines(fn_format_list_out[i_file])

    # apply R formatting
    source(fn_format_list_out[i_file], local = TRUE)

    #data <- var_clear_labels(data)

    if (i_file == 1) {
      dat_vci <- data
      rm(data)

      # additional formatting for variables
      dat_vci <- additional_variable_formatting_vci(dat_vci)

    }
    # if (i_file == 2) {
    #   dat_Elig <- data
    #   rm(data)
    # }
    # if (i_file == 3) {
    #   dat_vci <- data
    #   rm(data)
    # }
  }

  ### Save data into package
  if (sw_save_to_package) {
    usethis::use_data( dat_vci
                      #, dat_Elig
                      #, dat_vci
                      #, pkg = path
                      , overwrite = TRUE
                      )
    # return to original path
    setwd(path_org)
  } else {
    save(dat_vci, file = "dat_vci.RData")
  }


  invisible(NULL)
}
