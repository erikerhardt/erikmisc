#' Updating the REDCap data into the package
#'
#' @param path is the location of the data, assumed to be the current path.  Erik will specify as \code{NULL} to update data in the R package.
#' @sw_return Switch to return data or not
#'
#' @return
#' @import labelled
#' @import readr
#' @export
#'
#' @examples
#' read_format_data(path = NULL) # for Erik to update package data
#' read_format_data()            # for others to format data in current path
read_format_data <- function(path = getwd(), sw_return = FALSE) {
  ### Obtain data from REDCap manually.
  ## Getting data:
  # REDCap
  #   Cisco connect
  #     Cisco AnyConnect: hscvpn.health.unm.edu, group: hsc-unm
  #       pw: UNM salud pw
  #   https://hsc-ctscapps.health.unm.edu/redcap/index.php?action=myprojects
  #     pw: UNM salud pw
  # For each project file: Pre Screening, Eligibility, and PCORI DIABETES PROJECT
  #   1. Data Exports, Reports, and Stats
  #   2. All data (all records and fields), Export Data
  #   3. R Statistical Software
  #   4. Click both of the R and CSV file icons
  #     a. Save to: C:\Dropbox\StatAcumen\consult\Authorship\2016_JanetPageReeves_PCORI_Diabetes\PDP_RPackage\unmpdp\data-raw
  #     b. Name as: (default name) PCORIDIABETESPROJECT_DATA_2017-06-16_1012.csv


  # Erik specifies path = NULL to update package data, otherwise will reformat data in current path
  # read_format_data(path = NULL)
  if (is.null(path)) {
    path <- "/Dropbox/StatAcumen/consult/Authorship/2016_JanetPageReeves_PCORI_Diabetes/PDP_RPackage/unmpdp/data-raw"
    path_org <- setwd(path)
    sw_save_to_package <- TRUE
  } else {
    sw_save_to_package <- FALSE
  }

  ### Update filenames below

  fn_list_data_raw <- dir()

  ## filenames
  # original
  fn_dat_list_in     <- c( "PCORIPreScreening_DATA_20*.csv"
                         , "PCORIEligibilityScre_DATA_20*.csv"
                         , "PCORIDIABETESPROJECT_DATA_20*.csv"
                         )
  fn_format_list_in  <- c( "PCORIPreScreening_R_20*.r"
                         , "PCORIEligibilityScre_R_20*.r"
                         , "PCORIDIABETESPROJECT_R_20*.r"
                         )

  # cleaned, standard names
  fn_dat_list_out    <- c( "DAT_PreScr.csv"
                         , "DAT_Elig.csv"
                         , "DAT_PDP.csv"
                         )
  fn_format_list_out <- c( "DAT_PreScr_format.R"
                         , "DAT_Elig_format.R"
                         , "DAT_PDP_format.R"
                         )

  ### REDCap Unicode replacment

  # REDCap introduces unicode which can't be used, so we:
  # 1. Define filenames as input for format file and data file
  # 2. Define filenames to write to after unicode is corrected
  # 3. Preform the unicode replacement.

  for (i_file in seq_along(fn_dat_list_in)) {

    # Identify the current dat and format filenames
    #   Do a string compare and take the last one alphabetically (most recent by date/time)
    fn_dat_current    <- fn_list_data_raw[max(which(grepl(glob2rx(fn_dat_list_in[i_file]   ), fn_list_data_raw)))]
    fn_format_current <- fn_list_data_raw[max(which(grepl(glob2rx(fn_format_list_in[i_file]), fn_list_data_raw)))]
    print(paste("Files: ", fn_dat_current, fn_format_current))

    # replace unicode in both files, write out new files
    format_current <- f_replace_unicode(readLines(fn_format_current))

    # replace Hmisc label() with labelled var_label() in the R formatting file
    format_current <- f_replace_var_label(format_current)


    # Spanish corrections in PDP
    if (i_file == 3) {  # PDP file

      text_correct <-
        tribble(
          ~old, ~new
        , "Baseline appt. / Base"      , "Base / Baseline appt."
        , "3 Months appt. / 3 Meses"   , "3 Meses / 3 Months appt."
        , "6 Months appt./ 6 Meses"    , "6 Meses / 6 Months appt."
        , "12 Months appt. / 12 Meses" , "12 Meses / 12 Months appt."
        )


      for (i_row in 1:nrow(text_correct)) {
        format_current <-
          stringi::stri_replace_all_fixed(
            format_current
          , text_correct[i_row, 1]
          , text_correct[i_row, 2]
          )
      }

    }

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
      # path <- "C:/Dropbox/StatAcumen/consult/Authorship/2016_JanetPageReeves_PCORI_Diabetes/PDP_RPackage/unmpdp/data-raw"
      # path_org <- setwd(path)
      # format_current <- readLines("DAT_PDP_format.R")
      # str <- readLines("DAT_PDP_format.R")[740]
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

  }


  ### Read data, format and label the variables
  for (i_file in seq_along(fn_dat_list_in)) {

    #library(Hmisc, quietly = TRUE) # needed for label() lines
    #library(labelled, quietly = TRUE) # needed for var_label() lines
    # called "data" because that's the name given from REDCap for the R format file
    data <- read.csv(fn_dat_list_out[i_file], stringsAsFactors = FALSE)
    #data <- readr::read_csv(fn_dat_list_out[i_file])



    ## 5/22/2018
    # comment all the "label" lines since we don't use this and it causes issues
    # fn_format_list_out[i_file] %>%
    #   readLines() %>%
    #   stringi::stri_replace_all_fixed("label(", "## label(") %>%
    #   writeLines(fn_format_list_out[i_file])

    # apply R formatting
    source(fn_format_list_out[i_file], local = TRUE)

    #data <- var_clear_labels(data)

    if (i_file == 3) {  # PDP file

      var_factor_skip <-
        c(
          "redcap_event_name"
        )

      ## English only
      for (i_var in 1:ncol(data)) {

        # "choice=" variables have "S / E (choice=S / E)", so there are two sections of English to remove.
          # i_var = 3   # no choice
          # i_var = 31  # choice

        # if no label, then is returned as "logical(0)" with 0 length, so set to FALSE
        is_var_choice <-
          stringr::str_detect(labelled::var_label(data[[i_var]]), fixed("(choice="))
        if (length(is_var_choice) == 0) {
          is_var_choice <- FALSE
        }

        if(is_var_choice) {
          # if choice variable, then need to take apart and put back together

          labs_split <-
            labelled::var_label(data[[i_var]]) %>%
            as.character() %>%
            stringr::str_split(
              pattern = "choice="
            #, n = 2
            , simplify = TRUE
            ) %>%
            stringr::str_trim()

          labelled::var_label(data[[i_var]]) <-
            c(
              labs_split[1] %>% f_split_Spanish_English(., sw_variable_factor = "variable", language = "English")
            , labs_split[2] %>% f_split_Spanish_English(., sw_variable_factor = "variable", language = "English")
            ) %>%
            paste(collapse = "choice=")

        } else {
          # not choice variable
          labelled::var_label(data[[i_var]]) <-
            labelled::var_label(data[[i_var]]) %>% as.character() %>%
            f_split_Spanish_English(., sw_variable_factor = "variable", language = "English")
        } # end "choice="

        # factor labels
        if(is.factor(data[[i_var]])) {
          # skip some factors
          if(names(data)[[i_var]] %in% var_factor_skip) {
            next
          }

          levels(data[[i_var]]) <-
            levels(data[[i_var]]) %>%
            f_split_Spanish_English(., sw_variable_factor = "factor", language = "English")
        }
      }

      # ## Find variable label parsing issues
      #   # need to run manually, doesn't work when knitting
      #
      # ## capture all the output to a file.
      # fn_sink <- file("VariableLabelParsing.txt", open = "wt")
      # sink(fn_sink)
      #
      # print("== Variable label English parsing issues ==================================")
      # for (i_var in 1:ncol(data)) {
      #   labs <- labelled::var_label(data[[i_var]]) %>% as.character()
      #   if (length(labs) == 0) { next }
      #
      #   junk <- f_split_Spanish_English(labs, "variable", "English", debug=TRUE)
      #   if (junk == "ERROR") {
      #     print(paste(i_var, names(data)[i_var], "     ", labs))
      #   }
      # }
      #
      # ## revert output back to the console -- only then access the file!
      # sink(type = "message")
      # close(fn_sink)
    }


    if (i_file == 1) {
      dat_PreScr <- data
      rm(data)
    }
    if (i_file == 2) {
      dat_Elig <- data
      rm(data)
    }
    if (i_file == 3) {
      dat_pdp <- data
      rm(data)
    }
  }

  # # clear Hmisc labels, this causes errors because the class is not simple later on
  # dat_PreScr  <- var_clear_labels(dat_PreScr )
  # dat_Elig    <- var_clear_labels(dat_Elig   )
  # dat_pdp     <- var_clear_labels(dat_pdp    )

  ### Save data into package
  if (sw_save_to_package) {
    usethis::use_data(  dat_PreScr
                      , dat_Elig
                      , dat_pdp
                      #, pkg = path
                      , overwrite = TRUE
                      )
    # return to original path
    setwd(path_org)
  } else {
    save(dat_PreScr, file = "dat_PreScr.RData")
    save(dat_Elig  , file = "dat_Elig.RData")
    save(dat_pdp   , file = "dat_pdp.RData")
  }

  if (sw_return) {
    dat_all <-
      list(
        dat_PreScr = dat_PreScr
      , dat_Elig   = dat_Elig
      , dat_pdp    = dat_pdp
      )
    return(dat_all)
  }
  invisible(NULL)
}
