# path
path          <- getwd()
path_original <- setwd(path)
on.exit(setwd(path_original), add = TRUE)

sw_log_file <- c(TRUE, FALSE)[1]
log_prefix  <- "log_abc"

if (sw_log_file) {
  # library(log4r)
  # Initialize logger
    # Create a new logger object with create.logger().
    out_log <-
      log4r::create.logger(
        logfile   = paste0(log_prefix, "_", format(Sys.time(), format = "%Y%m%d-%H%M%S"), ".txt")
      , level     = "INFO"
      )

      # Try logging messages with different priorities.
      # At priority level INFO, a call to debug() won't print anything.
      #log4r::debug(out_log, "A Debugging Message")
      #log4r::info (out_log, "An Info Message")
      #log4r::warn (out_log, "A Warning Message")
      #log4r::error(out_log, "An Error Message")
      #log4r::fatal(out_log, "A Fatal Error Message")

  if (file.exists(out_log$logfile)) {
    file.remove(out_log$logfile)
  }

  log4r::info(out_log, "Process BEGIN")

  log4r::info(out_log, paste0("Original path was: ", path_original))
  log4r::info(out_log, paste0("Changed  path to : ", path         ))
}


# Write input settings to a log file
if (sw_log_file) {
  log4r::info(out_log, paste0("Input parameters: START"))
  param_list <- list( path                 = path
                    , infile_advert        = infile_advert
                    , outfile_advert       = outfile_advert
                    , sw_classify_or_train = sw_classify_or_train
                    , file_classifier      = file_classifier
                    , time_variable        = time_variable
                    , attn_variable        = attn_variable
                    , sw_log_file          = sw_log_file
                    , sw_plot_classify     = sw_plot_classify
                    , parallel_cores       = parallel_cores
                    )
  cat(yaml::as.yaml(param_list), file = out_log$logfile, append = TRUE)
  log4r::info(out_log, paste0("Input parameters: END"))
}


if(sw_classify_or_train == "train") {
  if (sw_log_file) {
    log4r::info(out_log, paste0("TRAINING classifier"))
  }

  train_classifier(infile_advert, file_classifier, time_variable, attn_variable, sw_log_file, out_log)

  if (sw_log_file) {
    log4r::info(out_log, paste0("Random Forest (RF) classifier saved as: ", paste0(file_classifier, ".RData")))
  }
}


if(sw_classify_or_train == "classify") {
  if (sw_log_file) {
    log4r::info(out_log, paste0("CLASSIFYING advert using ", file_classifier))
  }

  classify_advert(infile_advert, outfile_advert, sw_classify_or_train, file_classifier, time_variable, attn_variable, sw_log_file, out_log)

  if (sw_log_file) {
    log4r::info(out_log, paste0("Classified Advert saved as: ", outfile_advert))
  }
}


# resetting path on exit
if (sw_log_file) {
  log4r::info(out_log, paste0("Returning to original path: ", path_original))
}
#setwd(path_original)

if (sw_log_file) {
  log4r::info(out_log, "Process END\n")
}

