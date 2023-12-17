#' Initialize and print into a log file and to the console
#'
#' @param log_text    text string to print
#' @param log_obj     log_obj object that includes filename, console function, and \code{time_start}
#' @param i_level     Numbers 1 to 5 corresponding to "DEBUG", "INFO", "WARN", "ERROR", "FATAL" from \code{log4r::loglevel()}s
#' @param sw_init     TRUE to initilize log file and console logging, FALSE to print text
#' @param out_path    out path for log file
#' @param file_prefix filename prefix for log file
#'
#' @return log_obj or NULL
#' @importFrom log4r create.logger
#' @importFrom log4r console_appender
#' @importFrom log4r levellog
#' @importFrom log4r default_log_layout
#' @importFrom lubridate duration
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # initialize log file
#' log_obj <-
#'   e_log_write(
#'     sw_init     = TRUE
#'   , out_path    = "."
#'   , file_prefix = "out"
#'   )
#' # write log entry
#' e_log_write(
#'     log_text    = "Write this message"
#'   , log_obj     = log_obj
#'   , i_level     = 2
#'   )
#'
#' }
e_log_write <-
  function(
    log_text    = NULL
  , log_obj     = log_obj
  , i_level     = 2
  , sw_init     = c(FALSE, TRUE)[1]
  , out_path    = "."
  , file_prefix = "out"
  ) {

  # initialize log file and console logger
  if (sw_init) {

    dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

    fn_name <-
      file.path(
        out_path
      , paste0(
          file_prefix
        , "__LOG__"
        , format(Sys.time(), format = "%Y%m%d-%H%M%S")
        , ".txt"
        )
      )

    log_obj <- list()
    log_obj[[ "file" ]] <-
        log4r::create.logger(
          logfile   = fn_name
        , level     = "INFO"
        )
    log_obj[[ "console" ]] <-
        log4r::console_appender(
          layout = log4r::default_log_layout()
        )
    log_obj[[ "time_start" ]] <- proc.time()

    return(log_obj)
  }

  # set level
  this_level <- c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")[i_level]

  # text with time
  text_to_write <-
    paste0(
      round(lubridate::duration((proc.time() - log_obj[[ "time_start" ]])["elapsed"], units="seconds"), 2) |> as.character()
    , ":  "
    , log_text
    )

  # print to console
  log_obj[[ "console" ]](
    level = this_level
  , text_to_write
  )

  # write to log file
  log4r::levellog(
    logger = log_obj[[ "file" ]]
  , level  = this_level
  , text_to_write
  )

  # report R warning or error for higher-level messages
  if(this_level %in% c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")[3:5]) {
    warning(text_to_write)
  }
  #if(this_level %in% c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")[4:5]) {
  #  stop(text_to_write)
  #}

  invisible(NULL)
} # e_log_write
