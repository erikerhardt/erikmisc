#' Capture a message, warning, or error to take actions
#'
#' @param f a function that might return a message, warning, or error
#'
#' @return
#' @export
#'
#' @examples
#' e_message_capture(test_message)(1)
e_message_capture <-
  function(
    f
  ) {
  function(
    ...
  ) {

    ## https://www.r-bloggers.com/2020/10/capture-message-warnings-and-errors-from-a-r-function/

    logs <- list()

    add_log <-
      function(
        type
      , message
      ) {
        new_l   <- logs
        new_log <-
          list(
            timestamp = format(Sys.time()
          , tz        = 'UTC'
          , format    = '%Y-%m-%d %H:%M:%S')
          , type      = type
          , message   =  message
          )
        new_l[[length(new_l) + 1]] <- new_log
        logs <<- new_l
      }

    res <-
      withCallingHandlers(
        tryCatch(
          f(...)
        , error =
            function(e) {
              add_log("error", conditionMessage(e))
              NULL
            }
        )
      , warning =
          function(w) {
            add_log("warning", conditionMessage(w))
            invokeRestart("muffleWarning")
          }
      , message =
          function(m) {
            add_log("message", conditionMessage(m))
            invokeRestart("muffleMessage")
          }
      )

    list(res, logs = logs)
  }
} # e_message_capture

