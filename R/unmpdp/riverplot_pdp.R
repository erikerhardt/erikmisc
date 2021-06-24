#' Produces a consort diagram called from \code{consort_diagram()}
#'
#' @param title_legend
#' @param n_patients
#' @param rp_plot_area to scale social support relative to patients, it should be n_ss / n_p
#' @param report_date prints the date for the report, if specified; otherwise, prints the current date/time on the figure.
#'
#' @return
#' @import riverplot
#' @import lubridate

#' @export
#'
#' @examples
riverplot_pdp <- function(n_patients, title_legend = "Patients", rp_plot_area = 1, report_date = NULL) {

  ## riverplot formatting
  #library(riverplot)

  ## color examples
  # colors()[grep("green", colors())]
  # colors()[grep("orange", colors())]
  # colors()[grep("red", colors())]

  my_colors <- list(green  = "lightgreen"
                  , orange = "orange"
                  , red    = "palevioletred2"
                    )

  ## Nodes
  # order of nodes is from bottom up for each x position
  ## Update labels with values
  nodes <- n_patients
  nodes$labels <- paste0(nodes$labels, ": ", n_patients$n)
  nodes$labels[nodes$ID == "LeftMargin"] <- ""
  #nodes <- subset(nodes, select = c(ID, x, labels))


  ## Edges
  # order of edges is from bottom up for each x position
  edges <- list( RecTotal      = list(RecInterested      = n_patients[(n_patients$ID == "RecInterested"      ), "n"]
                                    , RecNotInterested   = n_patients[(n_patients$ID == "RecNotInterested"   ), "n"]
                                    )
               , RecInterested = list(Eligible           = n_patients[(n_patients$ID == "Eligible"           ), "n"]
                                    , EligInProcess      = n_patients[(n_patients$ID == "EligInProcess"      ), "n"]
                                    , EligNotParticpate  = n_patients[(n_patients$ID == "EligNotParticpate"  ), "n"]
                                    , EligNot            = n_patients[(n_patients$ID == "EligNot"            ), "n"]
                                    )
               , Eligible     = list(Baseline            = n_patients[(n_patients$ID == "Baseline"           ), "n"]
                                   , BaselineInProcess   = n_patients[(n_patients$ID == "BaselineInProcess"  ), "n"]
                                    )
               , Baseline     = list(Followup03          = n_patients[(n_patients$ID == "Followup03"         ), "n"]
                                   , Followup03InProcess = n_patients[(n_patients$ID == "Followup03InProcess"), "n"]
                                    )
               , Followup03   = list(Followup06          = n_patients[(n_patients$ID == "Followup06"         ), "n"]
                                   , Followup06InProcess = n_patients[(n_patients$ID == "Followup06InProcess"), "n"]
                                    )
               , Followup06   = list(Followup12          = n_patients[(n_patients$ID == "Followup12"         ), "n"]
                                   , Followup12InProcess = n_patients[(n_patients$ID == "Followup12InProcess"), "n"]
                                    )
               )

  # remove label for SS Eligible line
  if (title_legend == "Social Support") {
    nodes$labels[nodes$ID == "Eligible"] <- ""
  }


  # remove NA nodes and edges
  nodes <- subset(nodes, !is.na(n))
    # How to properly omit NAs from a nested list?
    # https://stackoverflow.com/questions/7921219/how-to-properly-omit-nas-from-a-nested-list
  edges <- lapply(edges, Filter, f = Negate(is.na))
    # Remove empty elements from list with character(0)
    # https://stackoverflow.com/questions/19023446/remove-empty-elements-from-list-with-character0
  edges <- edges[lapply(edges, length) > 0] ## you can use sapply,rapply

  ## Make riverplot
  r <- riverplot::makeRiver( nodes
                  , edges
                  , node_styles = list(
                      RecTotal            = list(col = my_colors$green)
                    , RecInterested       = list(col = my_colors$green)
                    , RecNotInterested    = list(col = my_colors$red)
                    , Eligible            = list(col = my_colors$green)
                    , EligInProcess       = list(col = my_colors$orange)
                    , EligNotParticpate   = list(col = my_colors$red)
                    , EligNot             = list(col = my_colors$red)
                    , Baseline            = list(col = my_colors$green)
                    , BaselineInProcess   = list(col = my_colors$orange)
                    , Followup03          = list(col = my_colors$green)
                    , Followup03InProcess = list(col = my_colors$orange)
                    , Followup06          = list(col = my_colors$green)
                    , Followup06InProcess = list(col = my_colors$orange)
                    , Followup12          = list(col = my_colors$green)
                    , Followup12InProcess = list(col = my_colors$orange)
                  )
                )

  par(mar = c(0.5, 0.5, 3, 0.5))  # c(bottom, left, top, right)

  riverplot::riverplot(r
      #, main = "Patients"
      #, mar = c(5,5,5,5)
      , gravity = "bottom"
      , plot_area = rp_plot_area ## to scale social support relative to patients
      , node_margin = 0.01
      , srt = 0             # rotate labels
      , xscale = 1.0
      , yscale = "auto"
      )
  legend( "bottomleft"  #x = 0, y = 1.3  # "top" #"topright"
        #, title = title_legend
        , cex = 1  #1.2
        , inset = 0.00
        , legend = c("Stop", "In Process", "Continue")
        , fill = c(my_colors$red, my_colors$orange, my_colors$green)
        , horiz = FALSE
        , border = "white"  # no border around colored boxes
        #, bty = "n"         # no border around legend
        , bg = "gray95"
        )
  #library(lubridate)
  #text(0, 0.1, title_legend, pos = 4, offset = 0, col="black", cex = 1.2)
  #text(1, 0, paste("PDP", lubridate::now()), pos = 4, offset = 1.75, col="black", cex = 0.8)
  text(0.5, 1, title_legend, pos = 3, offset = 2, col="black", cex = 1.2)

  # if report_date is specified, use that; otherwise, use the current date/time.
  if (is.null(report_date)) {
    report_date <- lubridate::now()
  }

  #text(1, 0.1, title_legend, pos = 2, offset = 2, col="black", cex = 1.2)
  #text(1, 0, paste("PDP", lubridate::now()), pos = 2, offset = 2, col="black", cex = 0.8) # bottom-right
  text(1, 0.93, paste("PDP", report_date), pos = 2, offset = 2, col="black", cex = 0.8) # top-right

  invisible(NULL)
}
