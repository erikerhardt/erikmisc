#' Plot projection from high dimensions to 2D
#'
#' UMAP is very fast and seems to separate better than slower t-SNE.
#'
#' @param dat_plot                  data to plot
#' @param var_group                 variable name to exclude from projection, but may be used for plotting
#' @param var_color                 variable color
#' @param var_shape                 variable shape
#' @param var_facet                 variable to by which to facet wrap
#' @param text_title                text for title of plot to override default
#' @param sw_print                  T/F whether to print plot
#' @param sw_projection             Projection type: "tsne" (others to come)
#' @param n_obs_sample              number of observations to sample from data since projection calculation can be very expensive for large samples (only for tsne)
#'
#' @return          a ggplot object
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @import tsne
#' @import umap
#' @importFrom rlang sym
#' @importFrom tidyselect any_of
#' @export
#'
#' @examples
#' e_plot_projection(
#'     dat_plot                  = dat_mtcars_e |> dplyr::select(mpg, cyl, disp, hp, vs)
#'   , var_group                 = NULL
#'   , var_color                 = NULL
#'   , var_shape                 = NULL
#'   , var_facet                 = NULL
#'   , text_title                = NULL
#'   , sw_print                  = FALSE
#'   , sw_projection             = c("umap", "tsne")[1]
#'   , n_obs_sample              = NULL
#'   )
#'
#' e_plot_projection(
#'     dat_plot                  = dat_mtcars_e |> dplyr::select(mpg, cyl, disp, hp, vs)
#'   , var_group                 = "cyl"
#'   , var_color                 = "cyl"
#'   , var_shape                 = "cyl"
#'   , var_facet                 = "vs"
#'   , text_title                = NULL
#'   , sw_print                  = FALSE
#'   , sw_projection             = c("umap", "tsne")[1]
#'   , n_obs_sample              = NULL
#'   )
#'
e_plot_projection <-
  function(
    dat_plot                  = NULL
  , var_group                 = NULL
  , var_color                 = NULL
  , var_shape                 = NULL
  , var_facet                 = NULL
  , text_title                = NULL
  , sw_print                  = FALSE
  , sw_projection             = c("umap", "tsne")[1]
  , n_obs_sample              = NULL
  ) {
  # e_plot_projection(
  #     dat_plot                  = dat_mtcars_e |> dplyr::select(-model)
  #   , var_group                 = "cyl"
  #   , var_color                 = "cyl"
  #   , var_shape                 = "cyl"
  #   , var_facet                 = NULL
  #   , text_title                = NULL
  #   , sw_print                  = FALSE
  #   , sw_projection             = c("tsne")[1]
  #   , n_obs_sample              = NULL
  #   )

  # n_obs
  if ( is.null(n_obs_sample) ) {
    n_obs_sample <- nrow(dat_plot)
  }
  if ( n_obs_sample > nrow(dat_plot) ) {
    n_obs_sample <- nrow(dat_plot)
  }

  if ( n_obs_sample < nrow(dat_plot) ) {
    # t-SNE plot
    ind_sample <-
      sample.int(
        n       = nrow(dat_all_Model_ID_Predict_out_all)
      , size    = n_obs_sample
      , replace = FALSE
      ) |>
      sort()
  } else {
    ind_sample <- 1:nrow(dat_plot)
  }

  if (is.null(var_color) & !is.null(var_group)) {
    var_color <- var_group
  }
  if (is.null(var_shape) & !is.null(var_group)) {
    var_shape <- var_group
  }


  dat_projection <-
    dat_plot |>
    dplyr::slice(
      ind_sample
    ) |>
    dplyr::select(
      -tidyselect::any_of(var_group)
    ) |>
    dplyr::mutate(across(where(is.factor), as.numeric)) |>
    as.matrix()


  if (sw_projection == c("umap", "tsne")[1]) {
    out_projection <-
      umap::umap(
        d             = dat_projection
      , config        = umap.defaults
      , method        = c("naive", "umap-learn")[1]
      , preserve.seed = TRUE
      )$layout
  }

  if (sw_projection == c("umap", "tsne")[2]) {
    out_projection <-
      tsne::tsne(
        X                 = dat_projection
      , initial_config    = NULL
      , k                 = 2
      , initial_dims      = 30
      , perplexity        = 30
      , max_iter          = 1000
      , min_cost          = 0
      , epoch_callback    = NULL
      , whiten            = TRUE
      , epoch             = 100
      )
  }

  colnames(out_projection) <- c("X1", "X2")


  dat_projection <-
    dat_plot |>
    dplyr::slice(
      ind_sample
    ) |>
    dplyr::bind_cols(
      out_projection |>
      tibble::as_tibble()
    )

  p <- ggplot(dat_projection, aes(x = X1, y = X2))
  p <- p + theme_bw()
  p <- p + geom_hline(aes(yintercept = 0), colour = "black", linetype = c("none", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[2], linewidth = 0.2, alpha = 0.5)
  p <- p + geom_vline(aes(xintercept = 0), colour = "black", linetype = c("none", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[2], linewidth = 0.2, alpha = 0.5)
  if (!is.null(var_color) & !is.null(var_shape)) {
    p <- p + geom_point(aes(color = !!rlang::sym(var_color), shape = !!rlang::sym(var_shape)), alpha = 1/2)
  }
  if (!is.null(var_color) & is.null(var_shape)) {
    p <- p + geom_point(aes(color = !!rlang::sym(var_color)), alpha = 1/2)
  }
  if (is.null(var_color) & !is.null(var_shape)) {
    p <- p + geom_point(aes(shape = !!rlang::sym(var_shape)), alpha = 1/2)
  }
  if (is.null(var_color) & is.null(var_shape)) {
    p <- p + geom_point(alpha = 1/2)
  }
  if (!is.null(var_facet)) {
    p <- p + facet_wrap(as.formula(paste("~", var_facet)), drop = TRUE)
  }
  p <- p + labs(
                  title     = text_title
                , subtitle  = "t-SNE projection"
                #, x         = "x"
                #, y         = "y"
                #, caption = paste0(  "Caption 1"
                #                  , "\nCaption 2"
                #                  )
                #, colour    = "Class"
                #, shape     = "Class"
                #, linetype  = "General Health"  #"Diagnosis"
                #, fill      = "Diagnosis"
                #, tag = "A"
                )

  if (sw_print) {
    print(p)
  }

  return(p)
} # e_plot_projection

