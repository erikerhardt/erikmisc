#' A helper function for a list of ROC curves to reorder ROC objects by type (rather than target)
#'
#' @param list_roc    list of ROC curves from \code{e_plot_roc} ordered by target
#'
#' @return            list of ROC curves reorder ROC objects by type (rather than target)
#' @import dplyr
#' @export
#'
#' @examples
#' # See e_plot_roc()
#'
e_plot_roc_reorder_hierarchy <-
  function(
    list_roc  = NULL
  ) {

  # hierarchy: reorder ROC objects by type (rather than target)
  list_roc_reorder <- list()
  for (n_object in names(list_roc[[ 1 ]])) {
    ## n_object = names(list_roc[[ 1 ]])[1]
    list_roc_reorder[[ n_object ]] <- list()

    for (n_target in names(list_roc)) {
      ## n_target = names(list_roc)[1]
      list_roc_reorder[[ n_object ]][[ n_target ]] <-
        list_roc[[ n_target ]][[ n_object ]]

      if (n_object == "roc_curve_best") {
        list_roc_reorder[[ n_object ]][[ n_target ]] <-
          list_roc_reorder[[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }
      if (n_object == "roc_curve") {
        list_roc_reorder[[ n_object ]][[ n_target ]] <-
          list_roc_reorder[[ n_object ]][[ n_target ]] |>
          dplyr::mutate(
            Group = n_target
          ) |>
          dplyr::relocate(Group)
      }

    }
  }

  return(list_roc_reorder)
} # e_plot_roc_reorder_hierarchy

