#' Read an Rmd rmarkdown file and apply enumerated code chunks
#'
#' @param fn_in  Rmd filename in.
#' @param fn_out Rmd filename out.  If NULL, then this is assigned \code{fn_in}.
#'
#' @return NULL, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' e_rmd_name_chunks(fn_in = "test_in.Rmd", fn_out = "test_out.Rmd", prefix_chunk_name = "chunk-")
#' }
e_rmd_name_chunks <-
  function(
    fn_in  = NULL
  , fn_out = NULL
  , prefix_chunk_name = "chunk-"
  ) {

  ## DEBUG
  # path <- "D:/Dropbox/StatAcumen/consult/Rpackages/RmdNameChunk/vignettes"
  # setwd(path)
  # fn_in  <- "test_in.Rmd"
  # fn_out <- "test_out.Rmd"
  # prefix_chunk_name = "chunk-"
  # library(tidyverse)
  # library(stringr)


  if(is.null(fn_in)) {
    stop(paste0("Input Rmd filename not specified: ", fn_in))
  }

  if(is.null(fn_out)) {
    paste0("Output Rmd filename not specified, assigning input filename: ", fn_in)
    fn_out <- fn_in
  }

  if(!file.exists(fn_in)) {
    stop(paste0("File does not exist: ", fn_in))
  }


  # read rmd_in file
  rmd_in <- readLines(fn_in)

  # copy to rmd_out, the updated chunk lines will be replaced later
  rmd_out <- rmd_in

  rmd_in_lower <- stringr::str_to_lower(rmd_in)

  ind_chunk_headers <-
    rmd_in_lower %>%
    # start of code chunk lines
    stringr::str_detect(pattern = stringr::fixed("```{")) %>%
    which()

  rmd_in_sub <-
    rmd_in[ind_chunk_headers]

  # break the chunk header line into 3 parts
  #   1: ```{r
  #   2: chunk name
  #   3: }  or  , [options]}
  # if there's a comma, then delete everything after the comma
  # else, delete the trailing brace "}"
  rmd_in_sub <-
    rmd_in_sub %>%
    stringr::str_split_fixed(pattern = stringr::fixed(","), n = 2)
  for (i_row in 1:nrow(rmd_in_sub)) {
    if (rmd_in_sub[i_row, 2] == "") { # no options
      rmd_in_sub[i_row, 1] <-
        rmd_in_sub[i_row, 1] %>%
        stringr::str_remove(pattern = stringr::fixed("}"))
      rmd_in_sub[i_row, 2] <- "}"
    } else {                          # options
      rmd_in_sub[i_row, 2] <- paste0(",", rmd_in_sub[i_row, 2])
    }
  }


  # strip internal whitespace
  rmd_in_sub2 <-
    rmd_in_sub[, 1] %>%
    stringr::str_replace_all(pattern = " ", replacement = "") %>%
    stringr::str_replace_all(pattern = stringr::fixed("```{R"), replacement = "```{r") %>%
    stringr::str_replace_all(pattern = stringr::fixed("```{r"), replacement = "")

  rmd_in_table <-
    data.frame(
      tick_r      = "```{r "
    , chunk_name  = rmd_in_sub2
    , options     = rmd_in_sub[, 2]
    , stringsAsFactors = FALSE
    )

  # Chunk names remain.  They are either:
  #   blank (unnamed)
  #   named (have a name other than the prefix_chunk_name prefix)
  #   previously numbered (have the prefix_chunk_name prefix)
  # Blank any with the prefix_chunk_name then renumber those.
  ind_blank <-
    rmd_in_table$chunk_name %>%
    stringr::str_detect(pattern = stringr::fixed(prefix_chunk_name)) %>%
    which()
  if (length(ind_blank)) {
    rmd_in_table$chunk_name[ind_blank] <- ""
  }

  # for all blank chunks, assign them sequentially numbered chunk names with the prefix.
  ind_to_name <-
    rmd_in_table$chunk_name %>%
    stringr::str_detect(pattern = "^$") %>%
    which()

  # number sequence
  chunk_number_seq <- 1:length(ind_to_name)
  # number of digits needed for leading 0s
  num_digits_leading_0s <-
    chunk_number_seq %>%
    max() %>%
    log10() %>%
    ceiling()
  # text numbers with leading 0s
  chunk_numbers <-
    sprintf(
      fmt = paste0("%0", num_digits_leading_0s, "d")
    , chunk_number_seq
    )
  # chunk prefix with numbers
  chunk_prefix_number <-
    paste0(
      prefix_chunk_name
    , chunk_numbers
    )

  # fill in all chunk names
  rmd_in_table$chunk_name[ind_to_name] <-
    chunk_prefix_number

  rmd_header_row <-
    apply(rmd_in_table, 1, paste0, collapse = "")

  # update header lines in rmd_out
  rmd_out[ind_chunk_headers] <-
    rmd_header_row

  # write fn_out file
  fileConn <- file(fn_out)
  writeLines(rmd_out, fileConn)
  close(fileConn)

  invisible(NULL)
}
