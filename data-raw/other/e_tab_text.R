# from
# D:\Dropbox\StatAcumen\consult\Authorship\2020_UNM_ECURE_UgradResearch_NSF\report
# Erhardt_ECURE_DataAssembly_Spring2021_20210713.Rmd

## Summaries of data over all time

```{r chunk-12, results = 'asis', fig.height = 6, fig.width = 8}
f_tab_text <-
  function(
    dat_sum
  , var_names
  , text_blank = c(NA, "none", "n/a", "na")
  ) {
  ## f_tab_sum(dat_all, "Gender") %>% print(n=Inf)

  # dat_all_dup
  # , c("Treatment", "Group", var_class_tbl$Var[i_col])

  dat_sum[[ var_names[length(var_names)] ]] <-
    ifelse(
      !(
        tolower(
          dat_sum[[ var_names[length(var_names)] ]]
        ) %in%
        text_blank
      )
    , TRUE
    , NA
    )

  tab_dat_summary_temp <-
    dat_sum %>%
    # dplyr::group_by_(
    #   .dots = var_names
    # ) %>%
    dplyr::group_by(
      across(var_names)
    ) %>%
    drop_na(var_names[length(var_names)]) %>%
    dplyr::summarize(
      n = n()
    , .groups = "drop_last"
    ) %>%
    #mutate(
    #  prop = round(n / sum(n), 3)
    #) %>%
    #arrange(
    #  desc(n)
    #) %>%
    dplyr::ungroup()

  return(tab_dat_summary_temp)
}


# Summarize results in table format, both contrast estimates and p-values.
f_plot_factor_table <-
  function(
    dat_all
  , var_group = "Group"
  , var_treat = "Treatment"
  ) {
  ## var_group  = "Group"
  ## var_treat  = "Treatment"

  # Find factor columns with names starting with Q#
  names_col_factor <-
    colnames(dat_all)[which(rbind(lapply(dat_all, class)) == "factor")] %>%
    stringr::str_subset(pattern = "^Q[0-9]")

  var_class_tbl <-
    bind_rows(lapply(dat_all, class))[1,] %>%
    t() %>%
    as_tibble(
      rownames = "Var"
    ) %>%
    rename(
      class = V1
    ) %>%
    mutate(
      I_Survey = stringr::str_detect(Var, pattern = "^Q[0-9]")
    )


  # duplicate dataset and combine all courses into an "ALL" group
  dat_all_dup <-
    dat_all %>%
    bind_rows(
      dat_all %>%
      filter(
        Treatment == "ECURE"
      ) %>%
      mutate(
        Group = "_ALL-ECURE"
      )
    )
    #%>%
    #mutate(
    #  Group = Group %>% str_replace("Control", "_ALL_Control")
    #, Group = Group %>% factor()
    #)

  dat_all_dup <-
    labelled::copy_labels(
      from = dat_all
    , to = dat_all_dup
    )



  ## https://ggplot2.tidyverse.org/reference/vars.html
  wrap_by <- function(...) {
    facet_wrap(
      vars(...)
    , labeller = label_both
    #, scales    = "free_x"
    )
  }

  for (i_col in 1:nrow(var_class_tbl)) {
    ## i_col = 1
    ## i_col = 7
    ## i_col = 13

    # skip non-survey questions
    if (!var_class_tbl$I_Survey[i_col]) {
      next
    }

    plot_label_title <-
      paste0(
        var_class_tbl$Var[i_col]
      )
    plot_label_caption <-
      paste0(
        labelled::var_label(dat_all_dup[var_class_tbl$Var[i_col]] ) %>%
        stringr::str_wrap(width = 120)
      )

    #print("----------------------------------------")
    print(
      paste0(
        var_class_tbl$Var[i_col]
      , ":  "
      , labelled::var_label(dat_all_dup[[ var_class_tbl$Var[i_col] ]])
        #%>%
        #stringr::str_wrap(width = 40)
      )
    )

    if (var_class_tbl$class[i_col] == "factor") {

      tab_sum <-
        e_table_sum_freq_prop(
          dat_all_dup
        , c("Treatment", "Group", var_class_tbl$Var[i_col])
        )


      library(ggplot2)
      p <- ggplot(tab_sum, aes_string(x = var_class_tbl$Var[i_col], y = "prop"))
      p <- p + theme_bw()
      p <- p + geom_hline(yintercept = c(0), alpha = 0.15)
      #p <- p + geom_bar()
      p <- p + geom_bar(stat = "identity")
      p <- p + scale_y_continuous(labels=scales::percent)
      p <- p + coord_flip()
      p <- p + labs(  title = plot_label_title
                    #, subtitle = "Progress and Starting Current"
                    #, x = Var$label$var_fl[i_var]
                    , y = "Percent"
                    , caption = plot_label_caption
                    #, colour    = "Pulse Width"
                    #, shape     = NULL  # "Imputed"
                    #, linetype  = "Diagnosis"
                    #, fill      = "Diagnosis"
                    )
      #p + wrap_by(!!var_treat, !!var_group)
      p <- p + wrap_by(Treatment, Group)
      #p <- p + facet_grid(!trt_group ~ !var_group)
      #p <- p + theme(legend.position = "bottom")
      print(p)

      ## library(ggplot2)
      ## p <- ggplot(dat_all_dup %>% drop_na(var_class_tbl$Var[i_col]), aes_string(x = var_class_tbl$Var[i_col]))
      ## p <- p + theme_bw()
      ## p <- p + geom_hline(yintercept = c(0), alpha = 0.15)
      ## #p <- p + geom_bar()
      ## p <- p + geom_bar(aes(y = (..count..)/sum(..count..)))
      ## p <- p + scale_y_continuous(labels=scales::percent)
      ## p <- p + geom_bar()
      ## p <- p + coord_flip()
      ## p <- p + labs(  title = plot_label_title
      ##               #, subtitle = "Progress and Starting Current"
      ##               #, x = Var$label$var_fl[i_var]
      ##               , y = "Percent"
      ##               , caption = plot_label_caption
      ##               #, colour    = "Pulse Width"
      ##               #, shape     = NULL  # "Imputed"
      ##               #, linetype  = "Diagnosis"
      ##               #, fill      = "Diagnosis"
      ##               )
      ## #p + wrap_by(!!var_treat, !!var_group)
      ## p <- p + wrap_by(Treatment, Group)
      ## #p <- p + facet_grid(!trt_group ~ !var_group)
      ## #p <- p + theme(legend.position = "bottom")
      ## print(p)


      tab_sum <-
        e_table_sum_freq_prop(
          dat_all_dup
        , var_names =
            c(
              var_treat
            , var_group
            , var_class_tbl$Var[i_col]
            )
        ) %>%
        pivot_wider(
          id_cols     = var_class_tbl$Var[i_col]
        , names_from  = all_of(var_group)
        , values_from = n
        )

      tab_sum %>%
        e_table_print(sw_scale = 12, sw_kable_format = table_output) %>%
        print()
      #print(n=Inf, width=Inf)
      #%>%
      #print()

      readr::write_csv(
        tab_sum
      , file = paste0(fn_prefix, "pre_summary_", var_class_tbl$Var[i_col], ".csv")
      , na = "0"
      )


    } # factor

    if (var_class_tbl$class[i_col] == "character") {

      tab_sum <-
        f_tab_text(
          dat_sum    = dat_all_dup
        , var_names  = c("Treatment", "Group", var_class_tbl$Var[i_col])
        , text_blank = c(NA, "none", "n/a", "na")
        )

      tab_sum %>%
        e_table_print(sw_scale = 12, sw_kable_format = table_output)
        #%>%
        #print()

      readr::write_csv(
        tab_sum
      , file = paste0(fn_prefix, "pre_summary_", var_class_tbl$Var[i_col], ".csv")
      , na = "0"
      )

    } # character

  } # i_col

} # END f_plot_factor_table

f_plot_factor_table(dat_all)
```
