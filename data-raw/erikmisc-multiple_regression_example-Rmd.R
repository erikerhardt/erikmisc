---
title: "Multiple regression example"
subtitle: "`unmada` package"
author: "Erik Erhardt"
date: "`r format(Sys.time(), '%B %d, %Y')`"
output:
  html_document:
    toc: true
    number_sections: true
    toc_depth: 5
    code_folding: show
    theme: cosmo #spacelab #yeti #united #cosmo
    highlight: tango
  pdf_document:
    df_print: kable
fontsize: 12pt
geometry: margin=0.25in
always_allow_html: yes
---

<style>
/* HTML FORMATTING */
h1, .h1, h2, .h2, h3, .h3, h4, .h4, h5, .h5 {
  margin-top: 25px; /* space before each header */
  font-weight: bold; /* bold headers */
}
</style>

```{R, echo=FALSE}
# I set some GLOBAL R chunk options here.
#   (to hide this message add "echo=FALSE" to the code chunk options)

knitr::opts_chunk$set(comment = NA, message = FALSE, warning = FALSE, width = 100)
knitr::opts_chunk$set(fig.align = "center", fig.height = 4, fig.width = 6)
```

<!---
## IGNORE THIS
# Erik's compile commands in R:
  rm(list=ls())
  fn_this <- "multiple_regression_example.Rmd"
  #fn_this <- "ADA2_CL_14_ANCOVA_AbqHomePrices.Rmd"
  setwd("D:/Dropbox/UNM/teach/ADA_Redesign_2015/RPackage/unmada/vignettes")
  library(knitr)
  purl(fn_this)
  rmarkdown::render(fn_this)
## IGNORE THIS
-->

Aspects of this example:

* Use `mtcars` data since common and available with R's `datasets`.
* Use `labelled::var_label()` to label variables for automated plot labeling.
* Fit a specific regression model using `lm()`
* Use our `lm_diag_plots()` to assess diagnositics.
* Use our `lm_plot_contrasts()` to report and visualize the results of a regression model.


# Data and fit model

Prepare the environment with packages and functions.

```{r}
library(erikmisc)
library(tidyverse)
```

Read and format the data.

```{r}
# Data
dat_mtcars <-
  mtcars |>
  # move rownames to a column
  as_tibble(
    rownames = "model"
  ) |>
  # label factor levels
  mutate(
    id = 1:n()
  , cyl = cyl |> factor(levels = c(4, 6, 8), labels = c("four", "six", "eight"))
  , vs  = vs  |> factor(levels = c(0, 1), labels = c("V-shaped", "straight"))
  , am  = am  |> factor(levels = c(0, 1), labels = c("automatic", "manual"))
  )

# Use variable description in ?mtcars to label columns
dat_labels <-
  tribble(
    ~var    , ~label
  , "model" , "Model"
  , "mpg"   , "Miles/(US) gallon"
  , "cyl"   , "Number of cylinders"
  , "disp"  , "Displacement (cu.in.)"
  , "hp"    , "Gross horsepower"
  , "drat"  , "Rear axle ratio"
  , "wt"    , "Weight (1000 lbs)"
  , "qsec"  , "1/4 mile time"
  , "vs"    , "Engine"                     # (0 = V-shaped, 1 = straight)"
  , "am"    , "Transmission"               # (0 = automatic, 1 = manual)"
  , "gear"  , "Number of forward gears"
  , "carb"  , "Number of carburetors"
  )

for (i_row in 1:nrow(dat_labels)) {
  labelled::var_label(dat_mtcars[[dat_labels[["var"]][i_row] ]]) <- dat_labels[["label"]][i_row]
}
```

Specify a specific model to and fit the model.

```{r}
 # Set specific model with some interactions
form_model <-
  mpg ~ cyl + disp + hp + wt + vs + am + cyl:vs + disp:hp + hp:vs

lm_fit <-
  lm(
    formula = form_model
  , data    = dat_mtcars
  )
```

# Model diagnostic plots

In the plots below we see several issues with model fit.

```{R, fig.height = 3, fig.width = 10}
# plot diagnostics
lm_diag_plots(lm_fit, sw_plot_set = "simple")

anova(lm_fit)
summary(lm_fit)
```

IGNORING ISSUES OF MODEL FIT, we illustrate the contrasts.

# Contrasts

```{r}
fit_contrasts <-
  lm_plot_contrasts(
    fit              = lm_fit
  , dat_cont         = dat_mtcars
  , sw_print         = FALSE
  #, sw_table_in_plot = FALSE
  , sw_TWI_plot_orientation = c("tall", "wide")[2]
  )

# tables available
names(fit_contrasts$tables)
# plots available
names(fit_contrasts$plots)
```

Print all contrast tables.

* `est` are the estimates.
* `cont` are the contrasts.

## Tables

For two-variable interactions, contrasts are computed in both conditional relationships, each variable conditional on the other.

```{r}
# to print tables
fit_contrasts$tables
```

### Pretty print each table

```{r}
# print each table
fit_contrasts$tables$wt$est        |> knitr::kable()

fit_contrasts$tables$am$est        |> knitr::kable()
fit_contrasts$tables$am$cont       |> knitr::kable()

fit_contrasts$tables$`cyl:vs`$est  |> knitr::kable()
fit_contrasts$tables$`cyl:vs`$cont |> knitr::kable()

fit_contrasts$tables$`hp:vs`$est   |> knitr::kable()
fit_contrasts$tables$`hp:vs`$cont  |> knitr::kable()
```

## Plots

For two-variable interactions, both conditional relationships are plotted.

```{r, fig.height = 6, fig.width = 6}
# to print plots
fit_contrasts$plots
```

### Arranging plots for publication

Plots can then be arranged for publication.

```{r}
# selected plots for figure layout
p_list <- list()
p_list[[1]] <-
  list(
    fit_contrasts$plots$wt
  , fit_contrasts$plots$am
  , fit_contrasts$plots$`cyl:vs`[["both"]]  # both conditional relationships
  #, fit_contrasts$plots$`disp:hp`[[1]]
  #, fit_contrasts$plots$`disp:hp`[[2]]
  #, fit_contrasts$plots$`hp:vs`
  )
p_list[[2]] <-
  list(
  #  fit_contrasts$plots$wt
  #, fit_contrasts$plots$am
  #, fit_contrasts$plots$`cyl:vs`[[1]]  # both conditional relationships
  #, fit_contrasts$plots$`cyl:vs`[[2]]  # both conditional relationships
    fit_contrasts$plots$`disp:hp`[[1]]  # each one, separately
  , fit_contrasts$plots$`disp:hp`[[2]]
  , fit_contrasts$plots$`hp:vs`[["both"]]
  )
```

```{r}
# For bolder labels, remove the labs(tag=) argument and use this
## Functions
f_plot_grid_corner_label <-
  function(
    p
  , label_corner = "A"
  ) {
  # for adding labels to corners of figure panels using grid.arrange()
    # https://stackoverflow.com/questions/17576381/label-individual-panels-in-a-multi-panel-ggplot2

  p <-
    gridExtra::arrangeGrob(
      p
    , top =
        grid::textGrob(
          label_corner
        , x     = grid::unit(0, "npc")
        , y     = grid::unit(1, "npc")
        , just  = c("left", "top")
        , gp    = grid::gpar(
                    col         = "black"
                  , fontsize    = 18
                  , fontface    = "bold"
                  #, fontfamily  = "Times Roman"
                  )
        )
    )

  return(p)
}


# add plot panel grid corner labels
for (i_list in 1:length(p_list[[1]])) {
  p_list[[1]][[i_list]] <-
    f_plot_grid_corner_label(
      p_list[[1]][[i_list]]
    , label_corner = LETTERS[[i_list]]
    )
}

# add plot panel grid corner labels
for (i_list in 1:length(p_list[[2]])) {
  p_list[[2]][[i_list]] <-
    f_plot_grid_corner_label(
      p_list[[2]][[i_list]]
    , label_corner = LETTERS[[i_list]]
    )
}
```

```{r, fig.height = 10, fig.width = 8}
## Arrange in a grid
library(gridExtra)
library(grid)
lay_grid <-
  rbind(
    c(1, 2)
  , c(3, 3)
  , c(3, 3)
  , c(3, 3)
  )
p_arranged <-
  #gridExtra::grid.arrange(
  gridExtra::arrangeGrob(
    grobs         = p_list[[1]]
  , layout_matrix = lay_grid
  #, top           = "top title"
  #, bottom        = "bottom\ntitle"
  #, left          = "left label"
  #, right         = "right label"
  ) |>
  ggpubr::as_ggplot()

print(p_arranged)
```

```{r, fig.height = 8, fig.width = 8}
## Arrange in a grid
library(gridExtra)
library(grid)
lay_grid <-
  rbind(
    c(1, 2)
  , c(3, 3)
  )
p_arranged <-
  #gridExtra::grid.arrange(
  gridExtra::arrangeGrob(
    grobs         = p_list[[2]]
  , layout_matrix = lay_grid
  #, top           = "top title"
  #, bottom        = "bottom\ntitle"
  #, left          = "left label"
  #, right         = "right label"
  ) |>
  ggpubr::as_ggplot()

print(p_arranged)
```
