#' Plotting residual diagnostics for an lm() object.
#'
#' @param fit               linear model object returned by lm()
#' @param rc_mfrow          number of rows and columns for the graphic plot, default is c(1, 3); use "NA" for a single plot with 3 columns
#' @param which_plot        default plot numbers for lm()
#' @param n_outliers        number to identify in plots from lm() and qqPlot()
#' @param sw_qqplot         T/F for whether to show the QQ-plot
#' @param sw_boxcox         T/F for whether to show Box-Cox transformation
#' @param sw_constant_var   T/F for whether to assess constant variance
#' @param sw_collinearity   T/F for whether to assess multicollinearity between predictor variables
#' @param sw_order_of_data  T/F for whether to show residuals by order of data
#' @param sw_addedvar       T/F for whether to show added-variables plot
#' @param sw_plot_set       NULL to accept other plot options, or "simple" to exclude boxcox, constant var, collinearity, order of data, and added-variable plots. "simpleAV" to add back in the added-variable plots.  "all" includes all possible plots in this function.
#'
#' @return NULL, invisibly
#' @importFrom car avPlots
#' @importFrom car bcPower
#' @importFrom car boxCox
#' @importFrom car ncvTest
#' @importFrom car qqPlot
#' @importFrom car spreadLevelPlot
#' @importFrom car vif
#' @importFrom stats IQR
#' @importFrom stats smooth.spline
#' @importFrom stats supsmu
#' @importFrom stats formula
#' @importFrom graphics dotchart
#' @importFrom graphics lines
#' @importFrom nortest ad.test
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ cyl + disp + hp + gear, data = datasets::mtcars)
#' e_plot_lm_diagostics(fit)
#' mod <- stats::formula(mpg ~ cyl + disp + hp + gear)
#' fit <- lm(mod, data = datasets::mtcars)
#' e_plot_lm_diagostics(fit)
e_plot_lm_diagostics <-
  function(
    fit              = NULL
  , rc_mfrow         = c(1, 3)
  , which_plot       = c(4, 6, 1)
  , n_outliers       = 3
  , sw_qqplot        = TRUE
  , sw_boxcox        = TRUE
  , sw_constant_var  = TRUE
  , sw_collinearity  = TRUE
  , sw_order_of_data = TRUE
  , sw_addedvar      = TRUE
  , sw_plot_set      = c(NA, "simple", "simpleAV", "all")[1]
  ) {
  # main version is in ./ADA2/notes, copy to ./worksheet and ./homework

  ### Function arguments
  ## fit               linear model object returned by lm()
  ## rc_mfrow          number of rows and columns for the graphic plot, default is c(1, 3); use "NA" for a single plot with 3 columns
  ## which_plot        default plot numbers for lm()
  ## n_outliers        number to identify in plots from lm() and qqPlot()
  ## sw_qqplot         T/F for whether to show the QQ-plot
  ## sw_boxcox         T/F for whether to show Box-Cox transformation
  ## sw_constant_var   T/F for whether to assess constant variance
  ## sw_collinearity   T/F for whether to assess multicollinearity between predictor varaibles
  ## sw_order_of_data  T/F for whether to show residuals by order of data
  ## sw_addedvar       T/F for whether to show added-variables plot
  ## sw_plot_set       NULL to accept other plot options, or "simple" to exclude boxcox, constant var, collinearity, order of data, and added-variable plots. "simpleAV" to add back in the added-variable plots.  "all" includes all possible plots in this function.

  ## Changes
  # 2/2/2020 created function
  # 2/14/2020 ask=FALSE to remove prompt "Hit <Return> to see next plot:"

  # sw_plot_set version.  NA accepts argument switch settings.
  if(!is.na(sw_plot_set)) {
    if(sw_plot_set == "all") {
      which_plot = c(4, 6, 1, 2, 3, 5)
    }
    if(sw_plot_set == "simple") {
      #sw_boxcox        <- FALSE
      sw_constant_var  <- FALSE
      sw_collinearity  <- FALSE
      sw_order_of_data <- FALSE
      sw_addedvar      <- FALSE
    }
    if(sw_plot_set == "simpleAV") {
      #sw_boxcox        <- FALSE
      sw_constant_var  <- FALSE
      sw_collinearity  <- FALSE
      sw_order_of_data <- FALSE
    }
  }

  # variable names
  var_names <- names(fit$model)[-1]
  # display settings
  if (is.na(rc_mfrow[1])) {
    n_plots <-
      sw_qqplot           +
      length(which_plot)  +
      length(var_names)   +
      sw_boxcox           +
      sw_constant_var     +
      sw_collinearity     +
      sw_order_of_data    +
      sw_addedvar
    rc_mfrow <- c(ceiling(n_plots / 3), 3)
    #rc_mfrow <- c(1, 3)
  }
  op <- par(no.readonly = TRUE) # the whole list of settable par
  par(mfrow = rc_mfrow)


  # Normal quantile plot (QQ-plot)
  #library(car)
  if(sw_qqplot) {
    car::qqPlot(as.numeric(fit$residuals), las = 1, id = list(n = n_outliers), main = "QQ Plot", ylab = "Residuals")
  }

  #library(nortest)
  nortest::ad.test(fit$residuals)


  # default: Fitted, Cook's distance (with cutoff), and Leverage (with cutoffs)
  for(i_plot in which_plot) {
    par(ask = FALSE)  # do not ask for next plot
    plot(fit, which = i_plot, id.n = n_outliers)
    if (i_plot == 4) {
      Di_large <- 4 / (dim(fit$model)[1] - dim(fit$model)[2] - 1)
      abline(h = Di_large, col = "blue", lty = 3)  # horizontal line
    }
    if (i_plot == 6) {
      lev_large <- c(2, 3) * dim(fit$model)[2] / dim(fit$model)[1]
      abline(h = Di_large    , col = "blue", lty = 3)  # horizontal line
      abline(v = lev_large[1], col = "blue", lty = 3)  # vertical line
      abline(v = lev_large[2], col = "blue", lty = 2)  # vertical line
    }
  }

  # residuals plotted vs each main effect
  if (length(var_names)) {
    for(i_plot in 1:length(var_names)) {
      m_lab <- paste("Residuals vs.", var_names[i_plot])
      if(inherits(fit$model[,var_names[i_plot]], "character")) {
        message(paste0("e_plot_lm_diagostics: ", var_names[i_plot], " is character and may need to be a factor."))
      }
      plot(x = fit$model[,var_names[i_plot]], y = fit$residuals, main = m_lab, ylab = "Residuals", xlab = var_names[i_plot])
      abline(h = 0, col = "gray75", lty = 3)  # horizontal line at zero

      if(inherits(fit$model[,var_names[i_plot]], c("numeric", "integer"))) {
        # use simple smoother if less than 4 observations, otherwise use splines
        if (length(unique(fit$model[,var_names[i_plot]])) < 4) {
          # Loess
          #lines(predict(loess(fit$residuals ~ fit$model[,var_names[i_plot]], enp.target=1)), col="red", lwd=1)
          # Friedman's SuperSmoother
          graphics::lines(
            supsmu(
              x = fit$model[,var_names[i_plot]]
            , y = fit$residuals
            )
          , col = "red"
          , lwd = 1
          )
        } else {
          # if the IQR is 0, we need to increase the tol to be strictly positive
          ss_tol <- 1e-6 * IQR(fit$model[,var_names[i_plot]]) # default
          if (ss_tol == 0) { ss_tol <- 1e-6 * diff(range(fit$model[,var_names[i_plot]])) }
          if (ss_tol == 0) { ss_tol <- 0.1 }
          graphics::lines(
            stats::smooth.spline(
              fit$residuals ~ fit$model[,var_names[i_plot]]
            , spar = 0.8
            , tol = ss_tol
            )
          , col = "red"
          , lwd = 1
          , lty = 1
          )
        }

      }
    }
  }

  # residuals vs order of data
  if(sw_order_of_data) {
    # order of data (not always interesting)
    plot(fit$residuals, main = "Residuals vs Order of data", ylab = "Residuals")
    abline(h = 0, col = "gray75", lty = 3)  # horizontal line at zero
  }

  # Box-Cox transformation suggestion
  # only if all values are positive
  if(sw_boxcox) {
    if(min(fit$model[,1] > 0)){
      #library(car)  # car::boxCox relies on family="bcPower", but "bcPower" is a function in the car package
      bcPower <- car::bcPower   # load the required function into the environment
      try(  # this may not work if the model function isn't in the environment, or if other objects are not available, too
        car::boxCox(
          fit
        , lambda = seq(-3, 3, length = 101)
        , main = "Box-Cox power transformation"
        , xlab = "lambda\nlambda of 1 is none (y^1); 0 is log(y) of any base"
        )
      )
      rm(bcPower)               # remove it
      abline(v = 1  , col = "orange", lty = 3, lwd = 2)  # horizontal line at zero
    }
  }

  # Evaluate homoscedasticity
  if (length(var_names)) {
    if(sw_constant_var) {
      #library(car)
      # non-constant error variance test
      try(
        print(car::ncvTest(fit))
      )
      # plot studentized residuals vs. fitted values
      try(
        car::spreadLevelPlot(fit, sub = "(Homoscedasticity)")
      )
    }
  }

  # Evaluate Collinearity
  if (length(var_names)) {
    if(sw_collinearity) {
      if (length(var_names) >= 2) {
        #library(car)
        vif_val <- car::vif(fit) # variance inflation factors
        graphics::dotchart(vif_val, main = "Collinearity", xlab = "Variance Inflation Factor (VIF)", sub = "Not as useful with interactions")
        abline(v = 0  , col = "gray75", lty = 3)  # horizontal line at zero
        abline(v = 2^2, col = "blue"  , lty = 2)  # vertical line
        abline(v = 10 , col = "blue"  , lty = 3)  # vertical line
        warning("Note: Collinearity plot unreliable for predictors that also have interactions in the model.")
      } else {
        warning("Collinearity plot only available with at least two predictor (x) variables.")
      }
    }
  }

  # Evaluate Partial regression residual plot (added-variables plot)
  if (length(var_names)) {
    if(sw_addedvar) {
      #library(car)
      car::avPlots(fit, id = list(n = n_outliers), layout = NA, ask = FALSE)
    }
  }


  par(op) # reset plotting options

  invisible(NULL)

  ## Useful list of diags: http://www.statmethods.net/stats/rdiagnostics.html
} # e_plot_lm_diagostics
