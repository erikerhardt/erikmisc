#' Random forest (RF) function.
#' This function produces standardized output for every analysis.
#'
#' @param y.class
#' @param x.class
#' @param dat.class
#' @param sw.return
#' @param out_rf_classifier
#' @param dat.name
#' @param sw.plots
#' @param sw.silent
#' @param sw.seed   Negative integer specifying seed for the random number generator.
#'
#' @return
#' @importFrom ggRandomForests gg_error
#' @importFrom ggRandomForests gg_interaction
#' @importFrom ggRandomForests gg_minimal_depth
#' @importFrom ggRandomForests gg_minimal_vimp
#' @importFrom ggRandomForests gg_rfsrc
#' @importFrom ggRandomForests gg_roc
#' @importFrom ggRandomForests gg_variable
#' @importFrom ggRandomForests gg_vimp
#' @importFrom ggRandomForests calc_auc
#' @importFrom randomForestSRC find.interaction
#' @importFrom randomForestSRC plot.variable
#' @importFrom randomForestSRC rfsrc
#' @importFrom randomForestSRC var.select
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @export
#'
#' @examples
f_rf_cat2 <- function(y.class, x.class, dat.class, n_tree = 10000, sw.return=FALSE, out_rf_classifier=NULL, dat.name=NULL, sw.plots=TRUE, sw.silent=FALSE, sw.seed = NULL) {
  # Random forest (RF) function.
  ## Random forest code
  # This function produces standardized output for every analysis.
  # Descriptions of each plot are included in comments within the code.

  ## DEBUG
  ## need to define the classification groups (numeric) and features (numeric)
  # dat.class <- base.sub1
  # y.class <- "success"
  # x.class <- c(names.X)

  if (nrow(dat.class) == 0) {
    warning("WARNING: Random Forest data has 0 obs, no output produced")
    return(NULL)
  }

  # binary or continuous outcomes do slightly different things
  #sw.binary <- ifelse(length(unique(unlist(dat.class[,y.class]))) == 2, TRUE, FALSE)
  #sw.binary <- ifelse(length(unique(unlist(dat.class[,y.class]))) <= 5, TRUE, FALSE)
  sw.binary <- !is.numeric(dat.class[,y.class])

  if (sw.binary) {
    dat.class[,y.class] <- factor(dat.class[,y.class])
  }

  # http://arxiv.org/pdf/1501.07196.pdf
  library(randomForestSRC) # random forests for survival, regression and classification
  library(ggplot2) # ggplot2 random forest figures
  library(ggRandomForests) # ggplot2 random forest figures

  theme_set(theme_bw()) # A ggplot2 theme with white background

  if (!sw.silent) {
    print("Classification inputs:")
    print(y.class)
    print(x.class)
  }

  # Random Forest
  rf.formula <- as.formula(paste(y.class, "~", paste(x.class, collapse= "+")))
  rfs.summary <- randomForestSRC::rfsrc(rf.formula
                      , data = dat.class
                      , ntree = n_tree
                      , tree.err = TRUE
                      , importance = TRUE
                      , seed = sw.seed
                      )
  if (!sw.silent) {
    print(rfs.summary)
  }

  if (sw.plots) {

    # Plot the OOB errors against the growth of the forest.
    #   Random forest generalization error.
    #   OOB error convergence along the number of trees in the forest.
    #   The error rate should stabilize as the number of trees grows,
    #     suggesting convergence in prediction error.
    gg_e <- ggRandomForests::gg_error(rfs.summary) %>% tidyr::drop_na()
    p.oob <- plot(gg_e)
    if (sw.binary) {
      p.oob <- p.oob + coord_cartesian(ylim=c(0,1))
    }
    print(p.oob)

    # Plot predicted response
    #   OOB predicted values.
    #   Points are jittered to help visualize predictions for each observation.
    #   Boxplot indicates the distribution of the predicted values.
    p.pred <- plot(ggRandomForests::gg_rfsrc(rfs.summary), alpha=.5) #+ coord_cartesian(ylim=c(0,1))
    print(p.pred)

    # Plot the VIMP rankings of independent variables.
    #   Random forest VIMP (variable importance) plot.
    #   Bars are colored by sign of VIMP,
    #     longer blue bars indicate more important variables,
    #     red indicates inclusion hurts prediction.
    p.vimp <- plot(ggRandomForests::gg_vimp(rfs.summary)) #, lbls=st.labs)
    print(p.vimp)

    # Plot the minimal depth
    #   Minimal Depth variables in rank order, most important at the top.
    #   Vertical dashed line indicates the maximal minimal depth for important variables.
    varsel_rfs <- randomForestSRC::var.select(rfs.summary, verbose = FALSE)
    gg_md <- ggRandomForests::gg_minimal_depth(varsel_rfs)
    p.mindepth <- plot(gg_md) #, lbls=st.labs)
    print(p.mindepth)

    # gg_minimal_depth objects contain information about both minimal depth and VIMP.
    #   Comparing Minimal Depth and Vimp rankings.
    #   Points on the red dashed diagonal line are ranked equivalently,
    #   points below have higher VIMP, those above have higher minimal depth ranking.
    #   Variables are colored by the sign of the VIMP measure.
    p.vimp_mindepth <- plot(ggRandomForests::gg_minimal_vimp(gg_md))
    print(p.vimp_mindepth)


    # We want the top ranked minimal depth variables only, plotted in minimal depth rank order.
    xvar <- gg_md$topvars
    # importance order, instead
    xvar <- names(sort(-rfs.summary$importance))[(names(sort(-rfs.summary$importance)) %in% xvar)]
    if (!length(xvar) | is.null(xvar)) {
      print("NO IMPORTANT XVAR")
      xvar <- gg_md$md.obj$topvars.1se
      if (is.null(xvar)) {
        # grab the most important variable
        xvar <- rownames(gg_md$varselect)[which.max(gg_md$varselect$vimp.all)]
      }
    }

    if (!sw.binary) {
      # Create the variable dependence object from the random forest
      #   Variable dependence plot.
      #   Individual case predictions are marked with points.
      #   Loess smooth curve indicates the trend as the variables increase
      #     with shaded 95% confidence band.
      #gg_v <- gg_variable(rfs.summary)
      gg_v <- ggRandomForests::gg_variable(rfs.summary, partial = TRUE, smooth.lines = TRUE)
      plot(gg_v, xvar=xvar, panel=TRUE) #, se=.95, span=1.2, alpha=.4) #+ labs(y=st.labs["medv"], x="")
    }

    #plot.variable(rfs.summary, xvar.names = xvar, partial = TRUE, smooth.lines = TRUE)

    # Partial Dependence
    #   Partial dependence panels.
    #   Risk adjusted variable dependence for variables in minimal depth rank order
    #   Partial variable dependence plots are a risk adjusted alternative to variable dependence.
    #   Partial plots are generated by integrating out the effects of all variables
    #     beside the covariate of interest.
      op <- par(no.readonly = TRUE)         # save plot settings
      par(oma = c(0,1,0,0) + 0.1, mar = c(4,1,1,1) + 0.1)
    if (sw.binary) {
      pv.tit <- "Marginal effect on class 0 probability"
    } else {
      pv.tit <- paste("Marginal effect on", y.class, "prediction")
    }
    randomForestSRC::plot.variable(rfs.summary, xvar.names=xvar, partial=TRUE, sorted=FALSE, main=pv.tit)
      par(op)                               # restore plot settings

    ## same as above, but ggplot without smoothing lines or intervals
    # if (!sw.binary) {
    #   partial.rfs <- plot.variable(rfs.summary,
    #                                   xvar.names=xvar,
    #                                   partial=TRUE, sorted=FALSE,
    #                                   show.plots = FALSE, smooth.lines=TRUE)
    #   # generate a list of gg_partial objects, one per xvar.
    #   gg_p <- gg_partial(partial.rfs)
    #   # plot the variable list in a single panel plot
    #   plot(gg_p, panel=TRUE)
    # }


    ## Variable Interactions
    #   Minimal depth variable interactions.
    #   Reference variables are marked with red cross in each panel.
    #   Higher values indicate lower interactivity with reference variable.
    if (length(rfs.summary$xvar.names) > 1) {
      interaction.rfs <- randomForestSRC::find.interaction(rfs.summary, verbose=FALSE)

      # Plot the results in a single panel.
      # Minimal depth variable interactions
      plot(ggRandomForests::gg_interaction(interaction.rfs), xvar=xvar, panel=TRUE)
    }


    # ROC classification (categorical) prediction response
    if (sw.binary) {
      p <- NULL
      for (i.binary in 1:length(levels(dat.class[,y.class]))) {  #$
        gg_dta <- ggRandomForests::gg_roc(rfs.summary, which.outcome=i.binary)
        p[[i.binary]] <- plot(gg_dta) + labs(title = paste("outcome ", levels(dat.class[,y.class])[i.binary], sep=""))
        #print(p[[i.binary]])
        print(paste("outcome ", i.binary, ", AUC = ", calc_auc(gg_dta), sep=""))
      }
      library(gridExtra)
      do.call("grid.arrange", c(p, nrow=1))
    }

    # RMPE nonclassification (continuous) prediction response
    if (!sw.binary) {

      df.rfs <- cbind(dat.class, pred = rfs.summary$predicted, pred.oob = rfs.summary$predicted.oob)  #$
      df.rfs$success <- factor(df.rfs$success)

      axis.lim <- range(c(df.rfs$age, df.rfs$pred, df.rfs$pred.oob))

      #library(ggplot2)
      # scatterplot of age and pred, with 1:1 line
      p1 <- ggplot(df.rfs, aes(x = age, y = pred, shape = success, colour = success))
      p1 <- p1 + geom_abline(intercept=0, slope=1, alpha=0.2)
      p1 <- p1 + geom_point()
      p1 <- p1 + geom_rug()
      p1 <- p1 + coord_equal()
      p1 <- p1 + scale_x_continuous(limits=axis.lim)
      p1 <- p1 + scale_y_continuous(limits=axis.lim)
      p1 <- p1 + labs(title = paste(y.class, ": obs vs in-sample pred\nRMPE = ", signif(f_rmse(df.rfs$age, df.rfs$pred), 3), sep=""))
      #print(p1)

      # scatterplot of age and pred, with 1:1 line
      p2 <- ggplot(df.rfs, aes(x = age, y = pred.oob, shape = success, colour = success))
      p2 <- p2 + geom_abline(intercept=0, slope=1, alpha=0.2)
      p2 <- p2 + geom_point()
      p2 <- p2 + geom_rug()
      p2 <- p2 + coord_equal()
      p2 <- p2 + scale_x_continuous(limits=axis.lim)
      p2 <- p2 + scale_y_continuous(limits=axis.lim)
      p2 <- p2 + labs(title = paste(y.class, ": obs vs out-of-sample pred\nRMPE = ", signif(f_rmse(df.rfs$age, df.rfs$pred.oob), 3), sep=""))
      #print(p2)

      #library(gridExtra)
      gridExtra::grid.arrange(p1, p2, nrow=1)

    }
  } # sw.plots

  # report accuracy in summary table in large report
  if (!is.null(out_rf_classifier)) {
    if (sw.binary) {
      # out_rf_classifier <- rbind(out_rf_classifier,
      #                 #paste("      ", dat.name, "   error =", signif(rfs.summary$err.rate[nrow(rfs.summary$err.rate), "all"], 4))
      #                 data.frame(condition = NA, data = dat.name, errortype = "proportion", errorrate = signif(rfs.summary$err.rate[nrow(rfs.summary$err.rate), "all"], 4))
      #                )
      out_rf_classifier[nrow(out_rf_classifier), c("data", "errortype", "errorrate")] <- c(dat.name, "proportion", signif(rfs.summary$err.rate[nrow(rfs.summary$err.rate), "all"], 4))
    }
    if (!sw.binary) {
      #out_rf_classifier <- rbind(out_rf_classifier,
      #                #paste("      ", dat.name, "   RMPE2 =", signif(rfs.summary$err.rate[length(rfs.summary$err.rate)], 4))
      #                data.frame(condition = NA, data = dat.name, errortype = "RMPE^2", errorrate = signif(rfs.summary$err.rate[length(rfs.summary$err.rate)], 4))
      #               )
      out_rf_classifier[nrow(out_rf_classifier), c("data", "errortype", "errorrate")] <- c(dat.name, "RMPE^2", signif(rfs.summary$err.rate[nrow(rfs.summary$err.rate), "all"], 4))
    }
    return(out_rf_classifier)
  }

  if (sw.return) {
    return(rfs.summary)
  }
}
