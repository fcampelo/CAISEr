#' plot.CAISEr
#'
#' S3 method for plotting _CAISEr_ objects output by [run_experiment()]).
#'
#' @param x list object of class _CAISEr_.
#' @param y unused. Included for consistency with generic `plot` method.
#' @param ... other parameters to be passed down to specific
#'            plotting functions (currently unused)
#' @param latex logical: should labels be formatted for LaTeX? (useful for
#'              later saving using library `TikzDevice`)
#' @param reorder logical: should the comparisons be reordered alphabetically?
#' @param show.text logical: should text be plotted?
#' @param digits how many significant digits should be used in text?
#' @param layout optional parameter to override the layout of the plots (see
#'               `gridExtra::arrangeGrobs()` for details. The default layout is
#'               `lay = rbind(c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(2,2,2,3,3,3))`
#'
#' @return list containing (1) a list of of `ggplot2` objects generated, and
#'         (2) a list of data frames used for the creation of the plots.
#'
#' @method plot CAISEr
#'
#' @export
#'
plot.CAISEr <- function(x, y = NULL, ...,
                        latex = FALSE,
                        reorder = FALSE,
                        show.text = TRUE,
                        digits = 3,
                        layout = NULL)
{
  assertthat::assert_that("CAISEr" %in% class(x),
                          is.logical(latex), length(latex) == 1,
                          is.logical(show.text), length(show.text) == 1,
                          is.logical(reorder), length(reorder) == 1,
                          is.null(layout) || is.matrix(layout))

  plots.list <- vector("list", 3)
  df.list    <- vector("list", 2)

  ignore <- utils::capture.output(x.summary <- summary(x))

  CIs <- as.data.frame(t(sapply(x.summary$test.info,
                                FUN = function(y) y$test$conf.int)))
  Est <- sapply(x.summary$test.info,
                FUN = function(y) y$test$estimate)
  Comps <- sapply(x.summary$test.info,
                  FUN = function(y) y$comparison)
  pvals <- sapply(x.summary$test.info,
                  FUN = function(y) y$test$p.value)
  alpha <- x$samplesize.calc$sig.level

  df <- data.frame(Comparison = Comps,
                   Estimate   = Est,
                   CIl        = CIs[, 1],
                   CIu        = CIs[, 2],
                   p.value    = pvals,
                   alpha      = alpha,
                   stringsAsFactors = FALSE)

  if(reorder) df <- df[order(df$Comparison), ]
  df[, -1]  <- signif(df[, -1], digits = digits)
  df$Reject <- df$p.value <= df$alpha

  if (latex){
    pvaltxt  <- paste0("$p = ", df$p.value, "$")
    alphatxt <- paste0("$\\alpha = ", df$alpha, "$")
    CItxt    <- paste0("$CI = [", df$CIl, ", ",
                       df$CIu, "]$")
    ylabtxt  <- "$\\hat{\\mu}_D$"
  } else {
    pvaltxt  <- paste0("p = ", df$p.value)
    alphatxt <- paste0("alpha = ", df$alpha)
    CItxt    <- paste0("$CI = [", df$CIl, ", ",
                       df$CIu, "]")
    ylabtxt  <- "Est. Difference"
  }

  df <- cbind(df, pvaltxt, alphatxt, CItxt)

  mp <- ggplot2::ggplot(df,
                        ggplot2::aes_string(x = "Comparison",
                                            y = "Estimate",
                                            ymin = "CIl",
                                            ymax = "CIu",
                                            colour = "!Reject",
                                            shape  = "Reject")) +
    ggplot2::theme_minimal() +
    ggplot2::geom_abline(slope = 0, intercept = 0,
                         lty = 3, lwd = 1.4, alpha = .5) +
    ggplot2::geom_pointrange(size = 1.1, fatten = 2,
                             show.legend = FALSE) +
    ggplot2::ylab(ylabtxt) + ggplot2::xlab("") +
    ggplot2::scale_shape_manual(values = c(16, 18)) +
    ggplot2::coord_flip()


  if(show.text){
    mp <- mp +
      ggplot2::geom_text(ggplot2::aes(label = CItxt),
                         nudge_x = .2, size = 2.5,
                         col = 1) +
      ggplot2::geom_text(ggplot2::aes(label = alphatxt),
                         nudge_x = -.175, size = 2.5,
                         col = 1) +
      ggplot2::geom_text(ggplot2::aes(label = pvaltxt),
                         nudge_x = -.35, size = 2.5,
                         col = 1)
  }

  plots.list[[1]] <- mp
  df.list[[1]] <- df


  df <- x$data.raw
  df <- as.data.frame(t(table(df$Algorithm, df$Instance)))
  names(df) <- c("Instance", "Algorithm", "n")
  mp <- ggplot2::ggplot(df,
                        ggplot2::aes_string(x    = "Algorithm",
                                            y    = "n",
                                            fill = "Algorithm")) +
    ggplot2::geom_boxplot(alpha = .3,
                          show.legend   = FALSE,
                          outlier.shape = NA) +
    ggplot2::geom_jitter(ggplot2::aes_string(colour = "Algorithm"),
                         width = .2, height = .1, alpha = .7,
                         show.legend = FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1)) +
    ggplot2::ylab("Runs/Instance") + ggplot2::xlab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  plots.list[[2]] <- mp
  df.list[[2]] <- df

  mp <- ggplot2::ggplot(df,
                        ggplot2::aes_string(x    = "Algorithm",
                                            y    = "n",
                                            fill = "Algorithm")) +
    ggplot2::geom_col(alpha = .5, show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Total number of runs") + ggplot2::xlab("") +
    ggplot2::theme_minimal()


  plots.list[[3]] <- mp

  if (!is.null(layout)) {
    lay <- layout
  } else {
    lay <- rbind(c(1,1,1,1,1,1),
                 c(1,1,1,1,1,1),
                 c(2,2,2,3,3,3))
  }

  gridExtra::grid.arrange(grobs = plots.list, layout_matrix = lay)
  invisible(list(ggplots = plots.list,
                 dfs     = df.list))
}
