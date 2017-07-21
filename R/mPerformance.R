#' Performance of multinomial predictions.
#'
#' @description Predictive performance for predicted or fitted values. Requires the predicted
#' (or fitted) probability matrix \code{p}, and one of the following: \code{labels},
#' \code{indices} or \code{indicator.matrix}. Preferably one of the former.
#'
#' @template param_perf
#' @template param_narm
#'
#' @template ref_Nagelkerke
#' @template ref_Brier
#' @template ref_Hand
#'
#' @template return_mPerformance
#'
#' @export

mPerformance <- function(p, labels, indices, indicator.matrix, names = colnames(p), na.rm = T)
{
  # labels <- getLabels(p = p, labels = labels, indices = indices, # Not (yet) necessary
  #                     indicator.matrix = indicator.matrix, names = names)
  indices          <- getIndices(        p = p, labels = labels, indices = indices,
                                         indicator.matrix = indicator.matrix, names = names)
  indicator.matrix <- getIndicatorMatrix(p = p, labels = labels, indices = indices,
                                         indicator.matrix = indicator.matrix, names = names)


  out <- list()
  out$brier <- brier(p = p, indicator.matrix = indicator.matrix, na.rm = na.rm)
  out$mPairwiseC <- mPairwiseC(p = p, indices = indices, names = names)
  out$mIndex <- mIndex(pairwiseC = out$mPairwiseC, names = names)
  out$rsquare <- mr2(p = p, indices = indices, na.rm = na.rm)
  out$LL.null <- mllnull(p = p, indices = indices, na.rm = na.rm)
  out$LL.model <- c("Multinomial log-likelihood model" =  mll(p = p, indices = indices, na.rm = na.rm)[[1]])

  class(out) <- "mPerformance"
  out
}


getFitData <- function(fit, ...)
{
  if (any(class(fit) == "mlogit"))
  {
    p <- fit$probabilities
    indicator.matrix <- fit$residuals + fit$fitted.values
    return(list(p = p, indicator.matrix = indicator.matrix))
  }

  if (any(class(fit) == "mnlogit"))
  {
    p <- fit$probabilities
    indicator.matrix <- fit$residuals + fit$fitted.values
    return(list(p = p, indicator.matrix = indicator.matrix))
    # Something goes wrong here. The likelihood is not correct. Probably conversion to indices goes wrong.
  }

  stop("class of fit object not recognized. Try mPerformance() instead.")
}

#' Performance of a multinomial prediction model
#'
#'
#' @description Predictive performance for a prediction model. Works with the \code{mlogit}
#' package.
#'
#' @param fit A model fit object.
#'
#' @template ref_Nagelkerke
#' @template ref_Brier
#' @template ref_Hand
#'
#' @template return_mPerformance
#'
#' @export

mModelPerformance <- function(fit, ...)
{
  if (is.list(fit) && !is.data.frame(fit))
  {
    x <- getFitData(fit, ...)
    p <- x$p
    ## not necessary yet:
    # labels <- getLabels(p, labels = x$labels, indices = x$indices, indicator.matrix = indicator.matrix)
    indices          <- getIndices(        p = p, labels = x$labels, indices = x$indices,
                                           indicator.matrix = x$indicator.matrix)
    indicator.matrix <- getIndicatorMatrix(p = p, labels = x$labels, indices = x$indices,
                                           indicator.matrix = x$indicator.matrix)
    remove(x)

    mPerformance(p = p, labels = labels, indices = indices, indicator.matrix = indicator.matrix)
  }
  else
    stop("mModelPerformance requires a model fit object. Try mPerformance() instead.")
}

