#' Performance of multinomial predictions.
#'
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
#' @return  Provides several types of performance measures: discrimination (M-index or multiclass AUC),
#' overall performance (Brier score and various R squares: Cox, Nagelkerke and McFadden) and calibration ().
#'
#' @export

predictionsPerformance <- function(p, labels, indices, indicator.matrix, names = colnames(p), na.rm = T)
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

# mPerformance <- function()
# {
#
# }

