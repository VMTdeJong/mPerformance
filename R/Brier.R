#' Brier score for multinomial data.
#'
#' Brier score for multinomial data. Requires the predicted (or fitted) probability matrix \code{p},
#' and one of the following: \code{labels}, \code{indices} or \code{indicator.matrix}.
#' Preferably the latter.
#'
#' @aliases BrierScore
#'
#' @template param_perf
#' @template param_narm
#'
#' @return  \code{brier} provides the Brier score, a vector of length 1.
#'
#' @template ref_Brier
#'
#' @examples
#' brier(matrix(1/3, nrow = 3, ncol = 3), indices = c(1,2,3))
#' brier(t(matrix(c(1/2, 1/4, 1/4,
#'                1/8, 5/8, 2/8,
#'                1/8, 1/8, 3/4),
#'                nrow = 3, ncol = 3)),
#' indicator.matrix = matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, byrow = TRUE))
#' @export

brier <- function(p, labels, indices, indicator.matrix, names = colnames(p), na.rm = T)
{
  indicator.matrix <- getIndicatorMatrix(p = p, labels = labels, indices = indices,
                                         indicator.matrix = indicator.matrix, names = names)
  if (!all.equal(dim(p), dim(indicator.matrix)))
    stop("Dimensions of probability matrix p and indicator.matrix should match,
         or nrow(p) should match length(labels) or length(indices).")
  c("Brier" = mean(rowSums((p - indicator.matrix)^2), na.rm = na.rm))
}


