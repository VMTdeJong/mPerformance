#' Brier score for multinomial data.
#'
#' Brier score for multinomial data. Requires the predicted (or fitted) probability matrix \code{p},
#' and one of the following: \code{labels}, \code{indices} or \code{indicator.matrix}.
#'
#' @aliases BrierScore
#'
#' @param p An n x K matrix of probabilities, where n is the number of observations,
#' and K the number of mutually exclusive outcome categories.
#' @param labels Vector of length n, containing the labels (character or factor) of
#' the observed outcome categories. Ignored if \code{indices} or \code{indicator.matrix} is specified.
#' @param indices Optional. A vector of length n, containing the indices k, k = 1,...,K,
#' of the observed outcome categories. Overrides \code{labels}. Ignored if
#' \code{indicator.matrix} is specified.
#' @param indicator.matrix Optional. An n x K matrix indicating the outcome category of each observation,
#' where n is the number of observations, and K the number of mutually exclusive outcome categories.
#' Overrides \code{labels} and \code{indices}.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @return  \code{brier} provides the Brier score, a vector of length 1.
#'
#' @references Brier GW. Verification of forecasts expressed in terms of probability. Monthly weather
#' review. 1950 Jan;78(1):1-3.
#'
#' @examples
#' brier(matrix(1/3, nrow = 3, ncol = 3), indices = c(1,2,3))
#' brier(t(matrix(c(1/2, 1/4, 1/4,
#'                1/8, 5/8, 2/8,
#'                1/8, 1/8, 3/4),
#'                nrow = 3, ncol = 3)),
#' indicator.matrix = matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, byrow = TRUE))
#' @export

brier <- function(p, labels, indices, indicator.matrix, na.rm = T)
{
  indicator.matrix <- getIndicatorMatrix(p = p, labels = labels, indices = indices,
                                         indicator.matrix = indicator.matrix)
  if (!all.equal(dim(p), dim(indicator.matrix)))
    stop("Dimensions of probability matrix p and indicator.matrix should match,
         or nrow(p) should match length(labels) or length(indices).")
  c("Brier" = mean(rowSums((p - indicator.matrix)^2), na.rm = na.rm))
}


