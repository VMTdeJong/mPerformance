#' mll
#'
#' Compute the multinomial log-likelihood
#'
#' Compute the multinomial log-likelihood, for predicted or fitted values.
#'
#' @name mll
#'
#' @aliases mll
#' MultinomialLogLikelihood
#'
#' @details Not debugged for the handling of missing values.
#'
#' @param p An n x K matrix of probabilities, where n is the number of observations,
#' and K the number of mutually exclusive outcome categories.
#' @param labels Vector of length n, containing the labels (character or factor) of
#' the observed outcome categories. Ignored if \code{indices} is specified.
#' @param indices A vector of length n, containing the indices k, k = 1,...,K, of the observed outcome categories.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @return  \code{mll} provides the multinomial log-likelihood.
#'
#' @examples
#' # Using the observed prevalence for the fitted probabilities,
#' # we obtain the log-likelihood of the null-model:
#' mll(matrix(1/3, nrow = 3, ncol = 3), c(1,2,3))
#'
#' @export
mll <- function(p, labels, indices = l2i(p, labels), na.rm = T)
{
  if (nrow(p) != length(indices)) stop("nrow(p) should equal length(indices).")
  cats <- 1:ncol(p)
  L <- c("Multinomial log-likelihood" = 0)
  for (i in cats)
    {
    L <- L + sum(log(p[indices == i, i]), na.rm = na.rm)
     }
  L
}


#' mr2
#'
#' @aliases MultinomialRSquare
#'
#' @param p An n x K matrix of probabilities, where n is the number of observations,
#' and K the number of mutually exclusive outcome categories.
#' @param labels Vector of length n, containing the labels (character or factor) of
#' the observed outcome categories. Ignored if \code{indices} is specified.
#' @param indices A vector of length n, containing the indices k, k = 1,...,K, of the observed outcome categories.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @return  \code{mr2} provides a data.frame of R-square values by the methods of Cox, Nagelkerke and Mcfadden.
#'
#' @examples
#' # Using for the fitted probabilities the observed frequencies,
#' # we obtain the R^2 values of the null-model:
#' mr2(matrix(1/3, nrow = 3, ncol = 3), c(1,2,3))
#'
#' @export
mr2 <- function(p, labels, indices = l2i(p, labels), na.rm = T)
{
  n_obs <- length(indices)
  n_na_obs <- sum(!is.na(indices))
  if (nrow(as.matrix(p)) != n_obs) { stop("nrow(p) should equal length(indices).")}
  K <- ncol(p)

  prevs <- rep(NA, K)
  for (i in 1:K) prevs[i] <- sum(indices == i, na.rm = T)/n_na_obs
  L0 <- mll(matrix(prevs, nrow = n_obs, ncol = K, byrow = T), indices = indices, na.rm = na.rm)

  L <- mll(p, indices = indices, na.rm = na.rm)

  Cox        <- 1 - exp(-(L - L0) * 2 / n_na_obs)
  Cox_max    <- 1 - exp(2 * n_na_obs ^ (-1) * L0)
  Nagelkerke <- Cox/Cox_max
  McFadden   <- 1 - L / L0

  return(data.frame("Cox" = Cox, "Nagelkerke" = Nagelkerke, "McFadden" = McFadden))
}
