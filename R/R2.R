#' mll
#'
#' The multinomial log-likelihood
#'
#' @description The multinomial log-likelihood, for predicted or fitted values. Requires the predicted (or fitted) probability matrix \code{p},
#' and one of the following: \code{labels}, \code{indices} or \code{indicator.matrix}.
#'
#' @name mll
#'
#' @aliases mll
#' MultinomialLogLikelihood
#'
#' @param p An n x K matrix of probabilities, where n is the number of observations,
#' and K the number of mutually exclusive outcome categories.
#' @param labels Vector of length n, containing the labels (character or factor) of
#' the observed outcome categories. Ignored if \code{indices} is specified.
#' @param indices A vector of length n, containing the indices k, k = 1,...,K, of the observed outcome categories.
#' @param indicator.matrix Optional. An n x K matrix indicating the outcome category of each observation,
#' where n is the number of observations, and K the number of mutually exclusive outcome categories.
#' Ignored if \code{labels} or \code{indices} is specified.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @return  \code{mll} provides the multinomial log-likelihood.
#'
#' @examples
#' # When we observe 3 outcomes with indices 1, 2 and 3,
#' # we can obtain the log-likelihood of the null-model:
#' # by letting all observed probabilities equal 1/3:
#' probabilities <- matrix(1/3, nrow = 3, ncol = 3)
#' indices <- c(1,2,3)
#' mll(probabilities, indices = indices)
#'
#' # If the outcome is measured as a factor, the levels need to correspond to colnames(p):
#' labels <- as.factor(indices)
#' colnames(probabilities) <- labels
#' mll(probabilities, labels)
#'
#' @export
mll <- function(p, labels, indices, indicator.matrix, na.rm = T)
{
  indices <- getIndices(p = p, labels = labels, indices = indices, indicator.matrix = indicator.matrix)
  if (nrow(p) != length(indices)) stop("nrow(p) should equal length(indices), length(labels) or nrow(indicator.matrix).")
  cats <- 1:ncol(p)
  L <- c("Multinomial log-likelihood" = 0)
  for (i in cats)
    L <- L + sum(log(p[indices == i, i]), na.rm = na.rm)
  L
}


#' mr2
#'
#' @aliases MultinomialRSquare
#'
#' @description  Compute various R-square measures: Cox, Nagelkerke and McFadden. Requires the predicted (or fitted) probability matrix \code{p},
#' and one of the following: \code{labels}, \code{indices} or \code{indicator.matrix}.
#'
#' @param p An n x K matrix of probabilities, where n is the number of observations,
#' and K the number of mutually exclusive outcome categories.
#' @param labels Vector of length n, containing the labels (character or factor) of
#' the observed outcome categories. Ignored if \code{indices} is specified.
#' @param indices Optional. A vector of length n, containing the indices k, k = 1,...,K,
#' of the observed outcome categories. Overrides \code{labels}.
#' @param indicator.matrix Optional. An n x K matrix indicating the outcome category of each observation,
#' where n is the number of observations, and K the number of mutually exclusive outcome categories.
#' Ignored if \code{labels} or \code{indices} is specified.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @return  \code{mr2} provides a data.frame of R-square values by the methods of Cox, Nagelkerke and Mcfadden.
#'
#' @references Nagelkerke NJ. A note on a general definition of the coefficient of determination.
#' Biometrika. 1991 Sep 1;78(3):691-2.
#' McFadden D. Conditional logit analysis of qualitative choice behavior.
#'
#' @examples
#' # If we observe outcomes A, B and C:
#' labels <- c("A", "B", "c")
#' # The fitted probabilities of an intercept only model are given by 1/3:
#' probabilities <- matrix(1/3, nrow = 3, ncol = 3)
#' colnames(probabilities) <- labels
#' # Then the multinomial R-squares can be obtained with:
#' mr2(probabilities, labels)
#' # Or:
#' mr2(probabilities, as.factor(labels))
#' # Similary, we can use the indices of the observed outcome
#' # categories:
#' mr2(probabilities, indices = c(1,2,3))
#' @export
mr2 <- function(p, labels, indices, indicator.matrix, na.rm = T)
{
  indices <- getIndices(p = p, labels = labels, indices = indices, indicator.matrix = indicator.matrix)
  n_obs <- length(indices)
  n_na_obs <- sum(!is.na(indices))
  if (nrow(as.matrix(p)) != n_obs) stop("nrow(p) should equal length(indices), length(labels) or nrow(indicator.matrix).")
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

mr2.old <- function(p, labels, indices = l2i(p, labels), na.rm = T)
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
