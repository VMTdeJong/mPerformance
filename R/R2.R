#' mll
#'
#' The multinomial log-likelihood
#'
#' @description The multinomial log-likelihood, for predicted or fitted values. Requires the
#' predicted (or fitted) probability matrix \code{p}, and one of the following: \code{labels},
#'  \code{indices} or \code{indicator.matrix}. Preferably one of the two former.
#'
#' @aliases mll
#' MultinomialLogLikelihood
#'
#' @template param_perf
#' @template param_narm
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
mll <- function(p, labels, indices, indicator.matrix, names = colnames(p), na.rm = T)
{
  indices <- getIndices(p = p, labels = labels, indices = indices, indicator.matrix = indicator.matrix, names = names)
  if (nrow(p) != length(indices)) stop("nrow(p) should equal length(indices), length(labels) or nrow(indicator.matrix).")
  cats <- 1:ncol(p)
  L <- c("Multinomial log-likelihood" = 0)
  for (i in cats)
    L <- L + sum(log(p[indices == i, i]), na.rm = na.rm)
  L
}


#' Multinomial R-square
#'
#' @aliases MultinomialRSquare
#'
#' @description  Compute various R-square measures: Cox, Nagelkerke and McFadden. Requires the predicted (or fitted) probability matrix \code{p},
#' and one of the following: \code{labels}, \code{indices} or \code{indicator.matrix}. Preferably one of the two former.
#'
#' @template param_perf
#' @template param_narm
#'
#' @return  \code{mr2} provides a data.frame of R-square values by the methods of Cox, Nagelkerke and Mcfadden.
#'
#' @template ref_Nagelkerke
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
mr2 <- function(p, labels, indices, indicator.matrix, names = colnames(p), na.rm = T)
{
  indices <- getIndices(p = p, labels = labels, indices = indices, indicator.matrix = indicator.matrix, names = names)
  n_obs <- length(indices)
  n_na_obs <- sum(!is.na(indices))
  if (nrow(as.matrix(p)) != n_obs) stop("nrow(p) should equal length(indices), length(labels) or nrow(indicator.matrix).")

  L0 <- mllnull(p = p, indices = indices, na.rm = na.rm)[[1]]
  L <- mll(p, indices = indices, na.rm = na.rm)[[1]]

  Cox        <- 1 - exp(-(L - L0) * 2 / n_na_obs)
  Cox_max    <- 1 - exp(2 * n_na_obs ^ (-1) * L0)
  Nagelkerke <- Cox/Cox_max
  McFadden   <- 1 - L / L0

  out        <- data.frame("Cox" = Cox, "Nagelkerke" = Nagelkerke, "McFadden" = McFadden)
  rownames(out) <- "Multinomial R-square"
  out
}

mllnull <- function(p, labels, indices, indicator.matrix, names = colnames(p), na.rm = T)
{
  indices <- getIndices(p = p, labels = labels, indices = indices, indicator.matrix = indicator.matrix, names = names)

  n_na_obs <- sum(!is.na(indices))
  K <- ncol(p)

  prevs <- rep(NA, K)
  for (i in 1:K) prevs[i] <- sum(indices == i, na.rm = T)/n_na_obs

  c("Multinomial log-likelihood null" =  mll(matrix(prevs, nrow = length(indices), ncol = K, byrow = T), indices = indices, na.rm = na.rm)[[1]])
}

mr2.old <- function(p, labels, indices, indicator.matrix, na.rm = T)
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
