#' M-index for multinomial outcome data
#'
#' M-index for multinomial data. Requires the predicted (or fitted) probability matrix \code{p},
#' and one of the following: \code{labels} or \code{indices}.
#'
#' @aliases M-index
#' MulticlassAUC
#' mPairwiseC
#'
#' @param p An n x K matrix of probabilities, where n is the number of observations,
#' and K the number of mutually exclusive outcome categories.
#' @param labels Vector of length n, containing the labels (character or factor) of
#' the observed outcome categories. Ignored if \code{indices} is specified.
#' @param indices Optional. A vector of length n, containing the indices k, k = 1,...,K,
#' of the observed outcome categories. Overrides \code{labels}.
#' @param pairwiseC Optional. Optained with mPairwiseC Overrides all other parameters.
#' @param names Optional. col and rownames of the outcome matrix.
#'
#' @return  \code{mIndex} provides the M-index (= Multiclass AUC), a vector of length 1.
#' @return  \code{mPairwiseC} provides a matrix with pairwise c-statistics. Contains both
#' c(i|i) and c(j|i), and the diagonal is \code{NA}.
#'
#' @examples
#' mIndex(matrix(1/3, nrow = 3, ncol = 3), indices = 1:3)
#' mPairwiseC(matrix(1/3, nrow = 3, ncol = 3), indices = 1:3)
#' @export

mIndex <- function(p, labels, indices = l2i(p, labels), pairwiseC, names = colnames(p))
{
  p <- as.matrix(p)
  K <- ncol(p)
  if (is.null(names)) names <- 1:K

  if (K < 2) stop("Prediction matrix should contain at least 2 columns,
                      where each column contains the predicted probabilities for that category.")

  if (missing(pairwiseC)) A_mat <- mPairwiseC(p = p, indices = indices, names = names) # Pairwise c-stats
  c("M" = sum(sum(A_mat, na.rm = T)/(K*(K - 1)))) # left part of equation 7 from Hand and Till 2001, except
  # that it is not multiplied by 2, as A_mat contains both c(i|j) and c(j|i), not their means.
}


#' @export
mPairwiseC <- function(p, labels, indices = l2i(p, labels), names = colnames(p))
{
  p <- as.matrix(p)
  K <- ncol(p)
  if (is.null(names)) names <- 1:K

  if (K < 2) stop("Prediction matrix should contain at least 2 columns,
                  where each column contains the predicted probabilities for that category.")

  A_mat <- matrix(NA, nrow = K, ncol = K)
  dimnames(A_mat) <- list("Outcome 1" = names, "Outcome 2" = names)
  for (i in 1:K)
  {
    cat0 <- indices == i
    n0   <- sum(cat0)

    for (j in 1:K)
      if (i != j)
      {
        cat1 <- indices == j
        temp_preds <- p[cat0 | cat1, i]
        temp_out  <- indices[cat0 | cat1]

        n1   <- sum(cat1)

        r <- rank(temp_preds)
        S0 <- sum(as.numeric(r[temp_out == i]))

        # Equation 3 from Hand and Till 2001.
        A_mat[i, j] <- (S0 - n0 * (n0 + 1)/2)/(as.numeric(n0) * as.numeric(n1))
      }
  }
  A_mat
}
