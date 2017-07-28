#' M-index for multinomial outcome data
#'
#' M-index for multinomial data. Requires the predicted (or fitted) probability matrix \code{p},
#' and one of the following: \code{labels}, \code{indices} or \code{indicator.matrix}.
#' Preferably one of the two former.
#'
#' @aliases M-index
#' MulticlassAUC
#' mPairwiseC
#'
#' @template param_perf
#' @param pairwiseC Optional. Optained with mPairwiseC. Overrides all other parameters.
#'
#' @return  \code{mIndex} provides the M-index (= Multiclass AUC) of Hand and Till (2001),
#'  a vector of length 1.
#' @return  \code{mPairwiseC} provides a matrix the pairwise c-statistics. Contains both
#' c(i|i) and c(j|i) of equation 3 of Hand and Till (2001), and the diagonal is \code{NA}.
#'
#' @template ref_Hand
#'
#' @examples
#' mIndex(matrix(1/3, nrow = 3, ncol = 3), indices = 1:3)
#' mPairwiseC(matrix(1/3, nrow = 3, ncol = 3), indices = 1:3)
#' @export

mIndex <- function(p, labels, indices, indicator.matrix, pairwiseC, names = colnames(p))
{
  if (missing(pairwiseC))
  {
    K <- ncol(p)
    if (is.null(names)) names <- 1:K

    if (K < 2) stop("Prediction matrix should contain at least 2 columns,
                      where each column contains the predicted probabilities for that category.")
    indices <- getIndices(p = p, labels = labels, indices = indices, indicator.matrix = indicator.matrix)
    pairwiseC <- mPairwiseC(p = p, indices = indices, names = names) # Pairwise c-stats
  }
  c("m" = mean(pairwiseC, na.rm = T)) # Simple version of left part of equation 7 from Hand and Till 2001,
    # except that it is not multiplied by 2, as pairwiseC contains both c(i|j) and c(j|i), not their means.
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
