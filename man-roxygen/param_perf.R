#' @param p An n x K matrix of probabilities, where n is the number of observations,
#' and K the number of mutually exclusive outcome categories.
#' @param labels Vector of length n, containing the labels (character or factor) of
#' the observed outcome categories. If specified, must correspond with the column names
#' of \code{p} or with \code{names}.
#' @param indices Optional. A vector of length n, containing the indices k, k = 1,...,K,
#' of the observed outcome categories. If specified, these indices must corresond with
#' their respective indices in \code{p}.
#' @param indicator.matrix Optional. An n x K matrix indicating the outcome category of
#' each observation, where n is the number of observations, and K the number of mutually
#' exclusive outcome categories. If specified, the order of the columns should correspond
#' with the order of the columns of \code{p}.
#' @param names Optional. What are the \code{labels} to which the columns of \code{p}
#' should be matched? By default, the colnames of the outcome matrix \code{p}.
