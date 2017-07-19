# To do:
# - debug im2i for NA.


# Converts a labels vector to an indices vector,
# using the probability matrix or the names thereof.
l2i <- function(p, labels, names = colnames(p))
{
  indices <- rep(NA, length(labels))
  for (i in 1:length(names))
    indices[names[i] == labels] <- i
  indices
}


i2im <- function(p, indices)
{
  indicator.matrix <- matrix(0, nrow = length(indices), ncol = ncol(p))
  for (col in 1:ncol(p))
    indicator.matrix[indices == col, col] <- 1
  indicator.matrix[is.na(indices), ] <- NA # entire row should be NA when index is NA.
  indicator.matrix
}

l2im <- function(p, labels, names = colnames(p))
{
  if (is.null(names)) stop("names must be specified if colnames(p) is NULL.")
  indicator.matrix <- matrix(0, nrow = nrow(p), ncol = ncol(p))
  for (col in 1:ncol(p))
    indicator.matrix[labels == names[col], col] <- 1
  indicator.matrix[is.na(labels), ] <- NA # entire row should be NA when label is NA.
  indicator.matrix
}

# im <- l2im(pml, Fishing$mode)

# Note: looks stupid, but is much faster than apply.
im2i <- function(indicator.matrix)
{
  indices <- rep(NA, nrow(indicator.matrix))
  rows <- 1:nrow(indicator.matrix)
  row.nna <- which(rowSums(indicator.matrix) == 1) # indices[i] <- which() fails when there are NA, hence this.

  if (length(row.nna) != length(indices))
    rows <- (1:length(indices))[row.nna]
  else
    rows <-  1:length(indices)


  for (i in rows)
    indices[i] <- which(indicator.matrix[i, ] == 1)
  indices
}

# all.equal(im2i(im), l2i(pml, Fishing$mode))
