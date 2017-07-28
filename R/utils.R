# Converts a labels vector to an indices vector,
# using the probability matrix or the names thereof.
l2i <- function(p, labels, names = colnames(p))
{
  if (is.null(names)) stop("names must be specified if colnames(p) is NULL.")
  indices <- rep(NA, length(labels))
  for (i in 1:length(names))
    indices[names[i] == labels] <- i
  indices
}

# Converts labels to indicator matrix
l2im <- function(p, labels, names = colnames(p))
{
  if (is.null(names)) stop("names must be specified if colnames(p) is NULL.")
  indicator.matrix <- matrix(0, nrow = length(labels), ncol = length(names))
  for (col in 1:ncol(indicator.matrix))
    indicator.matrix[labels == names[col], col] <- 1
  indicator.matrix[is.na(labels), ] <- NA # entire row should be NA when label is NA.
  indicator.matrix
}

# Convert indices to indicator matrix
i2im <- function(p, indices)
{
  indicator.matrix <- matrix(0, nrow = length(indices), ncol = ncol(p))
  for (col in 1:ncol(p))
    indicator.matrix[indices == col, col] <- 1
  indicator.matrix[is.na(indices), ] <- NA # entire row should be NA when index is NA.
  indicator.matrix
}

im2im <- function(indices, names)
{
  indicator.matrix <- matrix(0, nrow = length(indices), ncol = length(names))
  for (col in 1:ncol(indicator.matrix))
    indicator.matrix[indices == col, col] <- 1
  indicator.matrix[is.na(indices), ] <- NA # entire row should be NA when index is NA.
  indicator.matrix
}

# Converts indices to labels
i2l <- function(p, indices, names = colnames(p))
{
  if (is.null(names)) stop("names must be specified if colnames(p) is NULL.")
  lab <- names[indices]
  lab
}


# Converts indicator matrix to indices
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

# Converts indicator mmatirx to labels
im2l <- function(p, indicator.matrix, names = colnames(p))
{
  if (is.null(names)) stop("names must be specified if colnames(p) is NULL.")
  names[im2i(indicator.matrix)]
}



###### Methods that combine these. Such that getting the right object takes only a single command.
# Gets indices, using methods above.
# Note that the methods using indicator.matrix are by far the slowest.
getIndices <- function(p, labels, indices, indicator.matrix, names = colnames(p))
{
  if (!missing(indices) && is.numeric(indices))
    return(indices)

  if (!missing(labels) && is.character(names) && (is.character(labels) || is.factor(labels)))
    return(l2i(labels = labels, names = names))

  if (!missing(indicator.matrix) && is.numeric(indicator.matrix))
    return(im2i(indicator.matrix = indicator.matrix))
}

# Gets labels, using methods above
getLabels <- function(p, labels, indices, indicator.matrix, names = colnames(p))
{
  if (!missing(labels) && (is.character(labels) || is.factor(labels)))
    return(labels)

  if (!missing(indices) && is.numeric(indices) && is.character(names))
    return(i2l(indices = indices, names = names))

  if (!missing(indicator.matrix) && is.numeric(indicator.matrix))
    return(im2l(indicator.matrix = indicator.matrix, names = names))
}


# Gets indicator.matrix, using methos above.
getIndicatorMatrix <- function(p, labels, indices, indicator.matrix, names = colnames(p))
{
  if (!missing(indicator.matrix) && is.numeric(indicator.matrix))
    return(indicator.matrix)

  if (!missing(indices) && is.numeric(indices))
    if (!missing(p) && is.numeric(p))
      return(i2im(p = p, indices = indices)) else
        if (is.character(names))
          return(im2im(indices = indices, names = names))

  if (!missing(labels) && (is.factor(labels) || is.character(labels)) && is.character(names))
    return(l2im(labels = labels, names = names))
}
