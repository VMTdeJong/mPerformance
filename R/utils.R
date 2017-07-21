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
  indicator.matrix <- matrix(0, nrow = nrow(p), ncol = ncol(p))
  for (col in 1:ncol(p))
    indicator.matrix[labels == names[col], col] <- 1
  indicator.matrix[is.na(labels), ] <- NA # entire row should be NA when label is NA.
  indicator.matrix
}

# Converts indices to indicator matrix
i2im <- function(p, indices)
{
  indicator.matrix <- matrix(0, nrow = length(indices), ncol = ncol(p))
  for (col in 1:ncol(p))
    indicator.matrix[indices == col, col] <- 1
  indicator.matrix[is.na(indices), ] <- NA # entire row should be NA when index is NA.
  indicator.matrix
}

# Converts indices to labels
i2l <- function(p, indices, names = colnames(p))
{
  if (is.null(names)) stop("names must be specified if colnames(p) is NULL.")
  names[indices]
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
  if (!missing(indices))
    if (!is.null(indices))
      return(indices)
  if (!(missing(p) || missing(labels)) )
    if (!(is.null(p) || is.null(labels)) )
      return(l2i(p = p, labels = labels, names = names))
  if (!missing(indicator.matrix))
    if (!is.null(indicator.matrix))
      return(im2i(indicator.matrix = indicator.matrix))
  # stop("Too many arguments missing.")
}

# Gets labels, using methods above
getLabels <- function(p, labels, indices, indicator.matrix, names = colnames(p))
{
  if (!missing(labels))
    if (!is.null(labels))
      return(labels)
  if (!(missing(p) || missing(indices)) )
    if (!(is.null(p) || is.null(indices)) )
      return(i2l(p = p, indices = indices, names = names))
  if (!missing(indicator.matrix))
    if (!is.null(indicator.matrix))
      return(im2l(p = p, indicator.matrix = indicator.matrix, names = names))
  # stop("Too many arguments missing.")
}

# Gets indicator.matrix, using methos above.
getIndicatorMatrix <- function(p, labels, indices, indicator.matrix, names = colnames(p))
{
  if (!missing(indicator.matrix))
    if (!is.null(indicator.matrix))
      return(indicator.matrix)
  if (!(missing(p) || missing(indices)) )
    if (!(is.null(p) || is.null(indices)) )
      return(i2im(p = p, indices = indices))
  if (!(missing(p) || missing(labels)))
    if (!(is.null(p) || is.null(labels)) )
      return(l2im(p = p, labels = labels, names = names))
  # stop("TOo many arguments missing.")
}
