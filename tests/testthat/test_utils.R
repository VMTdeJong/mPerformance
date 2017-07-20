p3 <- matrix(1/3, nrow = 3, ncol = 3)
i3 <- 1:3
l3 <- c("A", "B", "C"); colnames(p3) <- l3
i.m3 <- matrix(0, nrow = 3, ncol = 3)
diag(i.m3) <- 1

test_that("x2y methods manipulate code as intended.", {
  # To indicator.matrix
  expect_equal(i.m3, i2im(p3, i3))
  expect_equal(i.m3, l2im(p3, l3))

  # To indices
  expect_equal(i3, l2i(p3, l3))
  expect_equal(i3, im2i(i.m3))

  # To labels : not implemented (yet)
  all.equal(l3, i2l(p3, i3))
  all.equal(l3, im2l(p3, i.m3))
})


test_that("GetLabels gets the labels.", {
  expect_equal(getLabels(labels = l3), l3)
  expect_equal(getLabels(p = p3, indices = i3), l3)
  expect_equal(getLabels(p = p3, indicator.matrix = i.m3), l3)
})

test_that("GetIndices gets the indices", {
  expect_equal(getIndices(indices = i3), i3)
  expect_equal(getIndices(p = p3, labels = l3), i3)
  expect_equal(getIndices(indicator.matrix = i.m3), i3)
})

test_that("GetIndicatorMatrix gets the indicator matrix", {
  expect_equal(getIndicatorMatrix(indicator.matrix = i.m3), i.m3)
  expect_equal(getIndicatorMatrix(p = p3, labels = l3), i.m3)
  expect_equal(getIndicatorMatrix(p = p3, indices = i3), i.m3)
})




### (Manual) tests using other packages
# all.equal(im2i(im), l2i(pml, Fishing$mode))
# im <- l2im(pml, Fishing$mode)


#### Some timing tests
# n <- 1e6
# px <- matrix(p3, nrow = nrow(p3) * n, ncol = ncol(p3), byrow = T)
# lx <- rep(l3, n)
# ix <- rep(i3, n)
# i.mx <- matrix(i.m3, nrow = nrow(i.m3) * n, ncol = ncol(i.m3), byrow = T)
#
#
# system.time(l2i(px, lx)) # Faster
# system.time(im2i(i.mx))
#
# system.time(i2l(px, ix)) # Faster
# system.time(im2l(px, i.mx))
#
# system.time(i2im(px, ix)) # Faster
# system.time(l2im(px, lx, names = c("A", "B", "C")))


# system.time(getLabels(labels = lx))
# system.time(getLabels(p = px, indices = ix))
