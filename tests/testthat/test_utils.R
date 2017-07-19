p3 <- matrix(1/3, nrow = 3, ncol = 3)
i3 <- 1:3
l3 <- c("A", "B", "C"); colnames(p3) <- l3
i.m3 <- matrix(0, nrow = 3, ncol = 3)
diag(i.m3) <- 1

test_that("utils manipulate code as intended.", {
  # To indicator.matrix
  expect_equal(i.m3, i2im(p3, i3))
  expect_equal(i.m3, l2im(p3, l3))

  # To indices
  expect_equal(i3, l2i(p3, l3))
  expect_equal(i3, im2i(i.m3))

  # To labels : not implemented (yet)
  # all.equal(l3, i2l(p3, i3)) # might need some editing
  # all.equal(l3, im2l(p3, i.m3)) # might need some editing
})
