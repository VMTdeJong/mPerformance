### mPairwiseC
p3 <- matrix(1/3, nrow = 3, ncol = 3)
i3 <- 1:3
l3 <- c("A", "B", "C"); colnames(p3) <- l3
i.m3 <- matrix(0, nrow = 3, ncol = 3)
diag(i.m3) <- 1

test_that("mPairwiseC works for random predictions.", {
  expect_true(is.matrix(mPairwiseC(p3, indices = i3)))
  expect_true(is.matrix(mPairwiseC(p3, l3)))
  expect_equal(mean(mPairwiseC(p3, indices = i3), na.rm = T), .5)
  expect_true(is.na(mean(mPairwiseC(p3, indices = i3), na.rm = F)))
})

## mIndex
p5 <- matrix(c(1/2, 1/4, 1/4,
                 1/8, 5/8, 2/8,
                 3/8, 1/8, 1/2,
                 5/20, 4/20, 11/20,
                 1/3, 1/3, 1/3),
               nrow = 5, ncol = 3, byrow = T)
colnames(p5) <- l3

test_that("mIndex works for simple cases.", {
  expect_equal(mIndex(p3, l3), c("m" = .5))
  expect_equal(mIndex(p3, indices = i3), c("m" = .5))
  expect_equal(mIndex(p5,  c("A", "B", "C", "A", "A")), c("m" = 5/6)) # Reference value
  expect_equal(mIndex(p5, indices = c(1, 2, 3, 1, 1)),  c("m" = 5/6)) # Reference value
})






