# Null-likelihood for 3 cases:
p3 <- matrix(1/3, nrow = 3, ncol = 3)
i3 <- 1:3
l3 <- c("A", "B", "C"); colnames(p3) <- l3
i.m3 <- matrix(0, nrow = 3, ncol = 3)
diag(i.m3) <- 1

# with missing values:
p3miss <- matrix(1/3, nrow = 4, ncol = 3)
i3miss <- c(i3, NA)
l3miss <- c(l3, NA)
mr2(p3miss, i3miss)

# Null-likelihood for 4 cases (3 categories):
p4 <- matrix(c(1/2, 1/4, 1/4), nrow = 4, ncol = 3, byrow = T)
i4 <- c(1:3, 1)
l4 <- c(l3, "A"); colnames(p4) <- l3
i.m4 <- rbind(i.m3, c(1, 0, 0))

test_that("R2 and mll provide the correct values for the null-likelihood", {
  expect_true(mr2(p3, i3)[1] == 0)
  expect_true(mr2(p3, i3)[2] == 0)
  expect_true(mr2(p3, i3)[3] == 0)

  expect_true(mr2(p4, i4)[1] == 0)
  expect_true(mr2(p4, i4)[2] == 0)
  expect_true(mr2(p4, i4)[3] == 0)
})

test_that("R2 and mll handle missing values as intended.", {
  expect_true(mr2(p3miss, i3miss)[1] == 0)
  expect_true(mr2(p3miss, i3miss)[2] == 0)
  expect_true(mr2(p3miss, i3miss)[3] == 0)

  expect_true(is.na(mr2(p3miss, i3miss, na.rm = F)[1]))
  expect_true(is.na(mr2(p3miss, i3miss, na.rm = F)[2]))
  expect_true(is.na(mr2(p3miss, i3miss, na.rm = F)[3]))
})
