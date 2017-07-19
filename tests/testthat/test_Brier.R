test_that("Brier score for null-model with equal frequencies is correct.", {
  expect_equal(brier(matrix(1/3, nrow = 3, ncol = 3), indices = 1:3), c("Brier" = 2/3))
  expect_equal(brier(matrix(1/4, nrow = 4, ncol = 4), indices = 1:4), c("Brier" = 3/4))
  expect_equal(brier(matrix(1/5, nrow = 5, ncol = 5), indices = 1:5), c("Brier" = 4/5))
})

test_that("Brier handles missing values correctly.", {
  expect_equal(brier(matrix(1/4, nrow = 3, ncol = 4),
                     indicator.matrix = i2im(matrix(1/4, nrow = 4, ncol = 4), 1:4)[1:3, ]),
               c("Brier" = 3/4))
  expect_true(is.na(brier(matrix(1/3, nrow = 3, ncol = 3), indices = c(NA, 2, 3), na.rm = FALSE)) )
}
)

