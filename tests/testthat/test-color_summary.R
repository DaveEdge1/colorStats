# Test basic multiplication
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Test basic functionality with numeric input
test_that("color_summary works with numeric input", {
  # Set a seed for reproducibility
  set.seed(123)

  # Generate random normal data
  x <- rnorm(100)

  # Call our function with 3 decimal places
  result <- color_summary(x, digits = 3)

  # Check that the returned object is a list
  expect_type(result, "list")

  # Verify that we get all 7 expected statistics
  expect_equal(length(result), 7)

  # Confirm that the calculated mean matches what we'd get directly
  # (rounded to the same number of digits)
  expect_equal(round(mean(x), 3), result$mean)

  # Same check for the median statistic
  expect_equal(round(median(x), 3), result$median)
})

# Test error handling for invalid inputs
test_that("color_summary handles errors", {
  # Should error when given a character vector
  expect_error(color_summary("not numeric"))

  # Should error when given a list
  expect_error(color_summary(list(1, 2, 3)))
})
