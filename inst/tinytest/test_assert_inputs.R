# setup - creating a function to emulate how assert_vector() would be called in
# practice

fake_fn <- function(x, class, len = NULL, null_ok = FALSE) {
  gtfsio:::assert_vector(x, class, len, null_ok)
}

# input checks

expect_error(
  fake_fn(1L, class = c("integer", "numeric")),
  class = "simpleError"
)
expect_error(fake_fn("a", "character", len = c(1, 2)), class = "simpleError")
expect_error(fake_fn("a", "character", len = "test"), class = "simpleError")
expect_error(fake_fn("a", "character", null_ok = 1))

# check if the correct function is assigned to the error

expect_error(fake_fn("a", "integer"), class = c("fake_fn_error"))

# check that error inherits from 'bad_<object>_argument' (since it was called
# with 'x' in fake_fn it will be 'bad_x_argument')

expect_error(fake_fn("a", "integer"), class = c("bad_x_argument"))

# check that correct messages are created depending on the error

expect_error(
  fake_fn("a", "integer"),
  pattern = "'x' must be a\\(n\\) integer vector\\."
)
expect_error(
  fake_fn("a", "list"),
  pattern = "'x' must be a list\\."
)
expect_error(
  fake_fn("a", "character", len = 2L),
  pattern = "'x' must have length 2\\."
)

# check that function works correctly with 'null_ok'

expect_true(fake_fn(NULL, "character", null_ok = TRUE))
expect_error(fake_fn(NULL, "character", null_ok = FALSE))
