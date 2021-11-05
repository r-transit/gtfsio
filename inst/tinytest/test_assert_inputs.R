# assert_vector() ---------------------------------------------------------

# creating a function to emulate how assert_vector()
# would be called in practice
 
fake_fn <- function(x,
                    class,
                    len = NULL,
                    null_ok = FALSE,
                    var_name = NULL,
                    subset_of = NULL,
                    named = FALSE,
                    n_call = -1L) {

  gtfsio:::assert_vector(
    x,
    class,
    len,
    null_ok,
    var_name,
    subset_of,
    named,
    n_call
  )

}

# input checks

expect_error(
  fake_fn(1L, class = c("integer", "numeric")),
  class = "bad_class_argument"
)
expect_error(
  fake_fn(1L, class = c("integer", "numeric")),
  class = "assert_vector_error"
)
expect_error(
  fake_fn("a", "character", len = c(1, 2)),
  class = "bad_len_argument"
)
expect_error(
  fake_fn("a", "character", len = "test"),
  class = "bad_len_argument"
)
expect_error(
  fake_fn("a", "character", null_ok = 1),
  class = "bad_null_ok_argument"
)
expect_error(
  fake_fn("a", "character", var_name = 1),
  class = "bad_var_name_argument"
)
expect_error(
  fake_fn("a", "character", subset_of = 1),
  class = "bad_subset_of_argument"
)
expect_error(
  fake_fn("a", "character", named = 1),
  class = "bad_named_argument"
)
expect_error(
  fake_fn("a", "character", n_call = 1),
  class = "bad_n_call_argument"
)

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

# check that function fails when NA is given, even if checking for logical vecs

expect_error(fake_fn(NA, "logical"), class = "bad_x_argument")

# check that 'var_name' is used correctly in the informative messages

expect_error(
  fake_fn("a", "integer", var_name = "oie"),
  pattern = "'oie' must be a\\(n\\) integer vector\\."
)

# check that 'subset_of' works correctly

expect_true(fake_fn("a", "character", subset_of = c("a", "b")))
expect_error(
  fake_fn("a", "character", subset_of = c("c", "b")),
  pattern = "'x' must be a subset of \\['c', 'b'\\]\\."
)

# check that 'named' works correctly

expect_true(fake_fn(c(oi = "a"), "character", named = TRUE))
expect_error(
  fake_fn("a", "character", named = TRUE),
  pattern = "'x' must be a named character vector\\."
)
expect_error(
  fake_fn(list(1), "list", named = TRUE),
  pattern = "'x' must be a named list\\."
)
expect_error(
  fake_fn(list(oi = 1, 2), "list", named = TRUE),
  pattern = "Every element in 'x' must be named\\."
)

# check that 'n_call' works correctly
# using a function to wrap fake_fn, and this function should be associated with
# the error if n_call = -2

another_fn <- function(x, class) fake_fn(x, class, n_call = -2L)
expect_error(another_fn("a", "integer"), class = c("another_fn_error"))


# assert_list() -----------------------------------------------------------

# another setup function

list_fn <- function(x, len = NULL, null_ok = FALSE, named = FALSE) {

  gtfsio:::assert_list(x, len, null_ok, named)

}

# check that info message is correct
expect_error(list_fn(1), pattern = "'x' must be a list\\.")
expect_error(
  list_fn(list(1), named = TRUE),
  pattern = "'x' must be a named list\\."
)

# check that error class is correct
expect_error(list_fn(1), class = "list_fn_error")

# check that error class is correct


# assert_class() ----------------------------------------------------------

# yet another setup function

class_fn <- function(x, class, call = -1) {

  gtfsio:::assert_class(x, class, call)

}

# check that it errors if 'class' is not a character vector
expect_error(class_fn(1, 1), class = "bad_class_argument")
expect_error(class_fn(1, 1), class = "assert_class_error")

# check that info message is correct
expect_error(
  class_fn(1, "a"),
  pattern = "'x' must inherit from the 'a' class\\."
)
expect_error(
  class_fn(1, c("a", "b")),
  pattern = "'x' must inherit from the 'a', 'b' class\\."
)

# check that error class is correct
expect_error(class_fn(1, "a"), class = "class_fn_error")
