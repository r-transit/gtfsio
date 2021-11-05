# input checks

expect_error(
  gtfsio:::gtfsio_error(message = c("oi", "ola"), subclass = "test"),
  class = "simpleError"
)

expect_error(
  gtfsio:::gtfsio_error(message = "oi", subclass = 1),
  class = "simpleError"
)

# error structure

an_error <- tryCatch(
  gtfsio:::gtfsio_error("oi"),
  error = function(cnd) cnd
)
expect_true(all(c("doTryCatch_error", "gtfsio_error") %in% class(an_error)))

an_error <- tryCatch(
  gtfsio:::gtfsio_error("oi", subclass = "test"),
  error = function(cnd) cnd
)
expect_true(
  all(
    c("test", "doTryCatch_error", "gtfsio_error") %in% class(an_error)
  )
)
expect_equal(an_error$message, "oi")

# checking the function that is assigned to the error

fake_fn <- function(parent = FALSE) {
  if (!parent) gtfsio:::gtfsio_error("oi", "test")

  parent_call <- sys.call(-1)
  gtfsio:::gtfsio_error("oi", "test", call = parent_call)
}

other_error <- tryCatch(
  fake_fn(),
  error = function(cnd) cnd
)
expect_inherits(other_error, "fake_fn_error")

# assigning parent function to the error

function_to_assign <- function() fake_fn(parent = TRUE)

last_error <- tryCatch(
  function_to_assign(),
  error = function(cnd) cnd
)
expect_inherits(last_error, "function_to_assign_error")


# parent_function_error() -------------------------------------------------


# should return a function
expect_inherits(gtfsio:::parent_function_error("oi", "ola"), "function")

# the function should assign the error to the parent function
test_error <- gtfsio:::parent_function_error("oi", "ola")
another_fake_fn <- function() {
  test_error()
}

expect_error(another_fake_fn(), "oi", class = "ola")
expect_error(another_fake_fn(), class = "another_fake_fn_error")
