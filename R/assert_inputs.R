#' Assert that an input is a vector with desired properties
#'
#' @param x A vector. The input to be analysed.
#' @param class A string. The class that this input should inherit from.
#' @param len An integer. The length of the vector. If `NULL`, length is not
#'   checked.
#' @param null_ok A logical. Whether the input could also be `NULL`.
#'
#' @return `TRUE` if the check is successful. Throws an error describing the
#'   issue with the input otherwise.
#'
#' @keywords internal
assert_vector <- function(x, class, len = NULL, null_ok = FALSE) {

  # basic input checking

  if (!(is.character(class) && length(class) == 1))
    stop("'class' must me a string.")

  if (!is.null(len) && !(is.integer(len) && length(len) == 1))
    stop("'length' must be an integer vector with length 1.")

  if (!(is.logical(null_ok) && length(null_ok) == 1))
    stop("'null_ok' must be a logical vector with length 1.")

  # get the name of the object sent to 'x'

  call <- sys.call(0)
  input_name <- as.character(call[[2]])

  # objects to create the gtfsio_error
  # use parent_call to assign the parent function to the error class

  parent_call <- sys.call(-1)
  input_error_class <- paste0("bad_", input_name, "_argument")

  input_name <- paste0("'", input_name, "'")
  vector_name <- ifelse(
    class == "list",
    "a list.",
    paste0("a(n) ", class, " vector.")
  )

  # check against desired properties
  # the complicated logical condition below checks for the possibility of 'x'
  # being NULL is null_ok is TRUE

  if ((!inherits(x, class) && null_ok && !is.null(x))
    || (!inherits(x, class) && !null_ok)) {

    gtfsio_error(
      paste0(input_name, " must be ", vector_name),
      input_error_class,
      parent_call
    )

  }

  if (!is.null(len) && len != length(x))
    gtfsio_error(
      paste0(input_name, " must have length ", len, "."),
      input_error_class,
      parent_call
    )

  invisible(TRUE)

}
