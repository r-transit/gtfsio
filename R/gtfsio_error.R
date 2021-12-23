#' gtfsio's custom error condition constructor
#'
#' @param message The message to inform about the error.
#' @param subclass The subclass of the error.
#' @param call A call to associate the error with.
#'
#' @family error constructors
#'
#' @keywords internal
gtfsio_error <- function(message,
                         subclass = character(0),
                         call = sys.call(-1)) {

  # input checking

  if (! (is.character(message) && length(message) == 1))
    stop("'message' must be a string.")

  if (! is.character(subclass)) stop("'subclass' must be a character vector.")

  # retrieve the function that called 'gtfsio_error' and include it as a class
  # the tail() call ensures that we pick the name of the function in case of a
  # namespaced call (e.g. gtfsio::assert_file_exists())

  fn_name <- utils::tail(as.character(call[[1]]), 1)
  fn_error_class <- paste0(fn_name, "_error")

  subclass <- c(subclass, fn_error_class, "gtfsio_error")
  error <- errorCondition(message, class = subclass, call = NULL)

  stop(error)

}


#' Parent error function constructor
#'
#' Creates a function that raises an error that is assigned to the function in
#' which the error was originally seen. Useful to prevent big repetitive
#' `gtfsio_error()` calls in the "main" functions.
#'
#' @param message The message to inform about the error.
#' @param subclass The subclass of the error.
#'
#' @family error constructors
#'
#' @keywords internal
parent_function_error <- function(message, subclass = character(0)) {
  function() {
    parent_call <- sys.call(-1)
    gtfsio_error(message, subclass, parent_call)
  }
}
