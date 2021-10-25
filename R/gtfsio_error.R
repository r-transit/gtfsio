#' gtfsio's custom error condition constructor
#'
#' @param message The message to inform about the error.
#' @param subclass The subclass of the error.
#' @param call A call to associate the error with.
#'
#' @keywords internal
gtfsio_error <- function(message, subclass, call = sys.call(-1)) {

  # input checking

  if (! (is.character(message) && length(message) == 1))
    stop("'message' must be a string.")

  if (! is.character(subclass)) stop("'subclass' must be a character vector.")

  # retrieve the function that called 'gtfsio_error' and include it as a class

  fn_name <- as.character(call[[1]])
  fn_error_class <- paste0(fn_name, "_error")

  subclass <- c(subclass, fn_error_class, "gtfsio_error")
  error <- errorCondition(message, class = subclass, call = call)

  stop(error)

}
