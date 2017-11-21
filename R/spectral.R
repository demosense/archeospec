#' Check if the object x is of class spectral
#'
#' @export
#' @importFrom methods is
#' @param x Object to check if it is of class spectral
#' @return TRUE if x is of class spectral. False otherwise
is.spectral <- function(x) {
  is(x, "spectral")
}

#' Print a short description of the spectral object
#'
#' @export
#' @param x Object of class spectral
print.spectral <- function (x, ...) {
  print(sprintf("Spectral signatures of %d files", nrow(x$data)))
}

#' Print the summary of the spectral object
#'
#' @export
#' @param object Object of class spectral
summary.spectral <- function (object, ...) {
  print(sprintf("Spectral signatures of %d files\n\n", nrow(object$data)))
  cat("\nRanges\n")
  print(object$range)
  cat("\nFiles\n")
  print(object$files)
}
