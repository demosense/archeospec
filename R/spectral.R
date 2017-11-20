
#' @export
#' @importFrom methods is
is.spectral <- function(x) {
  is(x, "spectral")
}

#' @export
print.spectral <- function (x, ...) {
  print(sprintf("Spectral signatures of %d files", nrow(x$data)))
}

#' @export
summary.spectral <- function (object, ...) {
  print(sprintf("Spectral signatures of %d files\n\n", nrow(object$data)))
  cat("\nRanges\n")
  print(object$range)
  cat("\nFiles\n")
  print(object$files)
}
