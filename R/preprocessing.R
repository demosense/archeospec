#' Smooth leaps of signatures
#'
#' Data preprocessing to smooth the leaps in a number of wavelengths as a consequence of different sensors usage.
#'
#' @export
#' @param signatures A spectral object built using the load_signature_files function.
#' @param leaps Wavelengths where exists a leap in the value measurement as a consequence of a different sensor usage.
#' @return The processed (smoothed) signatures.
#'
#' @seealso \code{\link{load_signature_files}}
#'
#' @examples
#' data(signatures)
#' filtered_signatures <- smooth_leaps(signatures, leaps=c(1001, 1831))
#'
smooth_leaps <- function(signatures, leaps) {

  if(!is.spectral(signatures)) {
    stop("Error. signatures parameter is not a spectral data collection")
  }

  data <- signatures$data

  if (length(leaps) >= 1) {

    for (leap in leaps) {
      if( !length(which(names(data)==leap)) != 0 ) {
        stop(sprintf("Error. Leap value of %s is out of bounds.", leap))
      }
    }

    m <- as.matrix(data)

    leaps <- c(leaps, length(data))
    leaps_idx <- purrr::map(leaps, function(x) which(names(data)==x))

    for (i in 1:(length(leaps)-1)) {
      leap1 <- leaps_idx[[i]]
      leap2 <- leaps_idx[[i+1]]

      m <- t(apply(m, 1, function(x) {
        diff <- as.numeric(x[leap1] - x[leap1-1])
        mask <- rep(0, length(x))
        mask[leap1:(leap2-1)] <- diff
        x - mask
      }))
    }

    signatures$data <- as.data.frame(m)
  }

  signatures

}

#' Filter out the high wavelength values
#'
#' Data preprocessing to remove the lowest wavelengths for each signature, possibly because of noise.
#'
#' @export
#' @param signatures A spectral object built using the load_signature_files function.
#' @param head The wavelengths from the minimum to head will be removed.
#' @return The processed (low wavelength filtered) signatures.
#'
#' @seealso \code{\link{load_signature_files}}
#'
#' @examples
#' data(signatures)
#' filtered_signatures <- remove_head(signatures, head=400)
#'
remove_head <- function(signatures, head) {

  if(!is.spectral(signatures)) {
    stop("Error. signatures parameter is not a spectral data collection")
  }

  data <- signatures$data
  head_idx <- which(names(data)==head)

  if( !length(head_idx) != 0 ) {
    stop(sprintf("Error. Head value of %s is out of bounds.", head))
  }

  data <- data[, head_idx:ncol(data)]

  signatures$data <- data
  signatures$range <- colnames(data)

  signatures
}

#' Filter out the low wavelength values
#'
#' Data preprocessing to remove the lowest wavelengths for each signature, possibly because of noise.
#'
#' @export
#' @param signatures A spectral object built using the load_signature_files function.
#' @param tail The wavelengths from tail to the maximum will be removed.
#' @return The processed (high wavelength filtered) signatures.
#'
#' @seealso \code{\link{load_signature_files}}
#'
#' @examples
#' data(signatures)
#' filtered_signatures <- remove_tail(signatures, tail=2400)
#'
remove_tail <- function(signatures, tail) {

  if(!is.spectral(signatures)) {
    stop("Error. signatures parameter is not a spectral data collection")
  }

  data <- signatures$data
  tail_idx <- which(names(data)==tail)

  if( !length(tail_idx) != 0 ) {
    stop(sprintf("Error. Tail value of %s is out of bounds.", tail))
  }

  data <- data[, 1:tail_idx]

  signatures$data <- data
  signatures$range <- colnames(data)

  signatures
}
