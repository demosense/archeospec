#' Data preprocessing to smooth the leaps in a number of wavelengths as a consequence of different sensors usage
#'
#' @export
#' @param signatures A dataframe which contains the set of samples. Each sample represents the value for each wavelength
#' @param leaps Wavelengths where exists a leap in the value measurement as a consequence of a different sensor usage
#' @return The processed (smoothed) signatures
#'
smooth_leaps <- function(signature, leaps) {

  data <- signature$data

  for (i in 1:length(leaps-1)) {
    leap1 <- leaps[i]
    leap2 <- leaps[i+1]
    diff <- data[as.character(leap1)] - data[as.character(leap1-1)]
    data[as.character(leap1:(leap2-1))] <- data[as.character(leap1:(leap2-1))]-diff
  }

  signature$data <- data
  signature

}
