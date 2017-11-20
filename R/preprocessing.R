
#' @export
#'
smooth_leaps <- function(signature, leaps) {

  data <- signature$data

  for (i in 1:length(leaps-1)) {
    leap1 <- leaps[i]
    leap2 <- leaps[i+1]
    diff <- as.numeric(data[as.character(leap1)]- data[as.character(leap1-1)])
    data[as.character(leap1:(leap2-1))] <- data[as.character(leap1:(leap2-1))]-diff
  }

  signature$data <- data
  signature

}


# remove_head_noise <- (signature, )
