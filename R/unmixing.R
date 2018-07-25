#' Unmixing based on VCA
#'
#' VCA algorithm to obtain the k endmembers from the signatures.
#'
#' @export
#' @import unmixR
#' @param signatures A spectral object built using the load_signature_files function.
#' @param k The number of endmembers to find.
#' @return An extended spectral object which includes the indices of the endmember signatures.
#'
#' @seealso \code{\link{load_signature_files}}
#'
#' @examples
#' data(signatures)
#' unmixed_signatures <- unmixing_vca(signatures, k=2)
#'
unmixing_vca <- function(signatures, k) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  endmembers <- unmixR::vca(signatures$data, k, method="05")$indice
  endNames <- paste0("end", 1:length(endmembers))

  signatures$endmembers <- endmembers
  signatures$endNames <- endNames

  signatures
}

#' Fix the endmembers
#'
#' Find the files which match with the computed endmembers.
#'
#' @export
#' @param signatures A spectral object built using the load_signature_files function.
#' @param endmember_files A vector of filenames (strings) of the selected signatures to be fixed as endmembers.
#' @param endmember_names A vector of names to be assigned to each selected endmember. Optional, by default (NONE) uses the filenames.
#' @param endmember_colors A vector of color names to be assigned to each selected endmember. Check for possible colors \code{\link{colors}}.
#' Optional, by default (NONE) use random colors.
#' @return A data frame which contains the files which match with the selected endmembers.
#'
#' @seealso \code{\link{load_signature_files}}
#'
#' @examples
#' data(signatures)
#' files <- c("almagre.asd.txt", "blanco.asd.txt")
#' names <- c("red", "white")
#' colors <- c("red2", "white")
#' unmixed_signatures <- unmixing_fixed(signatures, endmember_files=files, endmember_names=names, endmember_colors=colors)
#'
unmixing_fixed <- function(signatures, endmember_files, endmember_names=NULL, endmember_colors=NULL) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if (!all(endmember_files %in% signatures$files)) {
    file = endmember_files[[match(FALSE,  endmember_files %in% signatures$files)]]
    stop(sprintf("Error. File %s not found in signature", file))
  }

  endmembers <- unlist(lapply(endmember_files, function(f) match(f, signatures$files)))
  endNames <- if (is.null(endmember_names)) endmember_files else endmember_names

  if (!is.null(endmember_colors)) {
    names(endmember_colors) <- endNames
  }

  signatures$endmembers <- endmembers
  signatures$endNames <- endNames
  signatures$colors <- endmember_colors

  signatures

}


.cnnls <- function(X, Y) {
  features <- ncol(X)
  Rinv <- solve(chol(t(X) %*% X));
  C <- cbind(rep(1,features), diag(features))
  b <- c(1, rep(0,features))
  d <- t(Y) %*% X
  quadprog::solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C, bvec = b, meq = 1)
}

.compute_weights <- function(signatures) {
  ems <- signatures$endmembers
  endNames <- signatures$endNames

  k <- length(ems)
  emData <- signatures$data[ems,]
  emData <- cbind(endmember=endNames, emData)

  X <- t(emData[, signatures$range])

  models <- apply(signatures$data, 1, FUN=function(x) .cnnls(X, matrix(as.numeric(x))) )
  weightsRaw <- data.frame( t(sapply(models, function(x) c(x$solution))) )

  # Remove numeric precision errors
  weightsRaw[weightsRaw < 0] <- 0
  weightsRaw[weightsRaw > 1] <- 1

  names(weightsRaw) <- endNames
  weightsRaw
}

.get_residuals <- function(original, weights, endmembers) {
  recovered <- t(as.numeric(weights)) %*% as.matrix(endmembers)
  residual  <- abs(original - recovered)
  sum(residual)
}

.check_endmembers_clusters <- function(signature) {
  kc <- sapply(signature$clusters, function(x) length(levels(x)) )
  ke <- sapply(signature$endmembers, function(x) length(x) )

  if (sum(kc != ke) != 0)
    stop("Cant analize endmembers and clusters. The experiments have different values of k")
}

