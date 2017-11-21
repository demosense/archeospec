#' VCA algorithm to obtain the k endmembers from the signatures
#'
#' @export
#' @import unmixR
#' @param signatures A spectral object built using the load_files function
#' @param k K value or list of k's to be used as number of endmembers
#' @return An extended spectral object which includes the indices of the endmember signatures
#'
#' @seealso \code{\link{load_files}}
#'
set_endmembers <- function(signatures, k) {

    signatures$endmembers <- data.frame(sapply(
        k,
        function(ki) unmixR::vca(signatures$data, ki, method="05")$indices
      )) %>%
      setNames(paste0("end", k))

    signatures
}

#' Find the files which match with the computed endmembers
#'
#' @export
#' @param signatures A spectral object built using the load_files function. set_endmembers must be called before of this function
#' @return A data frame which contains the files which  match with the computed endmembers
#'
#' @seealso \code{\link{set_endmembers}}
#' @seealso \code{\link{load_files}}
#'
endmember_files <- function(signatures) {

  endmembers <- signatures$endmembers

  lapply(
    endmembers, function(ems) {
      k <- length(ems)
      endNames <- paste0("end", 1:k)
      emData <- cbind(endmember=endNames, signatures$data[ems,])
      dplyr::select(emData, endmember)
    }
  )
}


.cnnls <- function(X, Y) {
  features <- ncol(X)
  Rinv <- solve(chol(t(X) %*% X));
  C <- cbind(rep(1,features), diag(features))
  b <- c(1, rep(0,features))
  d <- t(Y) %*% X
  quadprog::solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C, bvec = b, meq = 1)
}

.compute_weights <- function(signatures, i) {
  classes <- signatures$clusters[[i]]
  ems <- signatures$endmembers[[i]]

  k <- length(ems)
  emData <- signatures$data[ems,]
  endNames <- paste0("end", 1:length(ems))
  emData <- cbind(endmember=endNames, emData)

  X <- t(emData[, signatures$range])

  models <- apply(signatures$data, 1, FUN=function(x) .cnnls(X, matrix(as.numeric(x))) )
  weightsRaw <- data.frame( t(sapply(models, function(x) c(x$solution))) )
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

