#' VCA algorithm to obtain the k endmembers from the signatures
#'
#' @param signatures A spectral object built using the load_files function
#' @return An extended spectral object which includes the indices of the endmember signatures
#'
#' @seealso load_files
#'
set_endmembers <- function(signatures, k) {

    signatures$endmembers <- sapply(
        k,
        function(ki) unmixr::vca(signatures$data, ki, method="05")$indices
      ) %>%
      setNames(paste0("end", k))

    signatures
}

#' Find the files which match with the computed endmembers
#'
#' @param A spectral object which contains the indices of the endmember signatures. set_endmembers must be called before
#' @return A data frame which contains the files which  match with the computed endmembers
#'
#' @seealso \code{\link{set_endmembers}}
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
  emData <- data[ems,]
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
  kc <- sapply(clusters, function(x) length(levels(x)) )
  ke <- sapply(endmembers, function(x) length(x) )

  if (sum(kc != ke) != 0)
    stop("Cant analize endmembers and clusters. The experiments have different values of k")
}

