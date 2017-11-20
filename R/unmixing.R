

set_endmembers <- function(signatures, k, seed) {
    raw <- signatures$data[signatures$range]
    signatures$endmembers <- vca(raw, k, method="05", seed = seed)$indices
    signatures
}

endmember_files <- function(signatures) {
  ems <- signatures$endmembers
  k <- length(ems)
  emData <- signatures$data[ems,]
  endNames <- paste0("end", 1:k)
  emData <- cbind(endmember=endNames, emData)
  dplyr::select(emData, endmember, file)
}


.cnnls <- function(X, Y) {
  features <- ncol(X)
  Rinv <- solve(chol(t(X) %*% X));
  C <- cbind(rep(1,features), diag(features))
  b <- c(1, rep(0,features))
  d <- t(Y) %*% X
  solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C, bvec = b, meq = 1)
}

.getResiduals <- function(original, weights, endmembers) {
  recovered <- t(as.numeric(weights)) %*% as.matrix(endmembers)
  residual  <- abs(original - recovered)
  sum(residual)
}
