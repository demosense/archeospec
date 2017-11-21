
table_weights <- function(signatures) {
  .check_endmembers_clusters(signatures)

  clusters <- signatures$clusters
  endmembers <- signatures$endmembers

  lapply(
    1:length(clusters),
    function(i) {

      classes <- clusters[[i]]
      ems <- endmembers[[i]]
      data <- cbind(class = classes, signatures$data)
      k <- length(ems)
      emData <- data[ems,]
      endNames <- paste0("end", 1:length(endmembers[[i]]))
      emData <- cbind(endmember=endNames, emData)

      weightsRaw <- .compute_weights(signatures, i)
      weights <- cbind(file = row.names(signatures$data), class = clusters[[i]], weightsRaw)

      residuals <- apply(cbind(signatures$data, weightsRaw), 1, FUN = function (x) {
        original <- x[ 1:length(signatures$range) ]
        weights  <- x[ (length(signatures$range)+1):length(x) ]
        .get_residuals(original, weights, emData[,signatures$range])
      })

      ordering <- arrange(cbind(weights, residual=residuals), class, file)$file
      cbind(weights, residual=residuals)[match(ordering, weights$file),]
    }
  )
}

table_residuals <- function(signatures) {

  endmembers <- signatures$endmembers

  lapply(
    1:length(endmembers),
    function(i) {

      classes <- clusters[[i]]
      ems <- endmembers[[i]]
      data <- cbind(class = classes, signatures$data)
      k <- length(ems)
      emData <- data[ems,]
      endNames <- paste0("end", 1:length(endmembers[[i]]))
      emData <- cbind(endmember=endNames, emData)

      weightsRaw <- .compute_weights(signatures, i)
      weights <- cbind(file = row.names(signatures$data), class = clusters[[i]], weightsRaw)

      residuals <- apply(cbind(signatures$data, weightsRaw), 1, FUN = function (x) {
        original <- x[ 1:length(signatures$range) ]
        weights  <- x[ (length(signatures$range)+1):length(x) ]
        .get_residuals(original, weights, emData[,signatures$range])
      })

      summary(residuals)
    }
  )
}
