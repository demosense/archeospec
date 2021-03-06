% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{smooth_leaps}
\alias{smooth_leaps}
\title{Smooth leaps of signatures}
\usage{
smooth_leaps(signatures, leaps)
}
\arguments{
\item{signatures}{A spectral object built using the load_signature_files function.}

\item{leaps}{Wavelengths where there is a leap in the value measurement as a consequence of a different sensor usage.}
}
\value{
The processed (smoothed) signatures.
}
\description{
Data preprocessing to smooth the leaps in a number of wavelengths as a consequence of different sensors usage.
}
\examples{
data(signatures)
filtered_signatures <- smooth_leaps(signatures, leaps=c(1001, 1831))

}
\seealso{
\code{\link{load_signature_files}}
}
