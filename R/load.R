#' Load the samples from a folder path
#'
#' It opens every file wich .asd.txt extension and process them:
#' 1) Search for a line with the header format.
#' 2) Every line after that is supposed to be <wavelength><tab><value>.
#'
#' @export
#' @param path Folder path which contains the .asd files. The files might be in subfolders as well.
#' @param header The line format of the head of the data (the line before the data measurements).
#' @param wavelength_start The shorter measured wavelength in nm.
#' @param wavelength_end The longer measured wavelength in nm.
#' @return A spectral object. Contains the information of the signatures, the processed files and the wavelength ranges.
#'
#' @examples
#' \dontrun{
#' # Do not escape special characters for header parameter, like \t
#' load_signature_files(path = "/path/mydata", header="Wavelength\t%s", wavelength_start=350, wavelength_end=2500)
#' }
load_signature_files <- function(path, header, wavelength_start, wavelength_end) {

  # Compute wavelength range, we asume a complete range of observations
  nwavelengths <- wavelength_end - wavelength_start + 1
  wavelength_range <- as.character(wavelength_start:wavelength_end)

  # List .asd files in path
  files <- list.files(path, pattern = "\\.asd", recursive = T)

  # Parse files and load into matrix
  data <- data.frame(t(sapply(files, function(f) .parse_signature_file(path, f, header), simplify = T)))
  names(data) <- wavelength_range

  # Create output object
  out <- list(
    data = data,
    files = files,
    range = wavelength_range
  )

  class(out) <- "spectral"
  out
}

.parse_signature_file <- function(path, f, header) {
  conn  <- file(paste0(path, '/', f))
  lines <- readLines(conn, warn = F)
  close(conn)

  name <- strsplit(f, "/")[[1]]
  # We keep the last value in path
  name <- name[length(name)]
  # Elimination of residual .txt extension
  name <- substr(name, 1, gregexpr("\\.", name)[[1]][[1]]-1)

  header <- sprintf(header, name)
  headerCol <- pmatch(header, lines)

  dataLines <- lines[(headerCol+1):length(lines)]
  values <- sapply(dataLines, function(x) trimws(strsplit(x, "\t")[[1]][2]), USE.NAMES = F)

  as.numeric(values)
}
