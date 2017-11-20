
#' @export
#'
load_files <- function(path, header=NULL, wavelength_start=NULL, wavelength_end=NULL) {

  if (is.null(header)) {
    warning("Header is null, defaulting to wavelength<filename>")
    header <- "Wavelength\t%s" # (filename)
  }

  if (is.null(wavelength_start)) {
    warning("wavelength_start is null, defaulting to 350")
    wavelength_start <- 350
  }

  if (is.null(wavelength_end)) {
    warning("wavelength_end is null, defaulting to 350")
    wavelength_end <- 2500
  }

  nwavelengths <- wavelength_end - wavelength_start + 1
  wavelength_range <- as.character(wavelength_start:wavelength_end)

  files <- list.files(path, pattern = "\\.asd", recursive = T)

  # Create empty data.frame
  data <- data.frame(names=files)
  zeroes <- data.frame( matrix(0, nrow = nrow(data), ncol = nwavelengths) )
  data <- cbind(data, zeroes)
  names(data) <- c("file", 350:2500)

  for (i in 1:length(files)) {

    f <- files[[i]]

    x <- .parse_signature_file(path, f, header)
    data[i,-1] <- x
  }

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
  # Hardcoded elimination of residual .txt extension
  name <- substr(name, 1, nchar(name)-4)

  header <- sprintf(header, name)
  headerCol <- match(header, lines)

  dataLines <- lines[(headerCol+1):length(lines)]
  values <- sapply(dataLines, function(x) trimws(strsplit(x, "\t")[[1]][2]), USE.NAMES = F)

  as.numeric(values)
}
