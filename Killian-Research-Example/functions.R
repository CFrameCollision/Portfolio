# Functions workbook and storage

qcat0 <- function(..., number) {
  arguments <- list(...)
  i <- length(arguments)

  if (is.numeric(number)) {} else {
    stop("Error: WIP")
  }

  # Rest of function is a WIP
  if (i <= 1) {
    stop("Attempting to cat one item")
  }

  if (is.numeric(...)) {
    for (ind in i) {}
  } else if (is.character(...)) {} else {
    cat(..., sep = "") %>% return()
  }

  cat(..., sep = "")
}
