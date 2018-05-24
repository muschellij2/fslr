#' @importFrom utils citation
.onAttach <- function(...) {
  if (!interactive()) return()

  package_name = "fslr"
  x = citation(package_name)
  x = format(x, "text")
  x = paste(x, collapse = "\n\n")
  ack <- c(
    paste0("Please cite the ", package_name, 
           " package using:\n"),
    x)

  packageStartupMessage(paste(strwrap(ack), collapse = "\n"))
}