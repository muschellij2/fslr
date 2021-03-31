#' FSL Timeseries Plot using `fsl_tsplot` (not `tsplot`)
#'
#' @param infile comma-separated list of input file names 
#' (ASCII text matrix, one column per timecourse)
#' @param outfile output filename for the PNG file
#' @param plot_title plot title
#' @param legend file name of ASCII text file, one row per legend entry
#' @param labels comma-separated list of labels
#' @param ymin minimum y-value
#' @param ymax maximum y-value
#' @param xlabel X-axis label
#' @param ylabel Y-axis label
#' @param height plot height in pixels (default 150)
#' @param width plot width in pixels (default 600)
#' @param unit scaling units for x-axis (default 1...length of infile)
#' @param precision precision of x-axis labels
#' @param scientific_notation switch on scientific notation
#' @param start_position  Position of first column to plot
#' @param end_position Position of final column to plot
#' @param ... additional options to pass to \code{\link{fslcmd}}
#'
#' @return Name of PNG file
#' @export
fsl_tsplot = function(
  infile, 
  outfile = tempfile(fileext = ".png"),
  plot_title = NULL, legend = NULL,
  labels = NULL, 
  ymin = NULL,
  ymax = NULL,
  xlabel = NULL,
  ylabel = NULL,
  height = NULL,
  width = NULL,
  precision = NULL,
  unit = NULL,
  scientific_notation = FALSE,
  start_position = NULL,
  end_position = NULL, 
  ...) { 
  
  
  L = list(
    plot_title = plot_title, 
    legend = legend,
    labels = labels, 
    ymin = ymin,
    ymax = ymax,
    xlabel = xlabel,
    ylabel = ylabel,
    height = height,
    width = width,
    precision = precision,
    unit = unit,
    start_position = start_position,
    end_position = end_position)
  L = L[ !sapply(L, is.null)]
  if (!is.null(L$legend)) {
    L$legend = normalizePath(L$legend)
  }
  if (!is.null(L$title)) {
    L$title = shQuote(L$title)
  }
  if (!is.null(L$labels)) {
    L$labels = paste0(L$labels, collapse = ",")
  }
  if (length(L) > 0) {
    opts = paste0("--", names(L), "=", L)
  } else {
    opts = ""
  }
  
  if (scientific_notation) {
    opts = c(opts, "--sci")
  }
  opts = c(paste0("--out=", shQuote(outfile)), opts)
  opts = paste(opts, collapse = " ")
  if (is.character(infile)) {
    if (length(infile) > 0) {
      infile = paste(infile, collapse = ",")
    }
  } 
  res = fslcmd(
    "fsl_tsplot", file = infile,
    trim_front = TRUE,
    frontopts = "--in=",
    no.outfile = TRUE,
    opts = opts,
    ...)
  return(res)
}


#' @rdname fsl_tsplot
#' @export
fsl_tsplot.help = function(){
  return(fslhelp("fsl_tsplot", help.arg = ""))
}
