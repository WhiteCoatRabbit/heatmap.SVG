#' read_n_check()
#' This function reads an SVG file as an XML document and displays it as a raster plot for checking.
#'
#' @param infile Path to the input SVG file. The format should be "path/to/your/infile"
#' @return None (just displays the plot)
#' @export
read_n_check <- function(infile) {
  # Read SVG file as XML
  
  if (!file.exists(infile)) {
    warning("Warning: 'infile' is not a valid file path or does not exist.")
  }
  
  svg_xml <- read_xml(infile)
  
  # Load and display the image
  infile_raster <- rasterGrob(rsvg(infile))
  grid.newpage()
  grid.draw(infile_raster)
  
  # Return the XML object (optional)
  return(invisible(svg_xml))
}
