#' xy_guideline()
#' 
#' This function help you how to set your position and size of the legend.
#' @param svg_xml File XML document from read_n_check(infile).
#' @param color Color of the guideline. Default is red.
#' @return Rastered image with 10*10 guidelines
#' @export
xy_guideline <- function(svg_xml, color = "red"){
  # Make temporary file
  temp_svg_file <- tempfile(fileext=".svg")
  write_xml(svg_xml, temp_svg_file)
  
  # Display SVG preview
  changed_raster <- rasterGrob(rsvg(temp_svg_file), width = unit(1, "npc"), height = unit(1, "npc"))
  grid.newpage()
  grid.draw(changed_raster)
  
  # Debug: size of SVG
  grid.ls()  
  
  # 0.1
  x_seq <- seq(0, 1, by = 0.1)
  y_seq <- seq(0, 1, by = 0.1)  
  
  # Add vertical lines
  for (x in x_seq) {
    grid.lines(x = unit(c(x, x), "npc"), y = unit(c(0, 1), "npc"),
               gp = gpar(col = color, lty = "dashed"))
  }
  
  # Add horizontal lines
  for (y in y_seq) {
    grid.lines(x = unit(c(0, 1), "npc"), y = unit(c(y, y), "npc"),
               gp = gpar(col = color, lty = "dashed"))
  }
  
  return(invisible(NULL))
}