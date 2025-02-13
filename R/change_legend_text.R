#' change_legend_text()
#'
#' This function help you change the text and character of text in the legend.
#' @param svg_xml Your XML document with legend.
#' @param orig_text Original text in the legend.
#' @param change_text Text you want to replace.
#' @param text_color The color of text.
#' @param save_as A file path to save the modified SVG. Default is NULL.
#' @return Changed SVG file.
#' @export
change_legend_text <- function(svg_xml, orig_text, change_text, text_color, save_as=NULL) {

  if(is.null(change_text)){
    change_text <- orig_text
  }
  else if (length(orig_text) != length(change_text)) {
    warning("The length of 'orig_text', 'change_text' must be the same.")
    return(invisible(NULL))
  }

  # Make a copy to avoid modifying the original XML
  mod_svg_xml <- read_xml(as.character(svg_xml))
  # Get top-level children of the SVG
  top_level_nodes <- xml_children(mod_svg_xml)
  # Select nodes with <text> tag
  text_nodes <- top_level_nodes[xml_name(top_level_nodes) == "text"]

  # change text
  mapply(function(orig, change, color) {
    for (node in text_nodes) {
      node_text <- xml_text(node)

      if (node_text == orig) {
        xml_text(node) <- change
        xml_set_attr(node, "fill", color)
      }
    }
  }, orig_text, change_text, text_color)

  # Save modified SVG as a temporary file
  temp_svg_file <- tempfile(fileext=".svg")
  write_xml(mod_svg_xml, temp_svg_file)

  # Display modified SVG preview
  changed_raster <- rasterGrob(rsvg(temp_svg_file))
  grid.newpage()
  grid.draw(changed_raster)

  # Save modified file
  if (!is.null(save_as)) {
    write_xml(mod_svg_xml, save_as)
  }
  return(invisible(mod_svg_xml))
}
