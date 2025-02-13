#' recolorSVG
#' This function changes the colors of an SVG file by modifying the "fill" property in the style attribute of path elements.
#' Use this function when you want to apply specific colors to specific paths.
#'
#' @param svg_xml An XML document from read_n_check(infile).
#' @param path_id A character vector of path IDs to be colored.
#' @param color_factor A character vector of colors. The order should match the order of `path_id`.
#' @param save_as A file path to save the modified SVG. Default is NULL.
#' @return Modified XML document with updated colors (print and return).
#' @export
recolorSVG <- function(svg_xml, path_id, color_factor, save_as = NULL) {
  # Make a copy to avoid modifying the original XML
  mod_svg_xml <- read_xml(as.character(svg_xml))
  # Get top-level children of the SVG
  top_level_nodes <- xml_children(mod_svg_xml)
  # Select nodes with <path> tag
  path_nodes <- top_level_nodes[xml_name(top_level_nodes) == "path"]
  
  modified <- FALSE
  
  for (i in seq_along(path_id)) {
    id <- path_id[i]
    new_color <- color_factor[i]
    
    if(!is.na(new_color)){
      #find path node with the id
      target_node <- path_nodes[xml_attr(path_nodes, "id") == id]
      
      #change style
      if (length(target_node) > 0) {
        original_style <- xml_attr(target_node, "style")
        
        if (any(grepl("fill:\\s*([^;]+);?", original_style))) {
          updated_style <- str_replace(original_style,
                                       "fill:\\s*([^;]+);?",
                                       paste0("fill:", new_color, ";"))
        }
        else {
          if (is.na(original_style)) {
            updated_style <- paste0("fill:", new_color)
          }
          else {
            updated_style <- paste0(original_style, ";fill:", new_color)
          }
        }
        
        xml_set_attr(target_node, "style", updated_style)
        modified <- TRUE
      }
    } 
    else {
      warning(paste0("Skip ", as.character(id), ": no color designated."))
    }
  }
  
  if (!modified) {
    warning("No matching path IDs found in the file. Use find_path_id() to verify available IDs.")
    return(invisible(NULL))
  }
  
  #Save modified svg as a temporaly file
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