#' find_path_id()
#' This function finds the IDs of <path> elements in an SVG file.
#'
#' @param svg_xml XML document from read_n_check(infile).
#' @return A vector of path IDs (printed and returned).
#' @export
find_path_id <- function(svg_xml) {
  # Get top-level children of the SVG
  top_level_nodes <- xml_children(svg_xml)
  
  # Select nodes with <path> tag
  path_nodes <- top_level_nodes[xml_name(top_level_nodes) == "path"]
  
  # Extract ID attributes of <path> elements
  path_ids <- xml_attr(path_nodes, "id")
  
  # Return the path IDs
  return(path_ids)
}