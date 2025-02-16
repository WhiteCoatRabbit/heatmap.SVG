#' heatmapSVG()
#' This function helps you color path with gradation based on your data.
#' @param data_mapped A dataframe containing the mapping results from mapping_path_id.
#' @param svg_xml XML document from read_n_check(infile).
#' @param data_col Column name of value that is used to make heatmap.
#' @param id_col Column name of id. Default matched_path_id,
#' @param range Under and upper limit of the value. You should give this value since it determines the color. Format should be vector; c(0, 1).
#' @param palette Color you want. Format should be vector; c("blue", "red").
#' @param breaks Breaks for discrete heatmap. If provided, a discrete heatmap will be generated.
#' @param direction Default is vertical. You can choose between vertical and horizontal.
#' @param position Default is right middle (0.9 of width, 0.33 of height). Format should be vector, for example c(0.9, 0.33).
#' @param size Default is 0.03 of width, 0.33 of height. Format should be vector, for example c(0.03, 0.33)
#' @param label_num Default is 3, it designate how much numbers would show next to colorbar.
#' @param save_as A file path to save the modified SVG. Default is NULL.
#' @return Modified XML document with updated colors (print and return).
#' @export
heatmapSVG <- function(data_mapped, svg_xml, data_col, id_col="matched_path_id", range, palette, breaks = NULL, direction="vertical", position=NULL, size=NULL, label_num=3, save_as = NULL){

  #Warning signs if the input is not okay
  if(!direction %in% c("vertical", "horizontal")) {
    warning("Invalid direction. Choose 'vertical' or 'horizontal'.")
    return(invisible(NULL))
  }

  if (is.null(breaks) && length(palette) < 2) {
    warning("For gradient heatmap, you should write at least two colors.")
    return(invisible(NULL))
  }


  if (!is.null(breaks) && (length(breaks) != length(palette) - 1)) {
    warning("The number of breaks should be equal to the number of colors in the palette minus one.")
    return(invisible(NULL))
  }

  if (is.null(position)) {
    position <- if (direction == "horizontal") c(0.33, 0.9)
                else if (direction == "vertical") c(0.9, 0.33)
    else {warning(("You can only choose vertical or horizontal."))
      return(invisible(NULL))}
  }

  if (is.null(size)) {
    size <- if (direction == "horizontal") c(0.33, 0.03)
            else if (direction == "vertical") c(0.03, 0.33)
    else {warning(("You can only choose vertical or horizontal."))
      return(invisible(NULL))}
  }

  # Make a copy to avoid modifying the original XML
  mod_svg_xml <- read_xml(as.character(svg_xml))
  # Get top-level children of the SVG
  top_level_nodes <- xml_children(mod_svg_xml)
  # Select nodes with <path> tag
  path_nodes <- top_level_nodes[xml_name(top_level_nodes) == "path"]

  modified <- FALSE #for checking if it is modified after this code

  #check the size of svg; it will be used to set the position of legend
  width_svg <- as.numeric(xml_attr(mod_svg_xml, "width"))
  height_svg <- as.numeric(xml_attr(mod_svg_xml, "height"))

 #For gradient Heatmap
 if(is.null(breaks)){

 color_palette <- colorRampPalette(palette)(1000)

 for(i in 1:nrow(data_mapped)){
   id <- data_mapped[[id_col]][i]
   heat_value <- data_mapped[[data_col]][i]

   # matching colors by range
   color_index <- round((heat_value - range[1])/(range[2]-range[1])*999+1)
   new_color <- color_palette[color_index]

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

 if (!modified) {
   warning("No matching path IDs found in the file. Use find_path_id() to verify available IDs.")
   return(invisible(NULL))
 }

   #Make gradation colorbar
   # Select nodes with <defs> tag
   defs <- top_level_nodes[xml_name(top_level_nodes) == "defs"]

  #According to direction, change the bar

   #common codes
   rect_x <- width_svg * position[1]
   rect_y <- height_svg * position[2]
   rect_w <- width_svg * size[1]
   rect_h <- height_svg * size[2]

  if(direction=="vertical"){
   # create linearGradient
   gradient_str <- paste0('<linearGradient id="gradientBar" x1="0%" y1="100%" x2="0%" y2="0%">')
   # add stop (dynamically)
   for (i in seq_along(palette)) {
     offset <- (i - 1) / (length(palette) - 1) * 100
     gradient_str <- paste0(gradient_str, '<stop offset="', offset, '%" stop-color="', palette[i], '" />')
   }
   gradient_str <- paste0(gradient_str, '</linearGradient>')

   gradient <- read_xml(gradient_str)
   xml_add_child(defs, gradient)

   # create rect based on SVG size
   rect_str <- paste0('<rect x="', rect_x, '" y="', rect_y, '" width="', rect_w,
                      '" height="', rect_h, '" fill="url(#gradientBar)" />')
   rect <- read_xml(rect_str)
   xml_add_child(mod_svg_xml, rect)

   # Add labels
   label_values <- seq(from = range[1], to = range[2], length.out = label_num)
   label_positions <- seq(from = rect_y + rect_h, to = rect_y, length.out = label_num)

   for (i in seq_along(label_values)) {
     label_str <- paste0('<text x="', rect_x + rect_w + 5, '" y="', label_positions[i],
                         '" font-size="', rect_w / 2, '" fill="black">',
                         round(label_values[i], 2), '</text>')
     label_node <- read_xml(label_str)
     xml_add_child(mod_svg_xml, label_node)
   }
  }
   #for horizontal
   else {
     gradient_str <- paste0('<linearGradient id="gradientBar" x1="0%" y1="0%" x2="100%" y2="0%">')
     for (i in seq_along(palette)) {
       offset <- (i - 1) / (length(palette) - 1) * 100
       gradient_str <- paste0(gradient_str, '<stop offset="', offset, '%" stop-color="', palette[i], '" />')
     }
     gradient_str <- paste0(gradient_str, '</linearGradient>')

     gradient <- read_xml(gradient_str)
     xml_add_child(defs, gradient)

     rect_str <- paste0('<rect x="', rect_x, '" y="', rect_y, '" width="', rect_w,
                        '" height="', rect_h, '" fill="url(#gradientBar)" />')
     rect <- read_xml(rect_str)
     xml_add_child(mod_svg_xml, rect)

     # Label
     label_values <- seq(from = range[1], to = range[2], length.out = label_num)
     label_positions <- seq(from = rect_x, to = rect_x + rect_w, length.out = label_num)

     for (i in seq_along(label_values)) {
       label_str <- paste0('<text x="', label_positions[i], '" y="', rect_y + rect_h + 15,
                           '" font-size="', rect_h / 2, '" fill="black" text-anchor="middle">',
                           round(label_values[i], 2), '</text>')
       label_node <- read_xml(label_str)
       xml_add_child(mod_svg_xml, label_node)
     }
   }
 }

 #for discrete heatmap
 else{
   # Map the color with the breaks
     intervals <- range[1]
     intervals <- c(intervals, breaks)
     intervals <- c(intervals, range[2])

   data_mapped$color <- cut(data_mapped[[data_col]], breaks = intervals, labels = palette, include.lowest = TRUE)

   modified <- FALSE

   for (i in seq_len(nrow(data_mapped))) {
     id <- data_mapped[[id_col]][i]
     new_color <- as.character(data_mapped$color[i])

     target_node <- path_nodes[xml_attr(path_nodes, "id") == id]

     if (length(target_node) > 0) {
       original_style <- xml_attr(target_node, "style")
       updated_style <- ifelse(grepl("fill:\\s*([^;]+);?", original_style),
                               str_replace(original_style, "fill:\\s*([^;]+);?", paste0("fill:", new_color, ";")),
                               paste0(original_style, ";fill:", new_color))

       xml_set_attr(target_node, "style", updated_style)
       modified <- TRUE
     }
   }

   if (!modified) {
     warning("No matching path IDs found in the file. Use find_path_id() to verify available IDs.")
     return(invisible(NULL))
   }

   # add legend
   defs <- top_level_nodes[xml_name(top_level_nodes) == "defs"]

   #for vertical
   if (direction == "vertical") {
     rect_x <- width_svg * position[1]
     rect_y <- height_svg * position[2]
     rect_w <- width_svg * size[1]
     rect_h <- height_svg * size[2] / length(palette)

     for (i in seq_along(palette)) {
       rect_str <- paste0('<rect x="', rect_x, '" y="', rect_y + (i - 0.5) * rect_h - rect_w*0.66,
                          '" width="', rect_w, '" height="', rect_w,
                          '" fill="', palette[i], '" />')
       xml_add_child(mod_svg_xml, read_xml(rect_str))

       label_text <- paste0(intervals[i], " - ", intervals[i+1])

       label_str <- paste0('<text x="', rect_x + rect_w + rect_w/3, '" y="', rect_y + (i - 0.5) * rect_h,
                           '" font-size="', rect_w / 2, '" fill="black">',
                           label_text, '</text>')
       xml_add_child(mod_svg_xml, read_xml(label_str))
     }
   }
   #for horizontal
   else {
     rect_x <- width_svg * position[1]
     rect_y <- height_svg * position[2]
     rect_w <- width_svg * size[1] / length(palette) # Adjust width for horizontal
     rect_h <- height_svg * size[2]

     for (i in seq_along(palette)) {
       rect_str <- paste0('<rect x="', rect_x + (i - 0.5) * rect_w - rect_h*0.66, '" y="', rect_y,
                          '" width="', rect_h, '" height="', rect_h,
                          '" fill="', palette[i], '" />')
       xml_add_child(mod_svg_xml, read_xml(rect_str))

         label_text <- paste0(intervals[i], " - ", intervals[i+1])

       label_str <- paste0('<text x="', rect_x + (i - 0.5) * rect_w, '" y="', rect_y + rect_h + rect_h/1.7,
                           '" font-size="', rect_h / 2, '" fill="black" text-anchor="middle">', # Center text
                           label_text, '</text>')
       xml_add_child(mod_svg_xml, read_xml(label_str))
     }
   }
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
     return(mod_svg_xml)
}
