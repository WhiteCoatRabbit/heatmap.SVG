#' read_n_check()
#' This function reads an SVG file as an XML document and displays it as a raster plot for checking.
#'
#' @param infile Path to the input SVG file. The format should be "path/to/your/infile"
#' @return None (just displays the plot)
#' @export
read_n_check <- function(infile) {
  # Read SVG file as XML
  svg_xml <- read_xml(infile)

  # Load and display the image
  infile_raster <- rasterGrob(rsvg(infile))
  grid.newpage()
  grid.draw(infile_raster)

  # Return the XML object (optional)
  return(svg_xml)
}

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

#' mapping_path_id()
#' This function maps the path IDs from an SVG file to the corresponding labels in a user-provided dataframe.
#'
#' The function takes a dataframe with labeled regions and a vector of path IDs from an SVG file.
#' It maps these IDs to the dataframe based on a specified order in `col_factor` and adds a new column with matched IDs.
#'
#' @param data A dataframe containing the labeled regions.
#' @param path_id A vector of path IDs from the SVG file (obtained from find_path_id()).
#' @param col_name The name of the column in `data` that contains the region labels.
#' @param col_factor A character vector specifying the order of labels for matching with `path_id`.
#' @return The original dataframe with an additional column containing matched path IDs.
#' @export
mapping_path_id <- function(data, path_id, col_name, col_factor) {
  # make named vector; matching path_id based on the order of col_factor
  names(path_id) <- col_factor
  # Add new column and save matched path_id
  data$matched_path_id <- path_id[data[[col_name]]]
  return(data)
}

#' heatmapSVG
#' This function helps you color path with gradation based on your data.
#' @param data_mapped A dataframe containing the mapping results from mapping_path_id.
#' @param svg_xml XML document from read_n_check(infile).
#' @param col_name Column name of value that is used to make heatmap.
#' @param range Under and upper limit of the value. You should give this value since it determines the color. Format should be vector; c(0, 1).
#' @param palette Color you want. Format should be vector; c("blue", "red").
#' @param direction Default is vertical. You can choose between vertical and horizontal.
#' @param position Default is right middle (0.9 of width, 0.33 of height). Format should be vector, for example c(0.9, 0.33).
#' @param size Default is 0.03 of width, 0.33 of height. Format should be vector, for example c(0.03, 0.33)
#' @param label_num Default is 3, it designate how much numbers would show next to colorbar.
#' @param save_as A file path to save the modified SVG. Default is NULL.
#' @return Modified XML document with updated colors (print and return).
#' @export
heatmapSVG <- function(data_mapped, svg_xml, col_name, range, palette, direction="vertical", position=NULL, size=NULL, label_num=3, save_as = NULL){

  if (is.null(position)) {
    position <- if (direction == "horizontal") c(0.33, 0.9)
                else if (direction == "vertical") c(0.9, 0.33)
    else {warning(("You can only choose vertical or horizontal."))}
  }

  if (is.null(size)) {
    size <- if (direction == "horizontal") c(0.33, 0.03)
            else if (direction == "vertical") c(0.03, 0.33)
    else {warning(("You can only choose vertical or horizontal."))}
  }

  # Make a copy to avoid modifying the original XML
  mod_svg_xml <- read_xml(as.character(svg_xml))
  # Get top-level children of the SVG
  top_level_nodes <- xml_children(mod_svg_xml)
  # Select nodes with <path> tag
  path_nodes <- top_level_nodes[xml_name(top_level_nodes) == "path"]

 if(!"matched_path_id" %in% names(data_mapped)){
   warning("No mapped column is in the file. Use mapping_path_id() before use this function.")
   return(invisible(NULL))
 }
 if(range[2]<=range[1]){
   warning("The second value in the range must be greater than the first value.")
 }

 modified <- FALSE

 color_palette <- colorRampPalette(palette)(1000)

 for(i in 1:nrow(data_mapped)){
   id <- data_mapped$matched_path_id[i]
   heat_value <- data_mapped[[col_name]][i]

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

   #Make gradation remarks
   # Select nodes with <defs> tag
   defs <- top_level_nodes[xml_name(top_level_nodes) == "defs"]

  #According to direction, change the bar
  if(direction=="vertical"){
   # create linearGradient
   gradient_str <- paste0('<linearGradient id="gradientBar" x1="0%" y1="100%" x2="0%" y2="0%">')
   # add stop (dynamically)
   for (i in seq_along(palette)) {
     offset <- (i - 1) / (length(palette) - 1) * 100  # 0%, 50%, 100% 등
     gradient_str <- paste0(gradient_str, '<stop offset="', offset, '%" stop-color="', palette[i], '" />')
   }
   gradient_str <- paste0(gradient_str, '</linearGradient>')

   gradient <- read_xml(gradient_str)
   xml_add_child(defs, gradient)

   # create rect based on SVG size
   width_svg <- as.numeric(xml_attr(mod_svg_xml, "width"))
   height_svg <- as.numeric(xml_attr(mod_svg_xml, "height"))

   rect_x <- width_svg * position[1]
   rect_y <- height_svg * position[2]
   rect_w <- width_svg * size[1]
   rect_h <- height_svg * size[2]

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
     xml_add_child(mod_svg_xml, label_node)  # mod_svg_xml로 수정
   }
  }
   else {
     gradient_str <- paste0('<linearGradient id="gradientBar" x1="0%" y1="0%" x2="100%" y2="0%">')
     for (i in seq_along(palette)) {
       offset <- (i - 1) / (length(palette) - 1) * 100  # 0%, 50%, 100% 등
       gradient_str <- paste0(gradient_str, '<stop offset="', offset, '%" stop-color="', palette[i], '" />')
     }
     gradient_str <- paste0(gradient_str, '</linearGradient>')

     gradient <- read_xml(gradient_str)
     xml_add_child(defs, gradient)

     width_svg <- as.numeric(xml_attr(mod_svg_xml, "width"))
     height_svg <- as.numeric(xml_attr(mod_svg_xml, "height"))

     rect_x <- width_svg * position[1]
     rect_y <- height_svg * position[2]
     rect_w <- width_svg * size[1]
     rect_h <- height_svg * size[2]

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

#' @examples
#' You can use example SVG file and data. I'm planning add more SVG files in the future.
#' svg_path <- system.file("extdata", "snake_logo.svg", package = "coloringSVG")
#' svg_xml <- read_n_check(svg_path)
#' heatmap_path <- system.file("extdata", "data_mapped.csv", package = "coloringSVG")
#' heatmap_data <- read.csv(heatmap_path)
#'
