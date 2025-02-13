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