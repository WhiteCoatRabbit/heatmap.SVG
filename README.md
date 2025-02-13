# heatmap.SVG

Color SVG and make heatmap on SVG
---------------------------------
Last updated: 2025.02.13.

Hi! I developed this package for draw heatmap with desired shape, such as anatomy or geographical map, and recolor SVG file.
![Graphical abstract of this package](https://github.com/WhiteCoatRabbit/heatmap.SVG/blob/master/inst/extdata/heatmapSVG_graphical_abstract.png)

1) Before you start
   ----------------------------------
    You should make objects into path using Inkscape (Inkscape is free program dealing with SVG files).
    I recommend changing the id of path so you can distinguish. (ex. stomach, small instestine, ... is better than path1, path2, ...)
    Be aware of path label; label is not the id of path.

2) Main functions
   ----------------------------------
   - **`read_n_check()`**: Reads the SVG file and prints it in the Plots pane for visualization.
   - **`find_path_id()`**: Finds the path ID in the SVG file. Make sure to designate the path in Inkscape.
   - **`mapping_path_id()`**: Maps your data to path IDs if they are not the same.
   - **`xy_guideline()`**: Provides guidance on where to position your legend, as the position is relative to the SVG file size.
   - **`heatmapSVG()`**: Draws a heatmap on the SVG, either in a gradient or discrete manner.
   - **`recolorSVG()`**: Recolors a specific path in the SVG.
   - **`change_legend_text()`**: Changes the text or text color in the legend.


3) For example,
   ----------------------------------

 ```r
# Install required packages
install.packages("devtools")
library(devtools)
devtools::install_github("WhiteCoatRabbit/heatmap.SVG")
library(heatmap.SVG)

# 0. Prepare example data
example_data <- data.frame(
    id = c("t", "s_u", "s_l", "stk_m", "stk_u", "stk_l", "stk_c", "dot"), 
    values = c(0.2876, 0.7883, 0.4089, 0.8830, 0.9405, 0.0456, 0.5281, 0.8924)
)
library(readr)
write_tsv(example_data, "example_data.tsv")

# 1. Read and check SVG file with path ID
setwd("C:/Users/a/Downloads")
snake <- read_n_check("snake_logo.svg")

# 2. Find path ID in the data
path_id <- find_path_id(snake)
print(path_id)

# 3. Map path ID with data
mapped_data <- mapping_path_id(
  example_data,
  path_id,
  col_name="id",
  col_factor=c("t", "s_u", "s_l", "stk_m", "stk_u", "stk_l", "stk_c", "dot")
)
print(mapped_data)

# 4. XY guideline: Check where to add legend
xy_guideline(
  snake, 
  color = "red"  # Default is red
)

# 5. Create gradient heatmap using heatmapSVG
snake_gradient <- heatmapSVG(
  mapped_data,
  snake,
  data_col = "values",
  id_col = "matched_path_id",
  range = c(0,1),
  palette = c("blue", "red"),
  breaks = NULL,
  direction = "vertical",
  position = c(0.85, 0.3),
  size = c(0.03, 0.5),
  label_num = 11,
  save_as = "snake_gradient.svg"
)

# 6. Create discrete heatmap using heatmapSVG
snake_discrete <- heatmapSVG(
  mapped_data,
  snake,
  data_col = "values",
  id_col = "matched_path_id",
  range = c(0,1),
  palette = c("chocolate", "mediumorchid", "lightcoral", "cornflowerblue", "yellowgreen"),
  breaks = c(0.2, 0.4, 0.6, 0.8),
  direction = "vertical",
  position = c(0.85, 0.33),
  size = NULL,
  save_as = "snake_discrete.svg"
)

# 7. Change legend text
text_changed <- change_legend_text(
  snake_discrete,
  orig_text = c("0.2 - 0.4", "0 - 0.2"),
  change_text = c("Tongue", "It looks like carrot"),
  text_color = "red",
  save_as = "text_changed.svg"
)

# 8. Recolor specific paths in SVG
recolor_smallparts <- recolorSVG(
  snake_gradient,
  path_id = c("tongue", "Q_mark"), 
  color_factor = c("black", "grey"),
  save_as = "recolor_smallparts.svg"
)
```
4) For people who need detailed explanation,
   ----------------------------------
   I'll planning to write detailed tutorial in my blog, so please wait! Thank you.
