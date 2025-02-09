# heatmap_SVG

Color SVG and make heatmap on SVG
---------------------------------
Hi! I developed this package for recolor SVG file and drawing heatmap with desired shape, such as anatomy or geographical map.
It's somewhat incomplete now, please understand (it's my first time to develop R package...!)
I'll add better instructions in a few weeks.

Last updated: 2025.02.09.

1) Before you start
   ----------------------------------
    You should make objects into path using Inkscape (Inkscape is free program dealing with SVG files).
    I recommend changing the id of path so you can distinguish. (ex. stomach, small instestine, ... is better than path1, path2, ...)
    Be aware of path label; label is not the id of path.

2) Main functions
   ----------------------------------
    `read_n_check()`: Read svg file and on displays it as a raster plot for checking.
    `find_path_id()`: Find ids of path in an SVG file. If it returns nothing, you should go to Inkscape and check.
    `recolorSVG()`: If you want specific color to specific path, you can use this function.
    `mapping_path_id()`: If you have data with values, do match process for heatmap.
    `heatmapSVG()`: Draw heatmap using your data.

3) For example,
   ----------------------------------

    ```R
    original_file <- read_n_check("path/to/your/file.svg")

    check_path_id <- find_path_id(original_file)

    recolorSVG(original_file,
               check_path_id,
               color_factor=c("red","yellow","green","#450023"), #first path in check_path_id will be red, second yellow, ...
               save_as = "path/to/your/modified_file.svg")
            #or if you don't want to save, save_as=NULL and default is NULL.
            #if you want data in R, write as 'modified_file <- recolorSVG(~~)'

    data_add_id <- mapping_path_id(data,
                               check_path_id,
                               col_name = "column name of data",
                               col_factor = c("alpha", "beta", "gamma", "delta")) #first in check_path_id will match with alpha, second with beta, ...

    heatmapSVG(data_add_id,
             original_file,
             col_name = "beta", #the data column for heatmap
             range = c(-1,1),
             palette = c("blue", "white", "red"), #you can add colors as much as you can
             direction = "vertical", #vertical=colorbar at right, horizontal=colorbar at bottom.
             position = NULL, #You can change it relative to your original_file's width and height.
             size = NULL, #eg. size=c(0.01, 0.33) will gain colorbar with 0.01×width and 0.33×height size.
             label_num = 5, #How many numbers do you want to show next to the colorbar.
             save_as = "heatmap in anatomy.svg" )
        #if you want data in R, write as heated_file <- heatmapSVG(~~)
    ```
