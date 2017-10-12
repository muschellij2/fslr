library(hexSticker)
library(desc)
desc = desc::description$new()
package = desc$get("Package")
# outline = "#0caa41"
outline = "black"
# background = "#0caa41"
background = "#7142f4"
p_color = "black"
sticker("icon.png",	
        package = package,
        h_fill = background,
        h_color = outline, 
        s_width = 0.45, 
        s_height = 0.45,
        s_x = 1,
        filename = "sticker.png")


usethis::use_build_ignore(
  c("icon.png", "sticker.R", "sticker.png"))