library(imager)
library(magick)
library(image.CornerDetectionHarris)
path <- system.file(package = "image.CornerDetectionHarris", "extdata", "building.png")

save.image(as.cimg(cannyEdges(grayscale(load.image("~/work/new_files/left_wing_a_fasciatus.png") > .85))), "tmp.png", 1)
# %>% plot



img  <- image_read("tmp.png"); file.remove("tmp.png")

# grayscale(image_read("~/work/new_files/left_wing_a_fasciatus.png")) %>% plot
image_draw(img)
pts  <- image_harris(img)
pts

# plt <- 
image_draw(img)
points(pts$x, pts$y, col = "red", pch = 20)
# dev.off()
# plt
