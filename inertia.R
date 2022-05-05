if (!require("imager")) {
  install.packages("imager")
  library(imager)
}

# install.packages("dplyr")

border_level <- .85

# img <- load.image("moth1.jpg") %>% plot

# img <- load.image("new_files/left_wing_a_fasciatus.png") %>% plot
img <- load.image("new_files/right_wing_a_fasciatus.png") %>% plot
img.gray <- grayscale(img) %>% plot
img.black <- (img.gray >= border_level) %>% plot

# cannyEdges(img.black) %>% plot


cloud <- which(
  matrix(img.black, dim(img.black)[1], dim(img.black)[2]) == FALSE,
  arr.ind = T
)

x <- mean(cloud[,c('row')])

y <- mean(cloud[,c('col')])

# max(cloud[,c('row')])
# max(cloud[,c('col')])

points(x=x, y=y, col='red', pch=4, cex=3)


sum((cloud[,c('row')] - x)^2 + (cloud[,c('col')] - y)^2)


