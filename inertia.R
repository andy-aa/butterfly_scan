if (!require("imager")) {
  install.packages("imager")
  library(imager)
}

# install.packages("dplyr")

border_level <- .85

# img <- load.image("moth1.jpg") %>% plot

img <- load.image("new_files/left_wing_a_fasciatus.png") %>% plot
# img <- load.image("new_files/right_wing_a_fasciatus.png") %>% plot
img.gray <- grayscale(img) %>% plot
img.black <- (img.gray >= border_level) %>% plot

# cannyEdges(img.black) %>% plot


cloud <- which(
  matrix(img.black, dim(img.black)[1], dim(img.black)[2]) == FALSE,
  arr.ind = T
)

x_c <- mean(cloud[,c('row')])

y_c <- mean(cloud[,c('col')])

# max(cloud[,c('row')])
# max(cloud[,c('col')])

points(x=x_c, y=y_c, col='red', pch=4, cex=3)


sum((cloud[,c('row')] - x_c)^2 + (cloud[,c('col')] - y_c)^2)


hpts <- chull(cloud)

triange_arrea <- function(x_1, y_1, x_2, y_2, x_3, y_3){
  abs((x_1 - x_3) * (y_2 - y_1) - (x_1 - x_2) * (y_3 - y_1))/2
}

quadrangle_arrea <- function(x_1, y_1, x_2, y_2, x_3, y_3, x_4, y_4){
  triange_arrea(x_1, y_1, x_2, y_2, x_3, y_3) + triange_arrea(x_1, y_1, x_3, y_3, x_4, y_4)
}

comb <- combn(hpts, 3)

areas <- apply(
  comb,
  2,
  function(p){
      triange_arrea(
        x_1 = cloud[p[1], 2],
        y_1 = cloud[p[1], 1],
        x_2 = cloud[p[2], 2],
        y_2 = cloud[p[2], 1],
        x_3 = cloud[p[3], 2],
        y_3 = cloud[p[3], 1]  
      )
  }
)

pp <- cloud[comb[,which.max(areas)],]

points(pp, col='red', pch=4, cex=3)


comb <- combn(hpts, 4)

areas <- apply(
  comb,
  2,
  function(p){
    quadrangle_arrea(
      x_1 = cloud[p[1], 2],
      y_1 = cloud[p[1], 1],
      x_2 = cloud[p[2], 2],
      y_2 = cloud[p[2], 1],
      x_3 = cloud[p[3], 2],
      y_3 = cloud[p[3], 1],  
      x_4 = cloud[p[4], 2],
      y_4 = cloud[p[4], 1]  
    )
  }
)

pp <- cloud[comb[,which.max(areas)],]

points(pp, col='green', pch=4, cex=3)