if (!require("imager")) {
  install.packages("imager")
  library(imager)
}

pixel_size <- 1
border_level <- .85

img <- load.image("new_files/right_wing_a_fasciatus.png") %>% plot

img.gray <- grayscale(img) %>% plot
img.black <- (img.gray >= border_level) %>% plot

cloud <- which(
  matrix(img.black, dim(img.black)[1], dim(img.black)[2]) == FALSE,
  arr.ind = T
)

hpts <- chull(cloud)

lines(cloud[c(hpts, hpts[1]), ], col="red")

comb <- combn(hpts, 2)

distances <- apply(
  comb,
  2,
  function(p){
    (
      (cloud[p[1], 2] - cloud[p[2], 2])^2
      + 
        (cloud[p[1], 1] - cloud[p[2], 1])^2
    )^.5
  }
)

pp <- cloud[comb[,which.max(distances)],]

lines(pp, col="green")

rotation_angle <- atan((pp[1,2] - pp[2,2]) / (pp[1,1] - pp[2,1])) * 180 / pi
rotation_angle

img.rotated <- imrotate(img, -90-rotation_angle, boundary=1) %>% plot

m <- (grayscale(img.rotated) < border_level) %>% plot

volume <- sum(pi * (rowSums(m) / 2 * pixel_size) * pixel_size) 
volume
