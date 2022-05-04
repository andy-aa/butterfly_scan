if (!require("imager")) {
  install.packages("imager")
  library(imager)
}

body_volume <- function(file_name, split_level = .85){
  img <- load.image(file_name)
  img.black <- (grayscale(img) >= split_level)
  
  cloud <- which(
    matrix(img.black, dim(img.black)[1], dim(img.black)[2]) == FALSE,
    arr.ind = T
  )
  
  hpts <- chull(cloud)
  
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
  
  rotation_angle <- atan((pp[1,2] - pp[2,2]) / (pp[1,1] - pp[2,1])) * 180 / pi
  
  img.rotated <- imrotate(img, -90-rotation_angle, boundary=1)
  
  img.white <- (grayscale(img.rotated) < border_level)
  
  volume <- sum(pi * (colSums(img.white) / 2 * pixel_size) * pixel_size) 
  
  return(volume)
}

body_volume("moth1.jpg")

