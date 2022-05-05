if (!require("imager")) {
  install.packages("imager")
  library(imager)
}

body_volume <- function(file_name, split_level = .85, pixel_size = 1){
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
  
  volume <- sum(pi * (rowSums(img.white) / 2 * pixel_size) * pixel_size) 
  
  return(volume)
}

wing_inertia <- function(file_name, split_level = .85){
  
  img.black <- (grayscale(load.image(file_name)) >= split_level)
  
  cloud <- which(
    matrix(img.black, dim(img.black)[1], dim(img.black)[2]) == FALSE,
    arr.ind = T
  )
  
  x <- mean(cloud[,c('row')])
  
  y <- mean(cloud[,c('col')])
  
  inertia <- sum((cloud[,c('row')] - x)^2 + (cloud[,c('col')] - y)^2)
  
  return(inertia)
}

# body_volume("moth1.jpg")

# wing_inertia("new_files/right_wing_a_fasciatus.png")

df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df) <- c('file','volume')
for(file in list.files(path = "files", pattern = "*body\\.(png|jpg)$")){
  df[nrow(df) + 1,] <- c(file, body_volume(paste0('files/', file)))
}
write.csv(df, "body.csv", row.names = FALSE)


df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df) <- c('file','inertia')
for(file in list.files(path = "files", pattern = "*left_wing\\.(png|jpg)$")){
  df[nrow(df) + 1,] <- c(file, body_volume(paste0('files/', file)))
}
write.csv(df, "left_wing.csv", row.names = FALSE)

df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df) <- c('file','inertia')
for(file in list.files(path = "files", pattern = "*right_wing\\.(png|jpg)$")){
  df[nrow(df) + 1,] <- c(file, body_volume(paste0('files/', file)))
}
write.csv(df, "right_wing.csv", row.names = FALSE)