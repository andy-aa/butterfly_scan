if (!require("imager")) {
  install.packages("imager")
  library(imager)
}

pixel_size <- 1
border_level <- .85

img <- load.image("moth1.jpg") %>% plot
# img <- load.image("new_files/left_wing_a_fasciatus.png") %>% plot
# img <- load.image("files/Agrius cingulata/Agrius_cingulata_body.png") %>% plot
img.gray <- grayscale(img) 
img.black <- (img.gray >= border_level)
img.edge <- cannyEdges(img.black) 

{
  par(mfrow=c(1,3), mar=c(5.1, 3.1, 1.1, 1.0))
  img %>% {plot(.); title(sub = 'original')}
  img.gray %>% {plot(.); title(sub = 'grayscale')}
  img.black %>% {plot(.); title(sub = 'black and white')}
  par(mfrow = c(1,1))
}


# cloud <- which(
#   matrix(img.black, dim(img.black)[1], dim(img.black)[2]) == FALSE,
#   arr.ind = T
# )

{
  cloud <- which(img.black %>% matrix(., dim(.)[1], dim(.)[2]) == FALSE, arr.ind = T)
  colnames(cloud) <- c("x", "y")
}

hpts <- chull(cloud)


comb <- combn(hpts, 2)

d2 <- function(data){
  x <- data[,"x"]
  y <- data[,"y"]

  sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2)
}

distances <- apply(comb, 2,  function(p){d2(cloud[p,])})

pp <- cloud[comb[,which.max(distances)],]

{
  img %>% plot
  polygon(cloud[c(hpts),], col=rgb(0, 1, 0, 0.1), border = "red")
  lines(pp, col="green")
}

rotation_angle <- atan((pp[1,'y'] - pp[2,'y']) / (pp[1,'x'] - pp[2,'x'])) * 180 / pi
rotation_angle

img.rotated <- imrotate(img, -90-rotation_angle, boundary=1)

img.white <- (grayscale(img.rotated) < border_level)

{
  par(mfrow=c(1,3), mar=c(5.1, 3.1, 1.1, 1.0))
  img %>% {plot(.); title(sub = 'original')}
  img.rotated %>% {plot(.); title(sub = 'rotated')}
  img.white %>% {plot(.); title(sub = 'white and black')}
  par(mfrow=c(1,1))
}

volume <- sum(pi * (colSums(img.white) / 2 * pixel_size) * pixel_size) 
volume

height <- max(rowSums(img.white))
width <- max(colSums(img.white))
area <- sum(img.white)
# barplot(as.vector(colSums(m)))
# barplot(as.vector(rowSums(m)))

