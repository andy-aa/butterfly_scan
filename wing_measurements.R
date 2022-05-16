if (!require("imager")) {
  install.packages("imager")
  library(imager)
}

split_level <- .85

# image_name <- "files/Manduca quinquemaculatus/Manduca_quinquemaculatus_left_wing.png"
# image_name <- "files/Manduca rustica/Manduca_rustica_left_wing.png"
# image_name <- "files/Agrius cingulata/Agrius_cingulata_left_wing.png"
# image_name <- "files/Hemaris diffinis/Hemaris_diffinis_left_wing.png"
image_name <- "new_files/left_wing_a_fasciatus.png"
img <- load.image(image_name) %>% plot
img.gray <- grayscale(img) %>% plot
img.black <- (img.gray >= split_level) %>% plot

{
  cloud <- which(img.black %>% matrix(., dim(.)[1], dim(.)[2]) == FALSE, arr.ind = T)
  colnames(cloud) <- c("x", "y")
  hpts <- chull(cloud)
}

d2 <- function(p){
  x <- cloud[p,"x"]
  y <- cloud[p,"y"]
  
  sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2)
}

S3 <-function(p){
  x <- cloud[p,"x"]
  y <- cloud[p,"y"]
  
  abs((x[1] - x[3]) * (y[2] - y[1]) - (x[1] - x[2]) * (y[3] - y[1]))/2
}

S4 <- function(p){
  S3(p[1:3]) + S3(p[c(1,3,4)])
}

S5 <- function(p){
  S4(p[1:4]) + S3(p[c(1,4,5)])
}

S6 <- function(p){
  S5(p[1:5]) + S3(p[c(1,5,6)])
}

PP <- function(N=3, FUN=S3){
  a <- 0
  p <- NULL
  combn(hpts, N, function(p){if((tmp <- FUN(p)) > a) {p <<- p; a <<- tmp}; 0})
  cloud[p,]
}

system.time(pp2 <- PP(2, d2))
system.time(pp3 <- PP(3, S3))
system.time(pp4 <- PP(4, S4))
system.time(pp5 <- PP(5, S5))
system.time(pp6 <- PP(6, S6))

# system.time({
#   # areas <- combn(hpts, 3, function(p){S3(p)})
#   a <- 0
#   combn(hpts, 5, function(p){if((tmp <- S5(p)) > a) {pp <<- cloud[p,]; a <<- tmp}; 0})
# })

img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
pp2 %>% {lines(., col="red"); points(., col='red', pch=20, cex=1, lwd=1)}
pp3 %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red"); points(., col='red', pch=4, cex=2, lwd=1)}
pp4 %>% {polygon(., col=rgb(0, 1, 0, 0.2), border = "green"); points(., col='green', pch=4, cex=2, lwd=1)}
pp5 %>% {polygon(., col=rgb(0, 0, 1, 0.2), border = "blue"); points(., col='blue', pch=4, cex=2, lwd=1)}
pp6 %>% {polygon(., col=rgb(1, 1, 0, 0.2), border = "gold"); points(., col='gold', pch=4, cex=2, lwd=1)}

# system.time({
#   a <- 0
#   combn(hpts, 2, function(p){if((tmp <- d2(p)) > a) {pp <<- cloud[p,]; a <<- tmp}; 0})
# })
# 
# 
# img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
# pp2 %>% {lines(., col="red"); points(., col='red', pch=20, cex=1, lwd=1)}
