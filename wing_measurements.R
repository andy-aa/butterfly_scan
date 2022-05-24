# if (!require("imager")) {
#   install.packages("imager")
#   library(imager)
# }
library(imager)
library(pathmapping)


split_level <- .95

# image_name <- "files/Manduca quinquemaculatus/Manduca_quinquemaculatus_left_wing.png"
# image_name <- "files/Manduca rustica/Manduca_rustica_left_wing.png"
image_name <- "files/Agrius cingulata/Agrius_cingulata_left_wing.png"
# image_name <- "files/Hemaris diffinis/Hemaris_diffinis_left_wing.png"
# image_name <- "new_files/left_wing_a_fasciatus.png"
img <- load.image(image_name) %>% plot
img %<>% resize(., round(width(.)/100*30),round(height(.)/100*30)) %>% plot
# img <- resize_halfXY(img) %>% plot
img.gray <- grayscale(img) %>% plot
img.black <- as.cimg(img.gray >= split_level) %>% plot
img.edge <- cannyEdges(img.black) %>% plot


{
  cloud <- which(img.black %>% matrix(., dim(.)[1], dim(.)[2]) == FALSE, arr.ind = T)
  colnames(cloud) <- c("x", "y")
  hull_pts <- cloud[chull(cloud),]
  cloud.edge <- which(img.edge %>% matrix(., dim(.)[1], dim(.)[2]) == TRUE, arr.ind = T)
  colnames(cloud.edge) <- c("x", "y")
}

{# Canny edge detection exmple 1
  par(mfrow=c(1,2))
  img %>% plot
  img %>% cannyEdges(.) %>% plot
}

{# Canny edge detection exmple 2
  par(mfrow=c(1,2))
  img.black %>% plot
  img.black %>% cannyEdges(.) %>% plot
}

{
  (img.ashape.obj <- ashape(cloud, alpha = .9)) %>% plot(., ylim=rev(range(.$x[,2])), , xlab="", ylab="")
}


# plot(cloud[chull(cloud),])
# combn(1:5, 3)
# length(cloud[chull(cloud),])
# 1:nrow(cloud[chull(cloud),])


d2 <- function(data){
  x <- data[,"x"]
  y <- data[,"y"]

  sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2)
}

S3 <-function(data){
  x <- data[,"x"]
  y <- data[,"y"]

  abs((x[1] - x[3]) * (y[2] - y[1]) - (x[1] - x[2]) * (y[3] - y[1]))/2
}

S4 <- function(data){
  S3(data) + S3(data[c(1,3,4),])
}

S5 <- function(data){
  S4(data) + S3(data[c(1,4,5),])
}

S6 <- function(data){
  S5(data) + S3(data[c(1,5,6),])
}

SPoly <- function(data){
  x <- data[,"x"]
  y <- data[,"y"]
  s <- 0
  for (i in 1:(nrow(data)-1)) {
    s <- s + abs((x[1] - x[i+1]) * (y[i] - y[1]) - (x[1] - x[i]) * (y[i+1] - y[1]))/2
  }
  s
}

PP <- function(N=2, FUN=d2, data){
  a <- 0
  p <- NULL
  combn(1:nrow(data), N, function(p){if((tmp <- FUN(data[p,])) > a) {p <<- p; a <<- tmp}; 0})
  data[p,]
}


PPP <- function(N=2, data){
  xx <- data[,"x"]
  yy <- data[,"y"]
  
  a <- 0
  p <- 1
  combn(nrow(data), N, function(p){
    x <- xx[p]
    y <- yy[p]

    s <- 0
    for (i in 2:(N-1)) {
      s <- s + abs((x[1] - x[i+1]) * (y[i] - y[1]) - (x[1] - x[i]) * (y[i+1] - y[1]))/2
    }

    if(s > a) {
      p <<- p
      a <<- s
    }
    
    0
  })
  
  data[p,]
}


PPPP <- function(N=2, data){
  xx <- data[,"x"]
  yy <- data[,"y"]
  
  a <- 0
  p <- 1

  comb <- combn(nrow(data), N)
  
  for (c in 1:choose(nrow(data), N)) {
    x <- xx[comb[,c]]
    y <- yy[comb[,c]]
    
    s <- 0
    for (i in 2:(N-1)) {
      s <- s + abs((x[1] - x[i+1]) * (y[i] - y[1]) - (x[1] - x[i]) * (y[i+1] - y[1]))/2
    }
    
    if(s > a) {
      p <- c
      a <- s
    }
    
  }
  
  data[comb[,p],]
}

minimize <- function(data, N=3){
  for (j in 1:(nrow(data)-N)) {
    max <- 0
    ind <- 1
    for (i in 1:nrow(data)) {
      s <- SPoly(data[-i,])
      if(s > max){
        max <- s
        ind <- i
      }
    }
    
    # ind <- which.max(sapply(1:nrow(data), function(i){SPoly(data[-i,])}))

    data <- data[c(-ind), ]
  }
  data
}

# (hull_pts[-65,])
system.time(minimize(hull_pts))


img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
minimize(hull_pts, 8) %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red"); points(., col='red', pch=4, cex=2, lwd=1)}



max_angle_point <- function(points){
  n <- nrow(points)
  pp <- points[c(n, 1:n, 1),]
  
  angles <- sapply(1:n, function(i){
    b <- d2(pp[c(i+1, i),]); 
    c <- d2(pp[c(i+1, i+2),])
    a <- d2(pp[c(i, i+2),])
    
    acos((b^2+c^2-a^2)/(2*b*c))
  })

  # print(angles * 180 / pi)
  print(sum(angles * 180 / pi))
  # print(angles)
  angles * 180 / pi
  
  # as.list(points[which.max(angles),])
  as.list(points[which.min(abs(angles-90)),])
}

corner_left_wing <- function(pp){
  x <- pp[,'x']
  y <- pp[,'y']

  x_max <- max(x)
  y_min <- min(y)
  
  as.list(pp[which.min(((x-x_max)^2 + (y-y_min)^2)^.5), ])
}

# points(corner_left_wing(hull_pts))
  
# pp <- matrix(c(0,100,0,0,0,50), ncol = 2) 
# colnames(pp) <- c("x", "y")
#   
# max_angle_point(pp)

# pp <- (pp3 %>% .[c(nrow(.), 1:nrow(.), 1),])
# i<- 2
# lines(pp[c(i, i+1),])

# sapply(1:5, function(x){x*2})

# PP <- function(N=3, FUN=S3){
#   a <- 0
#   p <- NULL
#   combn(hpts, N, function(p){if((tmp <- FUN(p)) > a) {p <<- p; a <<- tmp}; 0})
#   cloud[p,]
# }

system.time(pp2 <- PP(2, d2, hull_pts))

system.time(pp3 <- PP(3, S3, hull_pts))
system.time(pp3 <- PP(3, surveyors, hull_pts))
system.time(pp3 <- PPP(3, hull_pts))
system.time(pp3 <- PPPP(3, hull_pts))

system.time(pp4 <- PP(4, S4, hull_pts))
system.time(pp4 <- PP(4, surveyors, hull_pts))
system.time(pp4 <- PPP(4, hull_pts))
system.time(pp4 <- PPPP(4, hull_pts))

system.time(pp5 <- PP(5, S5, hull_pts))
system.time(pp5 <- PPP(5, hull_pts))
system.time(pp5 <- PPPP(5, hull_pts))

system.time(pp6 <- PP(6, S6, hull_pts))
system.time(pp6 <- PPP(6, S6, hull_pts))
system.time(pp6 <- PPPP(6, S6, hull_pts))

# system.time({
#   # areas <- combn(hpts, 3, function(p){S3(p)})
#   a <- 0
#   combn(hpts, 5, function(p){if((tmp <- S5(p)) > a) {pp <<- cloud[p,]; a <<- tmp}; 0})
# })

img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
# hull_pts %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red"); points(., col='red', pch=4, cex=2, lwd=1)}
# hull_pts %>% corner_left_wing(.) %>% points(., col="red", pch=20, cex=1.5)
pp2 %>% {lines(., col="red"); points(., col='red', pch=20, cex=1, lwd=1)}
pp3 %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red"); points(., col='red', pch=4, cex=2, lwd=1)}
pp3 %>% max_angle_point(.) %>% points(., col="red", pch=20, cex=1.5)
# text(pp3)
# pp3[1,]
# pp5 %>% .[1,] %>% points(., col="red", pch=20, cex=1.5)
pp4 %>% {polygon(., col=rgb(0, 1, 0, 0.2), border = "green"); points(., col='green', pch=4, cex=2, lwd=1)}
pp4 %>% max_angle_point(.) %>% points(., col="red", pch=20)
pp5 %>% {polygon(., col=rgb(0, 0, 1, 0.2), border = "blue"); points(., col='blue', pch=4, cex=2, lwd=1)}
pp5 %>% max_angle_point(.) %>% points(., col="red", pch=20)
pp6 %>% {polygon(., col=rgb(1, 1, 0, 0.2), border = "gold"); points(., col='gold', pch=4, cex=2, lwd=1)}

pp3 %>% .[c(1:nrow(.), 1:2),]




img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
angles(pp3) %>% points(col="red")
angles(pp4) %>% points(col="blue")
angles(pp5) %>% points(col="blue")

points(pp3[c(1,2),])
(pp3 %>% .[which.max(angles(.)),]) %>% as.list -> p 

points(p)

points(x=p[1], y=p[2])

(.[,'x'])
  
  points(x=.[1,'x'], y=.[1,'y'],)
points(pp3[which.max(angles(pp3)),])

angles(pp3)

lapply(1:5, function(x){x})

# system.time({
#   a <- 0
#   combn(hpts, 2, function(p){if((tmp <- d2(p)) > a) {pp <<- cloud[p,]; a <<- tmp}; 0})
# })
# 
# 
# img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
# pp2 %>% {lines(., col="red"); points(., col='red', pch=20, cex=1, lwd=1)}

img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
PP(2, d2, PP(3, S3, hull_pts)) %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red"); points(., col='red', pch=4, cex=2, lwd=1)}
# PP(2, d2, PP(4, S4, hull_pts)) %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red"); points(., col='red', pch=4, cex=2, lwd=1)}

