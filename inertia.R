if (!require("imager")) {
  install.packages("imager")
  library(imager)
}

# install.packages("dplyr")

border_level <- .85

# img <- load.image("moth1.jpg") %>% plot

# img <- load.image("new_files/left_wing_a_fasciatus.png") %>% plot
# img <- load.image("new_files/right_wing_a_fasciatus.png") %>% plot

img <- load.image("files/Agrius cingulata/Agrius_cingulata_left_wing.png") %>% plot
img.gray <- grayscale(img) %>% plot
img.black <- (img.gray >= border_level) %>% plot

# cannyEdges(img.black) %>% plot
{
  cloud <- which(img.black %>% matrix(., dim(.)[1], dim(.)[2]) == FALSE, arr.ind = T)
  colnames(cloud) <- c("x", "y")
}

x_c <- mean(cloud[,'x'])

y_c <- mean(cloud[,'y'])

# max(cloud[,c('row')])
# max(cloud[,c('col')])

points(x=x_c, y=y_c, col='red', pch=4, cex=3)

J_p <- sum((cloud[,'x'] - x_c)^2 + (cloud[,'y'] - y_c)^2)

axis_distance <- 0
x_a <- max(cloud[,'x']) + axis_distance

J_a <- sum((cloud[,'x'] - x_a)^2)


img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.5))
abline(v = (max(cloud[,'x']) + axis_distance), col="red")







system.time(
  hpts <- chull(cloud)
)
# AreaX <- function(i,h=chull(i),j=c(h,h[1]))round((i[h,1]+i[j[-1],1])%*%diff(-i[j,2])/2)

Area3 <- function(x_1, y_1, x_2, y_2, x_3, y_3){
  abs((x_1 - x_3) * (y_2 - y_1) - (x_1 - x_2) * (y_3 - y_1))/2
}

S3 <-function(p){
  # abs(
  #     (cloud[p,"x"][1] - cloud[p,"x"][3])
  #   *
  #     (cloud[p,"y"][2] - cloud[p,"y"][1])
  #   -
  #     (cloud[p,"x"][1] - cloud[p,"x"][2])
  #   *
  #     (cloud[p,"y"][3] - cloud[p,"y"][1])
  # )/2

  x <- cloud[p,"x"]
  y <- cloud[p,"y"]
  
  abs((x[1] - x[3]) * (y[2] - y[1]) - (x[1] - x[2]) * (y[3] - y[1]))/2
  
    
  # abs(
  #   (cloud[p[1],"x"] - cloud[p[3],"x"])
  #   *
  #   (cloud[p[2],"y"] - cloud[p[1],"y"])
  #   -
  #   (cloud[p[1],"x"] - cloud[p[2],"x"])
  #   *
  #   (cloud[p[3],"y"] - cloud[p[1],"y"])
  # )/2

  # pp <- cloud[p,]
  # 
  #   abs(
  #     (pp[1,"x"] - pp[3,"x"])
  #   *
  #     (pp[2,"y"] - pp[1,"y"])
  #   -
  #     (pp[1,"x"] - pp[2,"x"])
  #   *
  #     (pp[3,"y"] - pp[1,"y"])
  # )/2
  
  # Area3(
  #   x_1=cloud[p,"x"][1],
  #   y_1=cloud[p,"y"][1],
  #   x_2=cloud[p,"x"][2],
  #   y_2=cloud[p,"y"][2],
  #   x_3=cloud[p,"x"][3],
  #   y_3=cloud[p,"y"][3]
  # )
}





# Area4 <- function(x_1, y_1, x_2, y_2, x_3, y_3, x_4, y_4){
#   Area3(x_1, y_1, x_2, y_2, x_3, y_3) + Area3(x_1, y_1, x_3, y_3, x_4, y_4)
# }
# 
# Area5<- function(x_1, y_1, x_2, y_2, x_3, y_3, x_4, y_4, x_5, y_5){
#   Area4(x_1, y_1, x_2, y_2, x_3, y_3, x_4, y_4) + Area3(x_1, y_1, x_4, y_4, x_5, y_5)
# }

system.time(
  comb <- combn(hpts, 3)
)

system.time({
  # areas <- combn(hpts, 3, function(p){S3(p)})
  a <- 0
  combn(hpts, 5, function(p){if((tmp <- S5(p)) > a) {pp <<- cloud[p,]; a <<- tmp}; 0})
})

img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
pp %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red"); points(., col='red', pch=4, cex=2, lwd=1)}

# colnames(comb)

system.time({
  comb <- combn(hpts, 3)
  areas <- apply(comb, 2, function(p){S3(p)})
  pp3 <- cloud[comb[,which.max(areas)],]
})

# areas <- apply(
#   comb,
#   2,
#   function(p){
#     # print(cloud[p,])
#     
#     S3(p)
#     # Area3(
#     #   x_1 = cloud[p,][,"x"][1],
#     #   y_1 = cloud[p,][,"y"][1],
#     #   x_2 = cloud[p,][,"x"][2],
#     #   y_2 = cloud[p,][,"y"][2],
#     #   x_3 = cloud[p,][,"x"][3],
#     #   y_3 = cloud[p,][,"y"][3]
#     #   
#     #   # x_1 = cloud[p,][,"y"][1],
#     #   # y_1 = cloud[p,][,"x"][1],
#     #   # x_2 = cloud[p,][,"y"][2],
#     #   # y_2 = cloud[p,][,"x"][2],
#     #   # x_3 = cloud[p,][,"y"][3],
#     #   # y_3 = cloud[p,][,"x"][3]
#     #   # 
#     #   
#     #   # x_2 = cloud[p[2], 2],
#     #   # y_2 = cloud[p[2], 1],
#     #   # x_3 = cloud[p[3], 2],
#     #   # y_3 = cloud[p[3], 1]
#     # )
#     # Area3(
#     #   x_1 = cloud[p[1], 2],
#     #   y_1 = cloud[p[1], 1],
#     #   x_2 = cloud[p[2], 2],
#     #   y_2 = cloud[p[2], 1],
#     #   x_3 = cloud[p[3], 2],
#     #   y_3 = cloud[p[3], 1]
#     # )
#   }
# )


# img %>% plot(., ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
# polygon(pp, col=rgb(0, 1, 1, 0.2))
# points(pp, col='red', pch=4, cex=2, lwd=1)

img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
pp3 %>% polygon(col=rgb(1, 0, 0, 0.2), border = "red")
pp3 %>% points(col='red', pch=4, cex=2, lwd=1)

comb <- combn(hpts, 4)

system.time(
  areas <- apply(comb, 2, function(p){S4(p)})
)

# areas <- apply(
#   comb,
#   2,
#   function(p){
#     S4(p)
#     # Area4(
#     #   x_1 = cloud[p[1], 2],
#     #   y_1 = cloud[p[1], 1],
#     #   x_2 = cloud[p[2], 2],
#     #   y_2 = cloud[p[2], 1],
#     #   x_3 = cloud[p[3], 2],
#     #   y_3 = cloud[p[3], 1],
#     #   x_4 = cloud[p[4], 2],
#     #   y_4 = cloud[p[4], 1]
#     # )
#   }
# )

pp4 <- cloud[comb[,which.max(areas)],]

# img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
pp4 %>% polygon(col=rgb(0, 1, 0, 0.2), border = "green")
pp4 %>% points(col='green', pch=4, cex=2, lwd=1)


comb <- combn(hpts, 5)

system.time(
  areas <- apply(comb, 2, function(p){S5(p)})
)

# areas <- apply(
#   comb,
#   2,
#   function(p){
#     S5(cloud[p,])
#     # Area5(
#     #   x_1 = cloud[p[1], 2],
#     #   y_1 = cloud[p[1], 1],
#     #   x_2 = cloud[p[2], 2],
#     #   y_2 = cloud[p[2], 1],
#     #   x_3 = cloud[p[3], 2],
#     #   y_3 = cloud[p[3], 1],  
#     #   x_4 = cloud[p[4], 2],
#     #   y_4 = cloud[p[4], 1],  
#     #   x_5 = cloud[p[5], 2],
#     #   y_5 = cloud[p[5], 1]  
#     # )
#   }
# )

pp5 <- cloud[comb[,which.max(areas)],]

# img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
pp5 %>% polygon(col=rgb(0, 0, 1, 0.2), border = "blue")
pp5 %>% points(col='blue', pch=4, cex=2, lwd=1)
