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

cloud <- which(
  matrix(img.black, dim(img.black)[1], dim(img.black)[2]) == FALSE,
  arr.ind = T
)

colnames(cloud) <- c("x", "y")

x_c <- mean(cloud[,'x'])

y_c <- mean(cloud[,'y'])

# max(cloud[,c('row')])
# max(cloud[,c('col')])

points(x=x_c, y=y_c, col='red', pch=4, cex=3)


J <- sum((cloud[,c('x')] - x_c)^2 + (cloud[,c('y')] - y_c)^2)


hpts <- chull(cloud)

# AreaX <- function(i,h=chull(i),j=c(h,h[1]))round((i[h,1]+i[j[-1],1])%*%diff(-i[j,2])/2)



S3 <-function(p){
  abs((cloud[p,][1,"x"] - cloud[p,][3,"x"]) * (cloud[p,][2,"y"] - cloud[p,][1,"y"]) - (cloud[p,][1,"x"] - cloud[p,][2,"x"]) * (cloud[p,][3,"y"] - cloud[p,][1,"y"]))/2
  # Area3 <- function(x_1, y_1, x_2, y_2, x_3, y_3){
  #   abs((x_1 - x_3) * (y_2 - y_1) - (x_1 - x_2) * (y_3 - y_1))/2
  # }
  # Area3(
  #   x_1=cloud[p,][1,"x"],
  #   y_1=cloud[p,][1,"y"],
  #   x_2=cloud[p,][2,"x"],
  #   y_2=cloud[p,][2,"y"],
  #   x_3=cloud[p,][3,"x"],
  #   y_3=cloud[p,][3,"y"]
  # )
}

S4 <- function(p){
  S3(p[c(1,2,3)]) + S3(p[c(1,3,4)])
}

S5 <- function(p){
  S4(p[c(1,2,3,4)]) + S3(p[c(1,4,5)])
}



# Area4 <- function(x_1, y_1, x_2, y_2, x_3, y_3, x_4, y_4){
#   Area3(x_1, y_1, x_2, y_2, x_3, y_3) + Area3(x_1, y_1, x_3, y_3, x_4, y_4)
# }
# 
# Area5<- function(x_1, y_1, x_2, y_2, x_3, y_3, x_4, y_4, x_5, y_5){
#   Area4(x_1, y_1, x_2, y_2, x_3, y_3, x_4, y_4) + Area3(x_1, y_1, x_4, y_4, x_5, y_5)
# }


comb <- combn(hpts, 3)
# colnames(comb)

system.time(
  areas <- apply(comb, 2, function(p){S3(p)})
)

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

pp3 <- cloud[comb[,which.max(areas)],]

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

img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
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

img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
pp5 %>% polygon(col=rgb(0, 0, 1, 0.2), border = "blue")
