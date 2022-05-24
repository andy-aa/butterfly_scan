library("alphahull")
library("RTriangle")
library("imager")

border_level <- .85

img <- load.image("new_files/left_wing_a_fasciatus.png") %>% plot
# img <- load.image("files/Agrius cingulata/Agrius_cingulata_body.png") %>% plot
img <- resize_halfXY(img) %>% plot
img.gray <- grayscale(img) %>% plot
img.black <- (img.gray >= border_level) %>% plot
img.edge <- cannyEdges(img.black) %>% plot


{
  cloud <- which(img.black %>% matrix(., dim(.)[1], dim(.)[2]) == FALSE, arr.ind = T)
  colnames(cloud) <- c("x", "y")
  hull_pts <- cloud[chull(cloud),]
  cloud.edge <- which(img.edge %>% matrix(., dim(.)[1], dim(.)[2]) == TRUE, arr.ind = T)
  colnames(cloud.edge) <- c("x", "y")
}

# lines(triangulate(pslg(cloud[hpts,]), a=6000), col="red")

# cloud <- which(
#   matrix(img.black, dim(img.black)[1], dim(img.black)[2]) == FALSE,
#   arr.ind = T
# )

hpts <- chull(cloud)

lines(cloud[c(hpts, hpts[1]), ], col="red")

points(cloud[hpts,], pch=4, col="red")


# data <- rbind(c(0, 0), c(0, 1), c(0.5, 0.5), c(1, 1), c(1, 0))
# data <- cloud[hpts, ]

# p <- pslg(cloud[hpts,])

# plot(p)
# plot(triangulate(p))
plot(triangulate(pslg(cloud[hpts,]), a=5000), col="red", ylim=c(1000, -10))
axis(2); axis(1)

plot(triangulate(pslg(cloud.edge), a=1000), col="red", ylim=c(1000, -10))
axis(2); axis(1)

system.time({
  ahull.obj <- ahull(cloud.edge, alpha = .1)
})

plot(ahull.obj)
points(ahull.obj$ashape.obj$x[,1])


matrix(cloud[hpts,], ncol=2)

system.time(
  ahull.obj <- ahull(matrix(cloud, ncol=2), alpha = 350.1)
)

plot(ahull.obj, ylim=c(1000, -10))


plot(cloud[hpts,])





typeof(cloud[c(hpts, hpts[1]), ])

plot(p)
tp <-triangulate(p)

plot(tp)

tp <- triangulate(p, a=0.1)
plot(triangulate(p, a=0.01))
