library("alphahull")
library("RTriangle")
library("imager")

border_level <- .85

img <- load.image("new_files/left_wing_a_fasciatus.png") %>% plot
img.gray <- grayscale(img) %>% plot
img.black <- (img.gray >= border_level) %>% plot

# lines(triangulate(pslg(cloud[hpts,]), a=6000), col="red")

cloud <- which(
  matrix(img.black, dim(img.black)[1], dim(img.black)[2]) == FALSE,
  arr.ind = T
)

hpts <- chull(cloud)

lines(cloud[c(hpts, hpts[1]), ], col="red")

points(cloud[hpts,], pch=4, col="red")


# data <- rbind(c(0, 0), c(0, 1), c(0.5, 0.5), c(1, 1), c(1, 0))
# data <- cloud[hpts, ]

# p <- pslg(cloud[hpts,])

plot(p)
# plot(triangulate(p))
plot(triangulate(pslg(cloud[hpts,]), a=5000), col="red", ylim=c(1000, -10))
# lines(p)
axis(2); axis(1)




ahull.obj <- ahull(cloud[hpts,], alpha = .005)
plot(ahull.obj)







typeof(cloud[c(hpts, hpts[1]), ])

plot(p)
tp <-triangulate(p)

plot(tp)

tp <- triangulate(p, a=0.1)
plot(triangulate(p, a=0.01))
