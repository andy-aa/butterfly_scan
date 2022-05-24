library(imager)
library(alphahull)
library(sp)
library("RTriangle")
# install.packages("devtools")
# library(devtools)
# install_github("babichmorrowc/hull2spatial")
# library(hull2spatial)
library(igraph)
library(pathmapping)

ashape2poly <- function(ashape){
  # Convert node numbers into characters
  ashape_graph <- graph_from_edgelist(
    matrix(as.character(ashape$edges[,1:2]), ncol=2),
    directed = FALSE
  )
  
  # ashape_graph <- graph_from_edgelist(
  #   cbind(
  #     as.character(ashape$edges[, "ind1"]), 
  #     as.character(ashape$edges[, "ind2"])
  #   ),
  #   directed = FALSE
  # )
  
  # print(ashape_graph)
  
  if (!is_connected(ashape_graph)) {
    stop("Graph not connected")
  }
  if (any(degree(ashape_graph) != 2)) {
    stop("Graph not circular")
  }
  if (clusters(ashape_graph)$no > 1) {
    stop("Graph composed of more than one circle")
  }
  # Delete one edge to create a chain
  cut_graph <- ashape_graph - E(ashape_graph)[1]
  # Find chain end points
  ends <- names(which(degree(cut_graph) == 1))
  path <- shortest_paths(cut_graph, ends[1], ends[2])$vpath[[1]]
  # this is an index into the points
  pathX <- as.numeric(V(ashape_graph)[path]$name)
  # join the ends
  # pathX <- c(pathX, pathX[1])
  # return(pathX)

  return(ashape$x[pathX,])
}



split_level <- .95

# image_name <- "files/Manduca quinquemaculatus/Manduca_quinquemaculatus_left_wing.png"
# image_name <- "files/Manduca rustica/Manduca_rustica_left_wing.png"
image_name <- "files/Agrius cingulata/Agrius_cingulata_left_wing.png"
# image_name <- "files/Hemaris diffinis/Hemaris_diffinis_left_wing.png"
# image_name <- "new_files/left_wing_a_fasciatus.png"
img <- load.image(image_name) %>% plot
img %<>% resize(., round(width(.)/100*3),round(height(.)/100*3)) %>% plot

# img <- resize_halfXY(img) %>% plot
img.gray <- grayscale(img) %>% plot
img.black <- as.cimg(img.gray >= split_level) %>% plot
img.edge <- cannyEdges(img.black) %>% plot

system.time({
  cloud <- which(img.black %>% matrix(., dim(.)[1], dim(.)[2]) == FALSE, arr.ind = T)
  colnames(cloud) <- c("x", "y")
  hull_pts <- cloud[chull(cloud),]
  cloud.edge <- which(img.edge %>% matrix(., dim(.)[1], dim(.)[2]) == TRUE, arr.ind = T)
  colnames(cloud.edge) <- c("x", "y")
  (img.ashape.obj <- ashape(cloud, alpha = .9)) %>% plot(., ylim=rev(range(.$x[,2])), xlab="", ylab="")
  cloud.ashape <- ashape2poly(img.ashape.obj)
  colnames(cloud.ashape) <- c("x", "y")
})

img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
cloud.ashape %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red"); points(., col='red', pch=20, cex=1, lwd=1)}
# cloud.ashape %>% polygon(., col = rgb(0, 1, 0, 0.2)) pch=20, cex=1, lwd=1


{# Alpha-shape exmple 
  par(mfrow=c(1,2))
  # img.ashape.obj %>% plot(., ylim=rev(range(.$x[,2])), xlab="", ylab="")
  cloud.ashape %>% {plot(., ylim=rev(range(.[,2])), xlab="", ylab="", xaxt='n', bty="n"); axis(1); polygon(., col=rgb(1, 0, 0, 0.2), border = "red"); points(., col='red', pch=20, cex=1, lwd=1)}
  # img.ashape.obj %>% plot(., ylim=rev(range(.$x[,2])), wpoints=FALSE, wlines='del', col=c('red', 0, rgb(0, 0, 1, 0.2), 1, 1, 1), xlab="", ylab="")
  cloud.ashape %>% {plot(triangulate(pslg(.), a=10), col="red", ylim=rev(range(.[,2])))}
   
}
axis(1); axis(2)

plot(triangulate(pslg(cloud.ashape), a=10), col="red", ylim=rev(range(cloud.ashape[,2]))); axis(1); axis(2)
cloud.ashape %>% {plot(triangulate(pslg(.), a=10), col="red", ylim=rev(range(.[,2])))}
cloud.ashape %>% pslg(.) %>% plot(triangulate(., a=10), col="red")

{# Alpha-shape exmple 1
  par(mfrow=c(1,2))
  img.ashape.obj %>% plot(., ylim=rev(range(.$x[,2])), wpoints=FALSE, wlines='del', col=c('red', 0, rgb(0, 0, 1, 0.2), 1, 1, 1))
  img.ashape.obj %>% plot(., ylim=rev(range(.$x[,2])), wpoints=FALSE, wlines='both')
}

surveyors(cloud.ashape)

minimize.alpha <- function(data, N=3){
  n <- (nrow(data)-N)
  
  for (j in 1:n) {
    s_0 <- surveyors(data)
    min <- s_0
    ind <- 1
    for (i in 1:nrow(data)) {
      s <- surveyors(data[-i,])
      if(abs(s_0-s) < min){
        min <- abs(s_0-s)
        ind <- i
      }
    }
    
    data <- data[c(-ind), ]
    cat(paste0('\r', j, " on ", n))
  }
  cat('\n')
  data
}



system.time(pp.alpha <- minimize.alpha(cloud.ashape, 10)) 

{ 
  par(mfrow=c(1,2))
  cloud.ashape %>% {plot(., type="p", pch=20, cex=.01, ylim=rev(range(.[,2])), xlab="", ylab="", xaxt='n', bty="n"); axis(1)}
  cloud.ashape %>% polygon(., col = rgb(0, 1, 0, 0.2))
  cloud.ashape %>% {plot(., type="p", pch=20, cex=.01, ylim=rev(range(.[,2])), xlab="", ylab="", xaxt='n', bty="n"); axis(1)}
  pp.alpha %>% polygon(., col = rgb(1, 0, 0, 0.2))
  pp.alpha %>% {points(.); points(., col='red', pch=20, cex=1, lwd=1)}
}

plot(pp.alpha)

# plot(cloud)
# plot(hull_pts)
# plot(cloud.edge)

# runif(100, minnrow(cloud.edge))

# x <- cloud.edge[sample(row(cloud.edge), 100),]

system.time({
  # ahull.obj <- ahull(minimize(hull_pts, 20)[20:1,], alpha = 100)
  ahull.obj <- ahull(cloud, alpha = 1)
  # ahull.obj <- ashape(cloud, alpha = 1)
})

areaahull(ahull.obj)

range(ahull.obj$ashape.obj$delvor.obj$tri.obj$y)

plot(cloud[ahull.obj$ashape.obj$alpha.extremes,], type="l")

ahull.obj$ashape.obj$edges


plot(ahull.obj, y=rev(range(ahull.obj$ashape.obj$delvor.obj$tri.obj$y)))
plot(ahull.obj, pch=4, number=TRUE)

plot(ahull.obj, do.shape = TRUE, wlines = "both", col = c(0, 'red', 0, 0, 0))
plot(ahull.obj, do.shape = FALSE, wlines = "both", col = c(0, 4, 0, 0, 0))
plot(ahull.obj, do.shape = FALSE, wlines = "none", col = c(6, 4, 1, 2, 3))




system.time({
  ashape.obj <- ashape(cloud, alpha = .9)
})


# plot(cloud)
ashape.obj %>% plot(., wpoints=T, wlines='none', col=c(1,6), ylim=rev(range((.)$x[,2])), xlab="", ylab="")

# plot(cloud[ashape.obj$alpha.extremes,], type="l")



# ashape2poly(ashape.obj)

ashape2poly(ashape.obj) 

cloud %>% {plot(., type="p", pch=20, cex=.01, ylim=rev(range(.[,2])), xlab="", ylab="", xaxt='n', bty="n"); axis(1)}
ashape2poly(ashape.obj) %>% polygon(., col = rgb(1, 0, 0, 0.2))


surveyors(ashape2poly(ashape.obj))

load.image(image_name) %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
ashape2poly(ashape.obj) %>% {./3*100-25} %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red")}
ashape2poly(ashape.obj) %>% {(.)/3*100} %>% {lines(., col="red"); points(., col='red', pch=20, cex=1, lwd=1)}


img %>% plot(ylim=c(dim(.)[2]*1.1, -dim(.)[2]*.1), xlim=c(-dim(.)[1]*.1, dim(.)[1]*1.1))
ashape2poly(ashape.obj) %>% {polygon(., col=rgb(1, 0, 0, 0.2), border = "red")}
ashape2poly(ashape.obj) %>% {lines(., col="red"); points(., col='red', pch=20, cex=1, lwd=1)}

plot(triangulate(pslg(ashape2poly(ashape.obj)), a=50), col="red", add=T)
axis(2); axis(1)


# plot(cloud[ashape2poly(ashape.obj),], type="l")

# plot(ashape.obj$x[ashape2poly(ashape.obj),], type="l")

# bds <- ahull.obj$ashape.obj$x  # matrix of coordinates in ashape
# bds <- rbind(bds, bds[1,])          # close the ring
# plot(bds)
# ashape <- Polygon(bds)            # convert to sp Polygon
# plot(ashape)













library(move)
library(ggmap)
# Data from Movebank
# Study Name: Dunn Ranch Bison Tracking Project
# Principal Investigator: Stephen Blake, Randy Arndt, Doug Ladd
# Max Planck Institute for Ornithology Radolfzell Germany

study <- "Dunn Ranch Bison Tracking Project"
cainfo <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
options(RCurlOptions = list(verbose = FALSE, capath = cainfo, ssl.verifypeer = FALSE))
# Login to movebank (first create the login object)
curl <- movebankLogin(username = "xxx", password = "zzz")
# Downloads study stored in Movebank
track <- getMovebankData(study = study, login = curl)
dat <- track@data[track@data[, "deployment_id"] == 13848432,]
# Map of animal locations
bbox <- ggmap::make_bbox(dat[,"location_long"], dat[,"location_lat"], f = 0.3)
map_loc <- get_map(location = bbox, source = "google", maptype = 'satellite')
map <- ggmap(map_loc, extent = 'panel', maprange=FALSE)
p <- map + geom_path(data = dat, aes(x = location_long, y = location_lat), col=2, size=0.3)
p
ah_gp <- ahull_track(x = dat[, c("location_long", "location_lat")], alpha = 0.005)
p + ah_gp