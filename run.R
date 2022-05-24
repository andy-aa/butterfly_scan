library(imager)

d2 <- function(data){
  x <- data[,"x"]
  y <- data[,"y"]
  
  sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2)
}

body_volume <- function(file_name, split_level = .85, pixel_size = 1){
  
  img <- load.image(file_name)
  img.black <- (grayscale(img) >= split_level)
  
  cloud <- which(img.black %>% matrix(., dim(.)[1], dim(.)[2]) == FALSE, arr.ind = T)
  colnames(cloud) <- c("x", "y")
  
  hpts <- chull(cloud)
  
  comb <- combn(hpts, 2)
  
  distances <- apply(comb, 2,  function(p){d2(cloud[p,])})
  
  pp <- cloud[comb[,which.max(distances)],]
  
  rotation_angle <- atan((pp[1,'y'] - pp[2,'y']) / (pp[1,'x'] - pp[2,'x'])) * 180 / pi

  img.rotated <- imrotate(img, -90-rotation_angle, boundary=1)
  
  img.white <- (grayscale(img.rotated) < split_level)
  
  volume <- sum(pi * (rowSums(img.white) / 2 * pixel_size) * pixel_size) 
  
  height <- max(rowSums(img.white))
  width <- max(colSums(img.white))
  area <- sum(img.white)
  
  # return(volume)
  return(list(
    volume=volume,
    area=area,
    height=height,
    width=width
  ))
  
}

wing_inertia <- function(file_name, split_level = .85, left=TRUE){
  
  img.black <- (grayscale(load.image(file_name)) >= split_level)
  
  cloud <- which(img.black %>% matrix(., dim(.)[1], dim(.)[2]) == FALSE, arr.ind = T)
  colnames(cloud) <- c("x", "y")

  x <- cloud[,c('x')]
  y <- cloud[,c('y')]
  
  x_c <- mean(x)
  y_c <- mean(y)
  
  J_p <- sum((x - x_c)^2 + (y - y_c)^2)
  
  x_a <- if(left) max(x) else min(x)
  
  J_a <- sum((x - x_a)^2)
  
  
  return(list(
    J_p=J_p,
    J_a=J_a,
    area=length(cloud)
  ))
}


file_path <- function(postfix, subfolder, base_folder="files"){
  list.files(
    path = paste0(base_folder, "/", subfolder), 
    pattern = paste0("*", postfix, "\\.(png|jpg)$")
    )[1]
}

c('Name', 

  'body_volume', 
  'body_area',
  'body_height',
  'body_width',
  
  "right_wing_inertia_polar", 
  "right_wing_inertia_axis", 
  "right_wing_area",
  
  "left_wing_inertia_polar", 
  "left_wing_inertia_axis", 
  "left_wing_area"
) %>% setNames(data.frame(matrix(ncol = length(.), nrow = 0)), .) -> df

dirs <- list.dirs(path = "files", full.names = FALSE, recursive=FALSE)

# system.time({
  
i <- 0; n <- length(dirs) 

for(dir in dirs){
  cat(paste0('\r', i<- i + 1, " on ", n))
  
  file_right_wing <- file_path("right_wing", dir)
  file_left_wing <- file_path("left_wing", dir)
  file_body <- file_path("body", dir)

  df[nrow(df) + 1,] <- c(
    dir,
    
    if(is.na(file_body)) 
      rep(NA, 4)
    else
      as.vector(body_volume(paste0("files/", dir, "/", file_body))),
    
    if(is.na(file_right_wing)) 
      rep(NA, 3) 
    else
      as.vector(wing_inertia(paste0("files/", dir, "/", file_right_wing), left = FALSE)),
    
    if(is.na(file_left_wing)) 
      rep(NA, 3) 
    else 
      as.vector(wing_inertia(paste0("files/", dir, "/", file_left_wing), left = TRUE))
  )

}

# })

write.csv(df, "data.csv", row.names = FALSE)
message("\nDone!")
