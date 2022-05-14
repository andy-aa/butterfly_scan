suppressPackageStartupMessages(library(imager))
suppressPackageStartupMessages(library(svMisc))


body_volume <- function(file_name, split_level = .85, pixel_size = 1){
  
  distance <- function(x_1, y_1, x_2, y_2){
    ((x_1 - x_2)^2 + (y_1 - y_2)^2)^.5
  }
  
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
      distance(
        x_1 = cloud[p[1], 2],
        y_1 = cloud[p[1], 1],
        x_2 = cloud[p[2], 2],
        y_2 = cloud[p[2], 1]
      )
    }
  )
  
  pp <- cloud[comb[,which.max(distances)],]
  
  rotation_angle <- atan((pp[1,2] - pp[2,2]) / (pp[1,1] - pp[2,1])) * 180 / pi
  
  img.rotated <- imrotate(img, -90-rotation_angle, boundary=1)
  
  img.white <- (grayscale(img.rotated) < split_level)
  
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

# df <- data.frame(matrix(ncol = 2, nrow = 0))
# colnames(df) <- c('file','volume')
# for(file in list.files(path = "files", pattern = "*body\\.(png|jpg)$")){
#   df[nrow(df) + 1,] <- c(file, body_volume(paste0('files/', file)))
# }
# write.csv(df, "body.csv", row.names = FALSE)
# 
# 
# df <- data.frame(matrix(ncol = 2, nrow = 0))
# colnames(df) <- c('file','inertia')
# for(file in list.files(path = "files", pattern = "*left_wing\\.(png|jpg)$")){
#   df[nrow(df) + 1,] <- c(file, body_volume(paste0('files/', file)))
# }
# write.csv(df, "left_wing.csv", row.names = FALSE)
# 
# df <- data.frame(matrix(ncol = 2, nrow = 0))
# colnames(df) <- c('file','inertia')
# for(file in list.files(path = "files", pattern = "*right_wing\\.(png|jpg)$")){
#   df[nrow(df) + 1,] <- c(file, body_volume(paste0('files/', file)))
# }
# write.csv(df, "right_wing.csv", row.names = FALSE)


# df <- (function(names) setNames(data.frame(matrix(ncol = length(names), nrow = 0)), names))(c('Name','inertia'))


# df <- (function(names) setNames(data.frame(matrix(ncol = length(names), nrow = 0)), names))(c('Name','inertia'))


# colnames(data.frame(matrix(ncol = 2, nrow = 0)))<- c('Name','inertia')

# df <- data.frame(matrix(ncol = 2, nrow = 0))
# colnames(df) <- c('Name','inertia')

file_path <- function(postfix, subfolder, base_folder="files"){
  list.files(
    path = paste0(base_folder, "/", subfolder), 
    pattern = paste0("*", postfix, "\\.(png|jpg)$")
    )[1]
}

c('Name', 
  'body_volume', 
  "right_wing_inertia", 
  "left_wing_inertia"
) %>% setNames(data.frame(matrix(ncol = length(.), nrow = 0)), .) -> df

dirs <- list.dirs(path = "files", full.names = FALSE, recursive=FALSE)

i <- 0; n <- length(dirs)

for(dir in dirs){
  progress(i <- i + 1, n)
  
  file_right_wing <- file_path("right_wing", dir)
  file_left_wing <- file_path("left_wing", dir)
  file_body <- file_path("body", dir)

  df[nrow(df) + 1,] <- c(
    dir,
    
    if(is.na(file_body)) 
      NA 
    else 
      body_volume(paste0("files/", dir, "/", file_body)),
    
    if(is.na(file_right_wing)) 
      NA 
    else 
      wing_inertia(paste0("files/", dir, "/", file_right_wing)),
    
    if(is.na(file_left_wing)) 
      NA 
    else 
      wing_inertia(paste0("files/", dir, "/", file_left_wing))
  )
  
}

write.csv(df, "data.csv", row.names = FALSE)
message("Done!")
