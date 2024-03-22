library(ambient)

# set up standard flow field 
flow_setup <- function(nx = 200, ny = 200, n_curves = 15000, size = 2,
                       n_steps = 20, step_length = 1, noise = c("perlin", "worley", "cubic", "simplex", "value"),
                       frequency = 0.01, fractal = "fbm", octaves = 3,
                       pal = c("#6d2f20","#b75347","#df7e66","#e09351","#edc775","#94b594","#224b5e"),
                       seed = 12) {
  #' @description This function makes a flow field dataset, using different noise algos from ambient
  #' @param noise types of noise: perlin, worley, cubic, simplex, or value
  #' @param fractal types are: none, fbm, billow, rigid-multi
  #' 
  # first we set up the initial variables
  set.seed(seed)
  nx <- nx # number of x vars
  ny <- ny # number of y vars
  n_curves <- n_curves # number of curves (flow elements in the field)
  n_steps <- n_steps # number of iterations
  step_length <- step_length
  limit <- n_steps * step_length
  pal <- pal
  
  # next we select the type of noise and create a matrix with nx * ny, vals = angle of noise
  noise <- match.arg(noise)
  
  if (noise == "perlin") {
    pnt <- array(ambient::noise_perlin(c(nx, ny),
                                       frequency = frequency,
                                       fractal = fractal,
                                       octaves = octaves), 
                 dim = c(nx, ny),
                 dimnames = list(1:nx, 1:ny)) * pi * 4
    
  } else if (noise == "worley") {
    pnt <- array(ambient::noise_worley(c(nx, ny),
                                       frequency = frequency,
                                       fractal = fractal,
                                       octaves = octaves), 
                 dim = c(nx, ny),
                 dimnames = list(1:nx, 1:ny)) * pi * 4
    
  } else if (noise == "cubic") {
    pnt <- array(ambient::noise_cubic(c(nx, ny),
                                      frequency = frequency,
                                      fractal = fractal,
                                      octaves = octaves), 
                 dim = c(nx, ny),
                 dimnames = list(1:nx, 1:ny)) * pi * 4
    
  } else if (noise == "simplex") {
    pnt <- array(ambient::noise_simplex(c(nx, ny),
                                        frequency = frequency,
                                        fractal = fractal,
                                        octaves = octaves), 
                 dim = c(nx, ny),
                 dimnames = list(1:nx, 1:ny)) * pi * 4
    
  } else if (noise == "value") {
    pnt <- array(ambient::noise_value(c(nx, ny),
                                      frequency = frequency,
                                      fractal = fractal,
                                      octaves = octaves), 
                 dim = c(nx, ny),
                 dimnames = list(1:nx, 1:ny)) * pi * 4
    
  } else {
    print("Please provide one of the following types of noise: perlin, worley, cubic, simplex, or value")
  }
  
  # next we make a matrix for the lines and specify the start and end points
  dat <- matrix(nrow = n_curves * n_steps, ncol = 8)
  colnames(dat) <- c("x_start", "y_start", "a", "x_end", "y_end", "l", "i", "colour")
  
  # loop for calculations
  for (l in 1:n_curves-1) { # loop through lines
    # pick random start points
    x_start = runif(1, min = limit, max = nx - (limit))
    y_start = runif(1, min = limit, max = ny - (limit))
    # get angle from mat (pnt)
    a = pnt[x_start, y_start]
    # get random colour from pal
    colour <- sample(pal, 1)
    
    for (i in 1:n_steps) { # loop through steps for each line
      # calc end point for each step
      x_end = x_start + step_length * cos(a)
      y_end = y_start + step_length * sin(a)
      # write first row of points and angle
      dat[i + l * n_steps, 1] = x_start
      dat[i + l * n_steps, 2] = y_start
      dat[i + l * n_steps, 3] = a
      dat[i + l * n_steps, 4] = x_end
      dat[i + l * n_steps, 5] = y_end
      dat[i + l * n_steps, 6] = l
      dat[i + l * n_steps, 7] = i
      dat[i + l * n_steps, 8] = colour
      # make starting point from prev end point with new angle
      x_start = x_end
      y_start = y_end
      a = pnt[x_start, y_start]
    }
  }
  
  # convert mat to df
  dat_df <- as.data.frame(dat)
  
  # change cols to numeric (except colour)
  convert <- colnames(dat_df[1:7])
  dat_df[, convert] <- sapply(convert, function(x) as.numeric(dat_df[[x]]))
  
  # add new column that makes a size var based on angle - default is angle * 2
  dat_df$size <- abs(dat_df$a) * size
  
  # just return the final data frame
  return(dat_df)
  
}

# set up another flow field with different parameters
# aim 1: try and make sure there isn't an overlap of particles
# aim 2: have particles coming in and out using larger limits

# minimum distance with no overlap
library(FNN)

remove_overlap <- function(flow_data, k_max = 8) {
  # use data made by flow_setup() function
  # set up empty vector
  x <- vector("double", nrow(flow_data[, 1:2]))
  # min knn distance (found 2 is good amount)
  knnDist <- 2
  
  # loop over each x_start and y_start and find nearest k. Save back into x
  for (knear in seq_along(1:nrow(flow_data))) {
    x[knear] <- FNN::knnx.dist(flow_data[, 1:2], dat_df[knear, 1:2],
                               k = k_max, algorithm = "kd_tree")[k_max] < knnDist
  }
  # add x back to the flow data
  flow_data$knn <- x
  
  flow_data <- subset(flow_data, knn != 1)
  
  return(flow_data)
}

# https://coolors.co/
coolors <- function(URL){
  # function takes coolors url, extracts hex codes and adds #
  # detects if from default or explored palettes
  if (grepl("palette", URL) == TRUE) {
    # extract just the hex
    cstr <- gsub("coolors.co", "", URL)
    cstr <- gsub("https:///", "", cstr)
    cstr <- gsub("palette/", "", cstr)
    cstr <- gsub("-", " ", cstr)
    
    # split into individual strings and add #
    cvec <- strsplit(cstr, " ")
    cvec <- paste0("#", cvec[[1]])
    
    return(cvec)
    
  } else {
    # extract just the hex
    cstr <- gsub("coolors.co", "", URL)
    cstr <- gsub("https:///", "", cstr)
    cstr <- gsub("-", " ", cstr)
    
    # split into individual strings and add #
    cvec <- strsplit(cstr, " ")
    cvec <- paste0("#", cvec[[1]])
    
    return(cvec)
  }
}

