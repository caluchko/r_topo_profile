# plots cross section of a raster matrix given two endpoints of a line

xsec <- function (data, x1, y1, x2, y2){
  d <- sqrt((x2-x1)^2 + (y2-y1)^2)
  print (d)
  a <- (y2-y1)/(x2-x1)
  print (a)
  d.int <- as.integer(d) #iterator
  print(d.int)
  x <- vector()
  y <- vector()
  z <- vector()
  for (i in 1:d.int){
    xn <- i/sqrt(a^2 + 1)
    x <- c(x, x1+xn)
    yn <- a*(xn)
    y <- c(y, y1+yn)
    zn <- data[x[i], y[i]]
    z <- c(z, zn)
  }
  z <- c(data[x1,y1], z)
  d.int <- c(0, 1:d.int)
  print (length(x))
  print (length(y))
  print (length(z))
  print (length(d.int))
  par(ask = TRUE)
  plot(x,y, type = "l", xlim = c(0,nrow(data)), ylim = c(0,ncol(data))) #need to get scale 1:1
  plot (d.int, z, type = "l")
}
# does not work when slope is undefined