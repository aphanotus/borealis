# Simulate TPS data for geomorph analysis
# Dave Angelini 2018-03-22 

rm(list=ls()) # Clear memory
package.list <- c('srtingr')
for (i in package.list) { if(!require(i)) { install.packages(i); library(i) } }

# Based on the posterior edge of Jadera haematoloma wings in a typical lab image
# 2592 × 1944

sample.size <- 1000
landmark.number <- 35
set.seed(314159)

# seed LW shape
lwx <- c(1023,1096.5,1170,1252.5,1335,1413,1491,1573.5,1656,1762.5,1869,1972.5,2076,2163,2250,2296.5,2343,2317.5,2292,2220,2148,2061,1974,1878,1782,1702.5,1623,1543.5,1464,1396.5,1329,1263,1197,1111.5,1026)
lwy <- c(1082,1100,1118,1127,1136,1134.5,1133,1128.5,1124,1109,1094,1067,1040,1001,962,888.5,815,738.5,662,623,584,555.5,527,515,503,497,491,495.5,500,503,506,513.5,521,546.5,572)
# seed SW shape
swx <- c(1113,1177.5,1242,1312.5,1383,1447.5,1512,1570.5,1629,1684.5,1740,1797,1854,1845,1836,1816.5,1797,1776,1755,1710,1665,1623,1581,1537.5,1494,1452,1410,1368,1326,1296,1266,1239,1212,1170,1128)
swy <- c(1160,1178,1196,1199,1202,1197.5,1193,1187,1181,1169,1157,1131.5,1106,1058,1010,978.5,947,912.5,878,884,890,897.5,905,908,911,914,917,924.5,932,951.5,971,989,1007,1028,1049)

cartesian.coord.matrix <- array(dim=c(landmark.number,2,sample.size), 
                                dimnames= list( 
                                  c(1:landmark.number), # row names 
                                  c("X", "Y"),          # column names 
                                  c(paste('ID',str_pad(1:sample.size, 
                                                       nchar(sample.size), 
                                                       pad = "0"),
                                          sep='_')) ) )

for (i in 1:sample.size) {
  # Odds will be LW; Evens SW
  x <- lwx; y <- lwy
  if (i %% 2) { x <- swx; y <- swy } 
  # Every 11th bug is intermediate with a random weight towards either morph
  if (i %% 11 == 0) { 
    weight <- abs(1-abs(rnorm(1,0,0.25))) # ensures values range 0 to 1
    if (i %% 2) { weight <- 1-weight } # Alternate weight in favor of each morph
    x <- swx*weight + lwx*(1-weight)
    y <- swy*weight + lwy*(1-weight)
  } 
  # plot(x,y, xlim=c(0,2592), ylim=c(0,1944))
  mean.x <- mean(x); mean.y <- mean(y)
  # Normalize
  x <- x - mean.x; y <- y - mean.y
  # Random resize
  scale.factor <- rnorm(1,1,0.03)
  x <- x * scale.factor
  y <- y * scale.factor
  # Random stretch in each dimension
  x <- x * rnorm(1,1,0.03)
  y <- y * rnorm(1,1,0.03)
  # Every 71st bug is outlier
  if (i %% 71 == 0) { 
    x <- x * rnorm(1,1,0.1)
    y <- y * rnorm(1,1,0.1)
  } 
  # Random rotation
  theta <- runif(1, -pi/12, pi/12)  # up to 15 degrees either way
  x.prime <-  x*cos(theta) + y*sin(theta)
  y.prime <- -x*sin(theta) + y*cos(theta)
  x <- x.prime; y <- y.prime
  # Translocate
  x <- x + rnorm(1,0,50)
  y <- y + rnorm(1,0,50)
  # Add noise
  for (j in 1:landmark.number) { x[j] <- x[j] + rnorm(1,0,3.5)}
  for (j in 1:landmark.number) { y[j] <- y[j] + rnorm(1,0,3.5)}
  # Add additional noise to individual points, randomly
  vector.position <- (rpois(landmark.number, 1) > 2) # Affects ~ 7.5% of points
  x[vector.position] <- x[vector.position] + rnorm(1,0,10)
  y[vector.position] <- y[vector.position] + rnorm(1,0,10)
  # Return to the original field
  x <- x + mean.x; y <- y + mean.y
  # Record the values in the matrix 
  cartesian.coord.matrix[,1,i] <- x
  cartesian.coord.matrix[,2,i] <- y
  # Show them
  # points(x,y,col="darkred", pch=16)
}

sink(file='sample.tps')
for (i in 1:sample.size) {
  cat('LM=',landmark.number,'\n',sep='')
  for (j in 1:landmark.number) {
    cat(cartesian.coord.matrix[j,1,i],' ',cartesian.coord.matrix[j,2,i],'\n',sep='')
  }
  cat('ID=',dimnames(cartesian.coord.matrix)[[3]][i],'\n\n',sep='')
}  
sink()

# Clean up
rm(package.list,sample.size,landmark.number,
   lwx,lwy,swx,swy,x,y,weight,
   cartesian.coord.matrix,
   mean.x, mean.y, x.prime, y.prime,
   scale.factor, theta,
   cartesian.coord.matrix
   )
