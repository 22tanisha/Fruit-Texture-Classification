
glcm_feature <- function(fileName)
  {
library(glcm)
library(raster)
r <- raster(fileName)
 
images <- glcm(r, n_grey=32, window=c(3, 3), shift=c(1, 1),
                 statistics=c('entropy'), min_x=NULL, max_x=NULL, na_opt="any", na_val=NA, scale_factor=1, asinteger=FALSE) 
 

images <- na.omit(images[,])

}