# Note: It's best to run this on a separate server because it takes a really
# long time. I created a fast VPS at DigitalOcean, installed Docker, installed
# the rocker/hadleyverse image, ran this through RStudio in a browser,
# downloaded all the .rds files locally, and then killed the VPS.

library(cshapes)  # Has to come before dplyr because it uses plyr
library(parallel)

get.distances <- function(year.to.get) {
  year.to.get <- as.Date(paste0(year.to.get, "-01-01"))
  
  dmat.min <- cshapes::distmatrix(year.to.get, type="mindist", useGW=FALSE)
  saveRDS(dmat.min, paste0("min_", year.to.get, ".rds"))
  
  dmat.capital <- cshapes::distmatrix(year.to.get, type="capdist", useGW=FALSE)
  saveRDS(dmat.capital, paste0("capital_", year.to.get, ".rds"))
  
  dmat.centdist <- cshapes::distmatrix(year.to.get, type="centdist", useGW=FALSE)
  saveRDS(dmat.capital, paste0("cent_", year.to.get, ".rds"))
}

# Calculate the number of cores
no_cores <- detectCores()

# Initiate cluster
cl <- makeCluster(no_cores, type="FORK")

# Calculate all three distances for each year
parSapply(cl, 1990:2012, get.distances)

# Kill the cluster
stopCluster(cl)
