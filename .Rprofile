# R won't load two different .Rprofiles
# https://support.rstudio.com/hc/en-us/community/posts/200657076-Also-load-the-user-s-Rprofile-when-opening-a-project-with-a-project-specific-Rprofile
if (file.exists("/Users/andrew/.Rprofile")) {
  source("/Users/andrew/.Rprofile")
}

.First <- function() {
  days.left <- as.numeric(lubridate::ymd("2017-06-01") - lubridate::today())
  
  cat("\n          ---------------------------------------------------------")
  cat("\n          😱 Write day and night like you're running out of time 😱 ")
  cat("\n          ---------------------------------------------------------\n")
  cat("\n                       💀 Days until the reckoning:", days.left, "💀\n\n")
}

# Yay @jennybc!
# https://gist.github.com/jennybc/362f52446fe1ebc4c49f
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)

# cat("sourcing project-specific .Rprofile\n")
cat('Retrieve the top-level project directory with PROJHOME:',
    get("PROJHOME", "RPROJ"), "\n")

rm(RPROJ)

# Someday use rprojroot instead
# library(rprojroot)
# PROJHOME <- find_root(is_rstudio_project)
# OR
# R <- find_rstudio_root_file
# R("Data", "data_raw")
#### -- Packrat Autoloader (version 0.4.8-1) -- ####
source("packrat/init.R")
#### -- End Packrat Autoloader -- ####
