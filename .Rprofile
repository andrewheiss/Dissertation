# R won't load two different .Rprofiles
# https://support.rstudio.com/hc/en-us/community/posts/200657076-Also-load-the-user-s-Rprofile-when-opening-a-project-with-a-project-specific-Rprofile
if (file.exists("/Users/andrew/.Rprofile")) {
  source("/Users/andrew/.Rprofile")
}

.First <- function() {
  cat("\n          ---------------------------------------------------------")
  cat("\n          😱 Write day and night like you're running out of time 😱 ")
  cat("\n          ---------------------------------------------------------\n\n")
}

# Yay @jennybc!
# https://gist.github.com/jennybc/362f52446fe1ebc4c49f
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)

cat("sourcing project-specific .Rprofile\n")
cat('retrieve the top-level project directory at any time with PROJHOME or via get("PROJHOME", "RPROJ"):\n',
    get("PROJHOME", "RPROJ"), "\n")

rm(RPROJ)
