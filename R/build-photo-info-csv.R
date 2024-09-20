# Creates the photo_info.csv file by listing all the available images in the relevent directories
library(jpeg)

# Find all background images
# Convert URLS to relative to the base directory
backs <- data.frame(URL = file.path("images/backgrounds", list.files("../images/backgrounds", "*.JPG")), What = "Back")

# Check photo aspect ratios
cat("Checking background aspect ratios...\n")
jpgDim <- sapply(file.path("..", backs$URL), function(fn) dim(readJPEG(fn, native = TRUE)))
ar <- jpgDim[1, ] / jpgDim[2, ]
assAR <- 333 / 500
badAR <- sapply(ar, function(a) !isTRUE(all.equal(a, assAR, check.attributes = FALSE, tolerance = 0.001)))
if (any(badAR)) {
  cat(sprintf("Backgrounds are assumed to have aspect ratio of %g. Incorrect aspect ratios:\n", assAR))
  print(data.frame(File = basename(backs$URL[badAR]), AR = ar[badAR], Width = jpgDim[2, badAR], Height = jpgDim[1, badAR]), row.names = FALSE)
} else {
  cat("All good!\n")
}

# Find all butterfly images
butts <- data.frame(URL = file.path("images/butterflies", list.files("../images/butterflies", "*.png")), What = "Butt")

all <- rbind(backs, butts)

# Save it as a CSV
write.csv(all, "../photo_info.csv", row.names = FALSE)

cat(sprintf("Found %d butterflies and %d backgrounds\n", nrow(butts), nrow(backs)))
