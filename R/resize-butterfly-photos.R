# Resizes butterfly photos so they all have similar numbers of non-transparent pixels

library(imager)

##################################################################################
# Check and correct butterfly images sizes


scaleAndSave <- function(fn, scale, on) {
  x <- load.image(fn)
  
  # Scale; the height is determined automatically so that
  # the aspect ratio is preserved
  y <- imresize(x, scale, interpolation = 6)
  
  # show the scaled image
  #display(y)
  
  save.image(y, on)
}

plotAll <- function(dir) {
  bf <- list.files(dir, "*.png", full.names = T)
  buts <- sapply(bf, function(fn) readPNG(fn, native = FALSE))
  # Count no. of non-transparent pixels
  notTrans <- sapply(buts, function(but) { sum(but[,,4] > 0.5) })
  buts <- buts[order(notTrans)]
  par(mar = c(0, 0, 0, 0))
  plot(NULL, xlim = c(1, 13), ylim = c(1, 12), axes = F, xlab = "", ylab = "")
  for (i in seq_along(buts)) {
    JPlotRaster(buts[[i]], i %% 12 + 1.5, i %/% 12 + 1.5, width = 0.7)
  }
}

# Read all the images in Big butts and resize them into Good butts
bf <- list.files("../images/Big butts", "*.png", full.names = T)
buts <- sapply(bf, function(fn) readPNG(fn, native = FALSE))
#butSz <- sapply(buts, function(but) dim(but)[1] * dim(but)[2])
# Count no. of non-transparent pixels
notTrans <- sapply(buts, function(but) { sum(but[,,4] > 0.5) })

targetSz <- 30000

for (i in seq_along(bf)) {
  print(i)
  scaleAndSave(bf[i], sqrt(targetSz) / sqrt(butSz[i]), file.path("../images/Good butts", basename(bf[i])))
}

#plotAll("../images/Good butts")


