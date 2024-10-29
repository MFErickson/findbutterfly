# Resizes butterfly photos so they all have similar numbers of non-transparent pixels

library(imager)
library(png)
library(JUtils)

##################################################################################
# Check and correct butterfly images sizes


# Plot all images in a directory on a grid with a constant scale
plotAll <- function(dir) {
  bf <- list.files(dir, "*.png", full.names = T)
  # bf <- head(bf, 12)
  buts <- sapply(bf, function(fn) readPNG(fn, native = FALSE))
  # Count no. of non-transparent pixels
  notTrans <- sapply(buts, function(but) { sum(but[,,4] > 0.5) })
  buts <- buts[order(notTrans)]
  bf <- bf[order(notTrans)]
  notTrans <- sort(notTrans)
  
  # Scale them all to fit in
  widths <- sapply(buts, function(but) dim(but)[2])
  heights <- sapply(buts, function(but) dim(but)[1])
  maxSz <- max(c(widths, heights))
  scale <- 1 / maxSz
  par(mar = c(0, 0, 0, 0))
  nr <- ceiling(sqrt(length(bf)))
  nc <- ceiling(length(bf) / nr)
  plot(NULL, xlim = c(0.5, nc + 0.5), ylim = c(0.5, nr + 0.5), axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (i in seq_along(buts) ) {
    cx <- (i - 1) %% nc + 1
    cy <- (i - 1) %/% nc + 1
    #cat(sprintf("%d: %s %d x %d\n", i, bf[i], widths[i], heights[i]))
    # text(cx, cy, i)
    JPlotRaster(buts[[i]], cx, cy, width = widths[i] * scale)
    w <- widths[i] * scale
    h <- heights[i] * scale
    #rect(cx - w / 2, cy - h / 2, cx + w / 2, cy + h / 2)
    text(cx, cy - 0.45, notTrans[i])
    text(cx, cy + 0.45, basename(bf[i]))
  }
}


# Scale and save a single image
scaleAndSaveImage <- function(inFile, scale, outFile) {
  x <- load.image(inFile)
  
  # Scale; the height is determined automatically so that
  # the aspect ratio is preserved
  y <- imresize(x, scale, interpolation = 6)
  
  # show the scaled image
  #display(y)
  
  save.image(y, outFile)
}

# Scale all PNG images to a target number of non-transparent pixels
scaleAll <- function(indir, outdir, targetPixels) {
  bf <- list.files(indir, "*.png", full.names = T)
  buts <- sapply(bf, function(fn) readPNG(fn, native = FALSE))
  # Count no. of non-transparent pixels
  notTrans <- sapply(buts, function(but) { sum(but[,,4] > 0.5) })
  
  for (i in seq_along(bf)) {
    print(i)
    scaleAndSaveImage(bf[i], sqrt(targetPixels) / sqrt(notTrans[i]), file.path(outdir, basename(bf[i])))
  }
}

# Read all the images in Big butts and resize them into Good butts
scaleAll("../images/Big butts", "../images/Good butts", 30000)

JPlotToPNG("butts-original.png", plotAll("../images/Big butts"), width = 2000, aspect = 1)
JPlotToPNG("butts-scaled.png", plotAll("../images/Good butts"), width = 2000, aspect = 1)

           