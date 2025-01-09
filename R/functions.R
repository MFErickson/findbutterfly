library(readxl)
library(png)
library(JUtils)

# Plot all images in a directory on a grid with a constant scale
plotAllInDir <- function(dir) {
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
    # w <- widths[i] * scale
    # h <- heights[i] * scale
    #rect(cx - w / 2, cy - h / 2, cx + w / 2, cy + h / 2)
    text(cx, cy - 0.45, notTrans[i])
    text(cx, cy + 0.45, basename(bf[i]))
  }
}

plotAll <- function(files, label = basename(files[i]), caption) {
  buts <- sapply(files, function(fn) readPNG(fn, native = FALSE))
  
  # Scale them all to fit in
  widths <- sapply(buts, function(but) dim(but)[2])
  heights <- sapply(buts, function(but) dim(but)[1])
  maxSz <- max(c(widths, heights))
  scale <- 1 / maxSz
  par(mar = c(0, 0, 0, 0))
  nr <- ceiling(sqrt(length(files)))
  nc <- ceiling(length(files) / nr)
  plot(NULL, xlim = c(0.5, nc + 0.5), ylim = c(0.5, nr + 0.5), axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (i in seq_along(buts) ) {
    cx <- (i - 1) %% nc + 1
    cy <- (i - 1) %/% nc + 1
    JPlotRaster(buts[[i]], cx, cy, width = widths[i] * scale)
    text(cx, cy - 0.45, label[i])
    text(cx, cy - 0.45 - 1.5 * strheight("x"), caption[i])
  }
}

# Creates an image of ordered butterflies. 
# 
# ... List of ordered vectors of butterfly image URLs
visuallyCmpConspMethods <- function(..., drawConnections = TRUE, lineDy = 0.07) {
  orders <- list(...)
  
  allButs <- unique(unlist(orders))
  buts <- sapply(allButs, function(fn) if (is.na(fn)) { NULL } else { readPNG(fn, native = FALSE) })
  widths <- sapply(buts, function(but) if (is.null(but)) { NA } else { dim(but)[2] })
  heights <- sapply(buts, function(but) if (is.null(but)) { NA } else { dim(but)[1] })
  maxSz <- max(c(widths, heights), na.rm = TRUE)
  scale <- 1 / maxSz
  
  plot(NULL, xlim = c(0.5, length(orders[[1]]) + 0.5), ylim = c(0.8, length(orders) + 0.2), 
       xaxs = 'i', yaxs = 'i', axes = FALSE, xlab = "", ylab = "")
  for (ri in seq_along(orders)) {
    r <- orders[[ri]]
    for (bi in seq_along(r)) {
      if (!is.na(r[bi])) {
        png <- buts[[r[bi]]]
        width <- dim(png)[2]
        JPlotRaster(png, bi, ri, width = width * scale, xpd = NA)
      }
    }
  }
  
  # Draw connecting lines
  if (drawConnections) {
    for (i in seq_len(length(orders) - 1)) {
      o1 <- orders[[i]]
      o2 <- orders[[i + 1]]
      for (x1 in seq_along(o1)) {
        x2 <- which(o2 == o1[x1])
        if (length(x2) > 0)
          segments(x1, i + lineDy, x2, i + 1 - lineDy, col = x1 %% 3 + 2)
      }
      # calculate Pearson's r
      rank1 <- seq_along(o1)
      rank2 <- match(o2, o1)
      r <- cor(rank1, rank2, method = "spearman")
      if (!is.na(r))
        text(0.6, i + 0.5, sprintf("Spearman's ρ = %g", r), pos = 4, cex = 1.5)
    }
  }
}

buildLeaderBoard <- function(outcomes) {
  leaderBoard <- aggregate(list(Time = outcomes$time), by = list(sessionId = outcomes$sessionId), FUN = sum)
  leaderBoard <- merge(leaderBoard, 
                       aggregate(list(Found = outcomes$score), by = list(sessionId = outcomes$sessionId), 
                                 FUN = function(x) sum(x == "hit")))
  leaderBoard <- leaderBoard[order(leaderBoard$Found, leaderBoard$Time, decreasing = c(TRUE, FALSE)), ]
  leaderBoard[, c("Found", "Time")]
}

#############################################################################################

# Read species data for each image URL
readSpeciesInfo <- function(photoInfoFile = "game_photo_legend.xlsx") {
  # Read photo information
  info <- read_xlsx(photoInfoFile)
  
  # Construct URL from Photo ID. This is a little bit complicated because the
  # files weren't named consistently
  info$URL <- sapply(info$Photo, function(pid) {
    # Look for a photo with the appropriate photo id in its name
    lf <- list.files("../images/butterflies", pattern = paste0("^", pid, "[^0-9]"))
    if (length(lf) == 1)
      lf[[1]]
    else
      NA
  })
  bad <- which(is.na(info$URL))
  if (any(bad)) {
    warning(sprintf("Unable to locate images for photo IDs: %s", paste(info$Photo[bad], collapse = ", ")))
  }
  
  info  
}

# Incorporate species data into conspicuousness measurements for each image URL
incorporateSpecies <- function(con, photoInfoFile = "game_photo_legend.xlsx") {
  info <- readSpeciesInfo(photoInfoFile)
  m <- merge(con, info)
  bad <- con$URL[!con$URL %in% m$URL]
  if (length(bad) > 0) {
    warning(sprintf("No species defined for URL(s) %s", paste(bad, collapse = ", ")))
  }
  m
}

# Given a data frame of outcomes, returns a data frame of conspicuousness for
# each butterfly using several methods. 
# 
# @returns
# data frame with columns:
# MeanTime - mean time for all hits and escapes for each butterfly
# ConspicRatio - mean ratio of hits (1) to escapes (0)
# TimeConspicuousness - 1 - meanTime / max(outcomes$time)
# BgConspicTime - TimeConspicuousness on most cryptic background type for each butterfly
# BgConspicRatio - ConspicRatio on the most cryptic background type
calculateConspicuousness <- function(scores) {
  outcomes <- scores[scores$score != "miss", ]

    # Calculate conspicuousness as time taken to find the butterfly (or max time in case of an escape)
  agg <- aggregate(list(MeanTime = outcomes$time,
                        ConspicRatio = outcomes$conspicuousRatio),
                   by = list(URL = outcomes$butterflyUrl),
                   FUN = mean)
  agg$TimeConspicuousness <- 1 - agg$MeanTime / max(outcomes$time)
  
  # Also calculate conspicuousness on least conspicuous background
  magg <- aggregate(list(ComboTime = outcomes$time,
                         ComboConsp = outcomes$conspicuousRatio), 
                    by = list(Combo = outcomes$combo, URL = outcomes$butterflyUrl, bgType = outcomes$bgType), 
                    FUN = mean)
  # Conspicuousness on most cryptic background using timed conspicuousness
  magg$comboTimeConspicuousness <- 1 - magg$ComboTime / max(outcomes$time)
  minTimeCon <- sapply(unique(magg$URL), function(butt) { min(magg$comboTimeConspicuousness[magg$URL == butt]) })
  
  # Conspicuousness on most cryptic background using hit:escape conspicuousness
  minRatioCon <- sapply(unique(magg$URL), function(butt) { min(magg$ComboConsp[magg$URL == butt]) })
  
  agg <- merge(agg, data.frame(URL = names(minTimeCon), BgConspicTime = minTimeCon, BgConspicRatio = minRatioCon))
  
  # Now do the same thing but with background images instead of background types
  agg
}

# Returns the "complexity" of a JPG image as the ratio of compressed to uncompressed size
LZComplexity <- function(path) {
  # Read raw data. Don't use image data directly because it is already compressed
  if (grepl("\\.png$|\\.PNG$", path)) {
    img <- readPNG(path)
  } else {
    img <- readJPEG(path)
  }
  raw <- serialize(img, NULL)
  origSz <- length(raw)
  
  txt.gz <- memCompress(raw, "g")
  newSz <- length(txt.gz)
  newSz / origSz
}

# Given butterfly information data frame, returns a label describing the
# butterfly as "species [sex]".
buttLabel <- function(info) {
  ifelse(is.na(info$Sex), info$Binomial,
         sprintf("%s (%s)", info$Binomial, info$Sex))
}
