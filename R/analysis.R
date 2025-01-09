# Analyses

library(png)
library(jpeg)
library(lubridate)
library(Durga)
library(JUtils)
source("functions.R")

# This script assumes that the game data has already been downloaded by running the script download-data.R
if (!file.exists("score.csv")) stop("You must run download-data.R before running this script")

# Read downloaded game data
scores <- read.csv("score.csv")
sessions <- read.csv("session.csv")

# Only outcomes, ie not misses
outcomes <- scores[scores$score != "miss", ]
# Remove duplicate species
scores <- scores[scores$butterflyUrl != "3455 (1).png", ]

#################################################################################
# Report summary stats

cat(sprintf("After cleaning, %d sessions from %d users with %d scores: %s escapes, %d misses and %d hits\n\n",
            nrow(sessions), length(unique(sessions$userId)), nrow(scores),
            sum(scores$score == "escape"), sum(scores$score == "miss"), sum(scores$score == "hit")))


#################################################################################
# Report correlations between different methods for measuring conspicuousness

con <- calculateConspicuousness(scores)
cat(sprintf("Comparison of methods for quantifying conspicuousness,\n"))
cat(sprintf("  correlation between mean detection time across all \n  backgrounds and mean detection time in most cryptic background:\n"))
r <- cor(con$TimeConspicuousness, con$BgConspicTime, method = "spearman")
cat(sprintf("  Spearman's ρ = %g (rank correlation)\n", r))
r <- cor(con$TimeConspicuousness, con$BgConspicTime, method = "pearson")
cat(sprintf("  Pearson correlation = %g\n\n", r))


#################################################################################
# Build a spreadsheet

# Get species, sex, angle info for each image, but only keep a single measure of conspicuousness
info <- incorporateSpecies(con[, c("URL", "BgConspicTime")], "game_photo_legend.xlsx")
# Reorder columns
info <- info[, c("Binomial", "Photo", "URL", "Side", "Sex", "BgConspicTime")]
# Split into dorsal and ventral then recombine
dorsal <- info[info$Side == "dorsal", ]
dorsal$Side <- NULL
names(dorsal) <- c("Binomial", "Dorsal.photo", "Dorsal.URL", "Sex", "Dorsal.conspicuousness")
ventral <- info[info$Side == "ventral", ]
ventral$Side <- NULL
names(ventral) <- c("Binomial", "Ventral.photo", "Ventral.URL", "Sex", "Ventral.conspicuousness")
comb <- merge(dorsal, ventral, all = TRUE)

# Sometimes there's one ventral for 2 dorsal images
reused <- numeric(0)
for (missing in which(is.na(comb$Ventral.photo))) {
  miss <- comb[missing,]
  vi <- which(comb$Binomial == miss$Binomial & is.na(comb$Dorsal.photo) & !is.na(comb$Ventral.photo))
  #cat(sprintf("%d <- %d\n", missing, vi))
  cols <- c("Ventral.photo", "Ventral.URL", "Ventral.conspicuousness")
  comb[missing, cols] <- comb[vi, cols]
  
  reused <- c(reused, vi)
}
comb <- comb[-unique(reused), ]

# Calculate difference in conspicuousness between the two sides
comb$ConspicuousnessDiff <- comb$Dorsal.conspicuousness - comb$Ventral.conspicuousness

# Write out the result
write.csv(comb, "output/conspicuousness.csv", row.names = FALSE)

#########################################################################
# Image conspicuousness

# Plot to show mean butterfly conspicuousness
JPlotToPNG("output/conspicuous-butts.png", {
  info <- info[order(info$BgConspicTime), ]
  plotAll(file.path("../images/butterflies", info$URL),
          buttLabel(info),
          sprintf("Consp. = %.3f", info$BgConspicTime))  
}, width = 2000, aspect = 1)


#########################################################################
# Dorsal vs ventralcomparisons

source("dorsal-ventral.R")