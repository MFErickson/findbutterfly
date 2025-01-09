library(JUtils)
library(Durga)
library(readxl)
source("functions.R")


comb <- read.csv("output/conspicuousness.csv")
# Order on difference
comb <- comb[order(comb$ConspicuousnessDiff, na.last = FALSE), ]


###############################################################################

# Is dorsal more conspicuous than ventral?
JPlotToPNG("output/dorsal-vs-ventral.png", {
  d <- DurgaDiff(comb, groups = c("Ventral" = "Ventral.conspicuousness", "Dorsal" = "Dorsal.conspicuousness"), na.rm = TRUE)
  par(mar = c(4, 4, 1, 1) + 0.1)
  DurgaPlot(d, left.ylab = "Conspicuousness")
  cat("Mean dorsal/ventral conspicuousness comparison:\n")
  print(d)
}, width = 900, res = 120)


# Dorsal - ventral conspicuousness - hypothesised that sexual signals are dorsal, anti-predator are ventral
JPlotToJPEG("output/dorsal-ventral.jpg", {
  du <- ifelse(is.na(comb$Dorsal.URL), NA, file.path("../images/butterflies", comb$Dorsal.URL))
  vu <- ifelse(is.na(comb$Ventral.URL), NA, file.path("../images/butterflies", comb$Ventral.URL))
  visuallyCmpConspMethods(vu, du)
  sd <- sd(comb$ConspicuousnessDiff, na.rm = TRUE)
  col <- sapply(comb$ConspicuousnessDiff, function(d) ifelse(is.na(d), 1,
                                        adjustcolor(ifelse(d < 0, 2, 4), 
                                                    ifelse(abs(d) > sd, 1, 0.4))))
  font <- ifelse(abs(comb$ConspicuousnessDiff) > 2 * sd, 2, 1)
  text(seq_len(nrow(comb)), 1.5, sprintf("%s\n%g", buttLabel(comb), comb$ConspicuousnessDiff), cex = 1, col = col, font = font)
  
  text(which(comb$ConspicuousnessDiff > 0)[1], 0.7, " Dorsal more conspicuous", adj = 0, cex = 2, col = 4, xpd = NA)
  text(which(comb$ConspicuousnessDiff > 0)[1] - 1, 0.7, "Dorsal less conspicuous ", adj = 1, cex = 2, col = 2, xpd = NA)
}, width = 10000, height = 700)

# Ventral ordered by conspicuousness
JPlotToPNG("output/conspicuous-butts-ventral.png", {
  ventral <- comb[!duplicated(comb$Ventral.photo) & !is.na(comb$Ventral.photo), ]
  ventral <- ventral[order(ventral$Ventral.conspicuousness), ]
  plotAll(file.path("../images/butterflies", ventral$Ventral.URL),
          buttLabel(ventral),
          sprintf("Consp. = %.3f", ventral$Ventral.conspicuousness))
}, width = 2000, aspect = 1)

# Dorsal ordered by conspicuousness
JPlotToPNG("output/conspicuous-butts-dorsal.png", {
  dorsal <- comb[!duplicated(comb$Dorsal.photo) & !is.na(comb$Dorsal.photo), ]
  dorsal <- dorsal[order(dorsal$Dorsal.conspicuousness), ]
  plotAll(file.path("../images/butterflies", dorsal$Dorsal.URL),
          buttLabel(dorsal),
          sprintf("Consp. = %.3f", dorsal$Dorsal.conspicuousness))
}, width = 2000, aspect = 1)
