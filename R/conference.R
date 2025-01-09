# Generate plots for use at a conference
library(lubridate)
source("functions.R")

HOURS_AGO <- 24
cohortStart <- now() - dhours(HOURS_AGO)
  
#if (!interactive()) {
  source("download-data.R")
#}

scores <- read.csv("score.csv")

# Only outcomes, ie not misses
preOutcomes <- scores[scores$score != "miss" & scores$localTime < cohortStart, ]
# Outcomes from this cohort
cohort <- scores[scores$score != "miss" & scores$localTime >= cohortStart, ]
cat(sprintf("%d games from the last %d hour%s\n", length(unique(cohort$sessionId)), HOURS_AGO, ifelse(HOURS_AGO == 1, "", "s")))

#############################################################################################

# Plot leader boards bar chart
plotLeaderBoards <- function(cohortScale) {
  par(mar = c(5, 4, 1, 1) + 0.1)
  preLB <- buildLeaderBoard(preOutcomes)
  preBars <- sapply(0:30, function(n) sum(preLB$Found == n))
  cohortLB <- buildLeaderBoard(cohort)
  cohortBars <- sapply(0:30, function(n) sum(cohortLB$Found == n))
  unscaled <- matrix(c(preBars / nrow(preLB),
                   cohortBars / nrow(cohortLB)),
                 nrow = 2, byrow = TRUE)
  bars <- matrix(c(preBars / nrow(preLB),
                   cohortScale * cohortBars / nrow(cohortLB)),
                 nrow = 2, byrow = TRUE)
  colnames(bars) <- 0:30
  barplot(bars * 100, col = c(2, 4), beside = TRUE, ylim = c(0, max(unscaled) * 100),
          xlab = "Butterflies found", ylab = "Proportion of players (%)")
  if (cohortScale > 0) {
    leg <- c(sprintf("Previous players (average %.1f)", mean(preLB$Found)),
             sprintf("This conference (average %.1f)", mean(cohortLB$Found)))
  } else {
    leg <- sprintf("Previous players (average %.1f)", mean(preLB$Found))
  }
  legend("topleft", leg, fill = c(2, 4), inset = c(0.03, 0))
}

animate2ndLeaderBoard <- function() {
  scenes <- list(JScene(2, 30,
                        cohortScale = JTransition(0, 1, JEaseInOut),
                        plotFn = plotLeaderBoards))
  JAnimateScenes("conference/leaderboard-animated.gif", scenes, loop = 1, width = 900, res = 140)
}


####

plotChoiceSummary <- function() {
  par(mar = c(2, 0, 2, 0) + 0.1)
  consp <- calculateConspicuousness(preOutcomes)
  consp$path <- file.path("../images/butterflies", consp$URL)
  ord <- order(consp$BgConspicRatio)
  preURLS <- c(consp$path[head(ord, 5)], NA, consp$path[tail(ord, 5)])

  cohcon <- calculateConspicuousness(cohort)
  cohcon$path <- file.path("../images/butterflies", cohcon$URL)
  ord <- order(cohcon$BgConspicRatio)
  cohURLS <- c(cohcon$path[head(ord, 5)], NA, cohcon$path[tail(ord, 5)])

  visuallyCmpConspMethods(preURLS, cohURLS, drawConnections = FALSE)
  text(6, 1, "...", cex = 3)
  text(6, 2, "...", cex = 3)
  dy <- 0.45
  text(1, 2 + dy, "Your results", adj = 0, xpd = NA, col = "#501010")
  text(1, 1 + dy, "Previous results", adj = 0, xpd = NA, col = "#501010")
  y <- 0.6
  text(4, y, "More conspicuous", adj = 0, xpd = NA)
  arrows(6.3, y, 9, y, lwd = 4, xpd = NA)
}

####
# Generate plots

JPlotToPNG("conference/leaderboard-pre.png", plotLeaderBoards(0), width = 900, res = 140)
animate2ndLeaderBoard()
JPlotToPNG("conference/leaderboard-both.png", plotLeaderBoards(1), width = 900, res = 140)

# Conspicuousness results
JPlotToPNG("conference/conspic.png", plotChoiceSummary(), width = 1800, height = 510, res = 200)
