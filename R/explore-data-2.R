# Experimental data exploration

library(png)
library(jpeg)
library(lubridate)
library(Durga)
library(JUtils)

df <- read.csv("score.csv")


# Only outcomes, ie not misses
outcomes <- df[df$score != "miss", ]

########################################################################

# Is there a consistent change in identification time over elapsed time?
ld <- ymd_hms(outcomes$localTime)
plot(time ~ ld, outcomes, pch = 16, col = adjustcolor(4, 0.1))
l <- lm(time ~ ld, outcomes)
abline(l, col = "red")
summary(l)



# par(mar = c(8, 5, 15, 1) + 0.1)
# d <- DurgaDiff(outcomes, "time", "butterflyUrl", contrasts = NULL, R = NA)
# par(las = 2)
# p <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE, box = T, points = F,
#                box.params = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5, outpch = NA), group.colour = 1:4)
# diffs <- Filter(function(pwes) (pwes$bca[4] > 500 || pwes$bca[5] < -500), p$es$group.differences)
# DurgaBrackets(p, contrasts = diffs)
# 
# par(mar = c(8, 5, 15, 1) + 0.1)
# d <- DurgaDiff(outcomes, "conspicuous", "butterflyUrl", contrasts = NULL, R = NA)
# par(las = 2)
# p <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE, box = T, points = F,
#                box.params = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5, outpch = NA), group.colour = 1:4)
# diffs <- Filter(function(pwes) (pwes$bca[4] > 500 || pwes$bca[5] < -500), p$es$group.differences)
# DurgaBrackets(p, contrasts = diffs)


plot(density(table(outcomes$combo)), main = "Samples per butterfly/background combo")


hits <- outcomes[outcomes$score == "hit",]
misses <- outcomes[outcomes$score == "miss",]
a <- aggregate(list(Conspicuousness = outcomes$conspicuous), by = list(Combo = outcomes$combo), FUN = mean)
b <- aggregate(list(Time = outcomes$time), by = list(Combo = outcomes$combo), FUN = mean)
c <- aggregate(list(HitTime = hits$time), by = list(Combo = hits$combo), FUN = mean)
x <- merge(a, b)
x <- merge(x, c)
x <- x[order(x$Conspicuousness), ]
x$Conspicuousness <- x$Conspicuousness * 10000
JPlotDensities(list(density(x$Conspicuousness), density(x$Time)),
               legendLabels = c("Conspicuousness", "Time"))


d <- DurgaDiff(outcomes, "time", "butterflyUrl", random.col = "bgType", ci.conf = 0.99)
p <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE, violin = FALSE, points = FALSE)
diffs <- Filter(function(pwes) (pwes$bca[4] > 3000 || pwes$bca[5] < -3000), p$es$group.differences)
# Customise colours by drawing brackets with the colour of the taller group.
# Get the taller group from each difference
tallerIdx <- sapply(diffs, function(diff) {
  # If the group difference > 0, the first group is taller
  ifelse(diff$t0 > 0, diff$groupIndices[1], diff$groupIndices[2])
})
DurgaBrackets(p, contrasts = diffs, br.col = p$palette[tallerIdx], xpd = NA)


library(lme4)
library(lmerTest)
l <- lm(conspicuous ~ butterflyUrl*bgType, data = outcomes)
summary(l)


agg <- aggregate(list(Escapes = df$score == "escape", 
                      Misses = df$score == "miss", 
                      Hits = df$score == "hit",
                      Time = ifelse(df$score == "miss", 0 , df$time)),
          by = list(Combo = df$comboId), FUN = sum)
plot(Time ~ Misses, data = agg)
l <- lm(Time ~ Misses, data = agg)
abline(l, col = 2)
print(summary(l))

plot(jitter(Escapes) ~ jitter(Misses), data = agg, pch = 16, col = adjustcolor(3, 0.1),
     xlab = "No. of misses", ylab = "Ends in escape?", yaxt = "n", main = "Relationship of misses and escapes")
axis(2, at = c(0, 1), labels = c("No", "Yes"))
l <- glm(Escapes ~ Misses, data = agg, family = "binomial")
summary(l)
x <- seq(0, max(agg$Misses), length.out = 100)
y <- predict(l, newdata = data.frame(Misses = x), type = "response")
lines(x, y, col = 4)


# Plot to show mean butterfly conspicuousness
agg <- aggregate(list(Conspicuousness = outcomes$conspicuous, Time = outcomes$time),
                 by = list(URL = outcomes$butterflyUrl),
                 FUN = mean)
agg <- agg[order(agg$Conspicuousness, decreasing = FALSE),]
JPlotToPNG("output/conspicuous-butts.png",
           plotAll(file.path("../images/butterflies", agg$URL),
                   sprintf("Consp. = %.3f", agg$Conspicuousness)),
           width = 2000, aspect = 1)


# Compare mean butterfly conspicuousness with min bgType conspicuousness
magg <- aggregate(list(Conspicuousness = outcomes$conspicuous), 
               by = list(Combo = outcomes$combo, URL = outcomes$butterflyUrl, bgType = outcomes$bgType), 
               FUN = mean)
minCon <- sapply(unique(unique(magg$URL)), function(butt) { min(magg$Conspicuousness[magg$URL == butt]) })
minF <- file.path("../images/butterflies", names(minCon)[order(minCon)])

meanF <- file.path("../images/butterflies", agg$URL) # NOTE using agg, not magg
timeF <- file.path("../images/butterflies", agg$URL[order(agg$Time, decreasing = TRUE)])
JPlotToPNG("output/method-cmp.png", {
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  visuallyCmpConspMethods(timeF, minF, meanF, timeF)}, 
  width = 10000, height = 700)




# Durga plot of a subset to see what sort of differentiation there is between butterflies
nb <- nrow(agg)
# Pick several adjacent pairs

sub <- round(seq(1, nb - 1, length.out = 4))
sub <- c(rbind(sub, sub + 1))
groups <- agg$URL[sub]
ng <- length(groups)
contrasts <- sprintf("%s - %s", groups[2:ng], groups[1:(ng - 1)])
d <- DurgaDiff(10000 - time ~ butterflyUrl, outcomes, groups = groups, contrasts = contrasts)
#d <- DurgaDiff(conspicuous ~ butterflyUrl, outcomes, groups = agg$URL[sub])
par(las = 2, mar = c(6, 4, 5, 1) + 0.1)
p <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE, points = F,
               box.params = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5, outpch = NA), 
               group.colour = 1:4)
## Show only non-significant differences
#diffs <- Filter(function(pwes) (pwes$bca[4] < 0 & pwes$bca[5] > 0), p$es$group.differences)
DurgaBrackets(p)


