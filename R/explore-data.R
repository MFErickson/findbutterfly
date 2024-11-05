# Experimental data exploration

library(lubridate)
library(Durga)

df <- read.csv("score.csv")

# Limit analysis to butterflies that have been tested at leat twice
bt <- table(df$butterflyUrl)
df <- df[df$butterflyUrl %in% names(bt)[bt > 2], ]

# Only outcomes, ie not misses thea then continue to either another miss, hit or escape
outcomes <- df[df$score != "miss", ]

# Is there a consistent change in identification time over elapsed time?
plot(time ~ as.Date(localTime), outcomes)
l <- lm(time ~ as.Date(localTime), outcomes)
abline(l, col = "red")
summary(l)



par(mar = c(8, 5, 1, 1) + 0.1)
plot(as.factor(outcomes$butterflyUrl), outcomes$time, las = 2)
plot(as.factor(outcomes$bgType), outcomes$time, las = 2, xlab = "", ylab = "")


par(mar = c(8, 5, 15, 1) + 0.1)
d <- DurgaDiff(outcomes, "time", "butterflyUrl", contrasts = NULL, R = NA)
par(las = 2)
p <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE, box = T)
diffs <- Filter(function(pwes) (pwes$bca[4] > 500 || pwes$bca[5] < -500), p$es$group.differences)
DurgaBrackets(p, contrasts = diffs)


d <- DurgaDiff(outcomes, "time", "bgType")
par(mar = c(7, 4, 16, 1) + 0.1)
p <- DurgaPlot(d, ef.size = FALSE, frame.plot = FALSE)
diffs <- Filter(function(pwes) (pwes$bca[4] > 500 || pwes$bca[5] < -500), p$es$group.differences)
# Customise colours by drawing brackets with the colour of the taller group.
# Get the taller group from each difference
tallerIdx <- sapply(diffs, function(diff) {
  # If the group difference > 0, the first group is taller
  ifelse(diff$t0 > 0, diff$groupIndices[1], diff$groupIndices[2])
})
DurgaBrackets(p, contrasts = diffs, br.col = p$palette[tallerIdx], xpd = NA)



table(table(paste(outcomes$bgType, outcomes$butterflyUrl, sep = "-")))

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
l <- lmer(time ~ butterflyUrl + (1|bgType), data = df)
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

plot(jitter(Escapes) ~ jitter(Misses), data = agg, pch = 16, col = adjustcolor(3, 0.5),
     xlab = "No. of misses", ylab = "Ends in escape?", yaxt = "n", main = "Relationship of misses and escapes")
axis(2, at = c(0, 1), labels = c("No", "Yes"))
l <- glm(Escapes ~ Misses, data = agg, family = "binomial")
summary(l)
x <- seq(0, 5, length.out = 100)
y <- predict(l, newdata = data.frame(Misses = x), type = "response")
lines(x, y, col = 4)
