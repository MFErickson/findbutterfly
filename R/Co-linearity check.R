library(car)

dt$live <- dt$Ndaphnia - dt$X240m

# Compute the correlation matrix
cor_matrix <- cor(dt[, c("Dorsal.conspicuousness", "Ventral.conspicuousness", "ConspicuousnessDiff")], use = "complete.obs")
print(cor_matrix)

