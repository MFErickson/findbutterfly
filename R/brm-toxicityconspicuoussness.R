# Load necessary libraries
library(ape)
library(caper)
library(phylolm)
library(MCMCglmm)
library(brms)
library(ggplot2)
library(performance)
library(missForest)

# Import data
tree <- read.nexus("C:/Users/46090444/OneDrive/GitHub/Daphnia-toxicity/data/raw/AA154_secondary_only_strategyA.tre")
dt <- read.csv("data/processed/brms.dt.csv")

# NA handeling
dt_subset <- dt[, c("Dorsal.conspicuousness", "Ventral.conspicuousness", "ConspicuousnessDiff")]
dt_subset_imputed <- missForest(dt_subset)$ximp
dt[, c("Dorsal.conspicuousness", "Ventral.conspicuousness", "ConspicuousnessDiff")] <- dt_subset_imputed

# Turn negative values positive

dt$ConspicuousnessDiff2 <- abs(dt$ConspicuousnessDiff)

# Check if species are in the tree and prune the tree
tree_pruned <- drop.tip(tree, setdiff(tree$tip.label, unique(dt$Phylo)))

# Check tree structure
is.binary.tree(tree_pruned)
is.rooted(tree_pruned)

# Filter data to align with the pruned tree
dt_filtered <- dt[dt$Phylo %in% tree_pruned$tip.label, ]

# Create phylogenetic covariance matrix
phylo_cov <- vcv(tree_pruned, corr = TRUE)

# Fit the Bayesian model for Dorsal & Ventral Conspicuoussness
brms_modelDV <- brm(
  X240m | trials(Ndaphnia) ~ Dorsal.conspicuousness + Ventral.conspicuousness +
    (1 | gr(Phylo, cov = phylo_cov)),
  data = dt_filtered,
  family = binomial(),
  data2 = list(phylo_cov = phylo_cov),
  control = list(max_treedepth = 10),
  chains = 4,
  iter = 5000,
  warmup = 1000,
  thin = 10
)

# Model diagnostics
summary(brms_modelDV)
prior_summary(brms_modelDV)

# Save output
output_file <- "output/brms-DVConpicuoussness.txt"
sink(output_file)
print(summary(brms_model))
sink()

# Model checks
pp_check(brms_modelDV)
bayes_R2(brms_modelDV)
r2_results <- performance::r2(brms_modelDV)
print(r2_results)

# Plot residuals
residuals <- resid(brms_modelDV)
plot(residuals[, 1], residuals[, 2], xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)
boxplot(residuals, main = "Boxplot of Residuals")

# Fit the Bayesian model for Diference in Conspicuoussness

brms_modelCD <- brm(
  X240m | trials(Ndaphnia) ~ ConspicuousnessDiff2 + 
    (1 | gr(Phylo, cov = phylo_cov)),
  data = dt_filtered,
  family = binomial(),
  data2 = list(phylo_cov = phylo_cov),
  control = list(max_treedepth = 10),
  chains = 4,
  iter = 5000,
  warmup = 1000,
  thin = 10
)

# Model diagnostics
summary(brms_modelCD)
prior_summary(brms_modelCD)

# Save output
output_file <- "output/brms-ConspicDiff.txt"
sink(output_file)
print(summary(brms_modelCD))
sink()

# Model checks
pp_check2(brms_modelCD)
bayes_R22(brms_modelCD)
r2_results2 <- performance::r2(brms_modelCD)
print(r2_results2)

# Plot residuals
residuals2 <- resid(brms_modelCD)
plot(residuals[, 1], residuals[, 2], xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)
boxplot(residuals, main = "Boxplot of Residuals")

################################### doing it based on species average#############
# Create the new Merging Factor based on conditions


# Create the new Merging Factor based on conditions
dt2 <- dt %>%
  mutate(Merge_Factor = ifelse(
    Binomial.x %in% c("Catopsilia pomona", "Psychonotis caelius", "Hypolimnas bolina", "Zizina otis"),
    paste(Binomial.x, Sex.x, sep = "_"),  # Merge based on Binomial.x and Sex.x
    Binomial.x                             # Default is based on Binomial.x
  ))

# Summing up Ndaphnia and X240m based on Merge_Factor
summarized_dt2 <- dt2 %>%
  group_by(Merge_Factor) %>%
  summarize(
    Sum_Ndaphnia = sum(Ndaphnia, na.rm = TRUE),  # Sum of Ndaphnia
    Sum_X240m = sum(X240m, na.rm = TRUE),        # Sum of X240m
    Phylo = unique(Phylo),                       # Keep unique Phylo
    Dorsal_conspicuousness = unique(Dorsal.conspicuousness),  # Keep Dorsal.conspicuousness
    Ventral_conspicuousness = unique(Ventral.conspicuousness),# Keep Ventral.conspicuousness
    ConspicuousnessDiff = unique(ConspicuousnessDiff),        # Keep ConspicuousnessDiff
    .groups = "drop"
  )

# Write the new summarized dataframe to a CSV (or use another format if needed)
write.csv(summarized_dt2, "summarized_data_dt2.csv", row.names = FALSE)


# Fit the Bayesian model for Dorsal & Ventral Conspicuoussness
brms_modelDV2 <- brm(
  X240m | trials(Ndaphnia) ~ Dorsal.conspicuousness + Ventral.conspicuousness +
    (1 | gr(Phylo, cov = phylo_cov)),
  data = dt_filtered,
  family = binomial(),
  data2 = list(phylo_cov = phylo_cov),
  control = list(max_treedepth = 10),
  chains = 4,
  iter = 5000,
  warmup = 1000,
  thin = 10
)

# Model diagnostics
summary(brms_modelDV2)
prior_summary(brms_modelDV2)

# Save output
output_file <- "output/brms-DVConpicuoussness2.txt"
sink(output_file)
print(summary(brms_model))
sink()

# Model checks
pp_check(brms_modelDV2)
bayes_R2(brms_modelDV2)
r2_results <- performance::r2(brms_modelDV2)
print(r2_results)

# Plot residuals
residuals3 <- resid(brms_modelDV2)
plot(residuals[, 1], residuals[, 2], xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)
boxplot(residuals, main = "Boxplot of Residuals")

# Fit the Bayesian model for Diference in Conspicuoussness

brms_modelCD2 <- brm(
  X240m | trials(Ndaphnia) ~ ConspicuousnessDiff2 +
    (1 | gr(Phylo, cov = phylo_cov)),
  data = dt_filtered,
  family = binomial(),
  data2 = list(phylo_cov = phylo_cov),
  control = list(max_treedepth = 10),
  chains = 4,
  iter = 5000,
  warmup = 1000,
  thin = 10
)

# Model diagnostics
summary(brms_modelCD2)
prior_summary(brms_modelCD2)

# Save output
output_file <- "output/brms-ConspicDiff.txt"
sink(output_file)
print(summary(brms_modelCD2))
sink()

# Model checks
pp_check2(brms_modelCD2)
bayes_R22(brms_modelCD2)
r2_results2 <- performance::r2(brms_modelCD2)
print(r2_results2)

# Plot residuals
residuals4 <- resid(brms_modelCD2)
plot(residuals[, 1], residuals[, 2], xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)
boxplot(residuals, main = "Boxplot of Residuals")
