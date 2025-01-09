library(dplyr)
library(ggplot2)

# Extract random effects for the phylogeny
phylo_effects <- ranef(brms_model, groups = "Phylo")
phylo_effects_df <- as.data.frame(phylo_effects$Phylo)

# Rank phylogenetic groups by effect estimates
phylo_effects_ranked <- phylo_effects_df %>% 
  mutate(Phylo = rownames(phylo_effects_df)) %>%  # Add rownames as a new column
  arrange(desc(Estimate.Intercept))  # Sort by phylogenetic effect estimates

# Plot ranked effects

ggplot(phylo_effects_ranked, aes(x = reorder(Phylo, Estimate.Intercept), y = Estimate.Intercept)) +
  geom_point() +
  geom_errorbar(aes(ymin = Q2.5.Intercept, ymax = Q97.5.Intercept), width = 0.2) +
  coord_flip() +
  labs(x = "Phylogenetic Group", 
       y = "Random Effect Estimate (Toxicity Contribution)",
       title = "Phylogenetic Contributions to Toxicity") +
  theme_minimal()


### Tribe level

# Extract tribe information, accounting for subspecies and variable structure
phylo_effects_df <- phylo_effects_df %>%
  mutate(Tribe = sub("^[^_]+_[^_]+_[^_]+_[^_]+_([^_]+)_.*$", "\\1", rownames(phylo_effects_df)))

# Check unique tribes to confirm extraction works
unique(phylo_effects_df$Tribe)

# Aggregating phylogenetic effects by tribe
tribe_effects <- phylo_effects_df %>%
  group_by(Tribe) %>%
  summarize(
    Estimate = mean(Estimate.Intercept, na.rm = TRUE),
    Q2.5 = mean(Q2.5.Intercept, na.rm = TRUE),
    Q97.5 = mean(Q97.5.Intercept, na.rm = TRUE),
    .groups = "drop"
  )

# Sorting and plotting the tribe-level effects
tribe_effects <- tribe_effects %>%
  arrange(desc(Estimate))

ggplot(tribe_effects, aes(x = reorder(Tribe, Estimate), y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
  coord_flip() +
  labs(x = "Tribe", y = "Aggregated Effect Estimate (Toxicity Contribution)",
       title = "Tribe-Level Contributions to Toxicity") +
  theme_minimal()


rows_with_x <- phylo_effects_df %>%
  filter(Tribe == "X")

print(rows_with_x)


### Subfamily

phylo_effects_df <- phylo_effects_df %>%
  mutate(Subfamily = ifelse(
    grepl("^[^_]+_[^_]+_[^_]+_([^_]+)_", rownames(phylo_effects_df)),
    sub("^[^_]+_[^_]+_[^_]+_([^_]+)_.*$", "\\1", rownames(phylo_effects_df)),
    "Unknown"  # Assign "Unknown" if subfamily can't be extracted
  ))

subfamily_effects <- phylo_effects_df %>%
  group_by(Subfamily) %>%
  summarize(
    Estimate = mean(Estimate.Intercept, na.rm = TRUE),
    Q2.5 = mean(Q2.5.Intercept, na.rm = TRUE),
    Q97.5 = mean(Q97.5.Intercept, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Estimate))

ggplot(subfamily_effects, aes(x = reorder(Subfamily, Estimate), y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
  coord_flip() +
  labs(x = "Subfamily", y = "Aggregated Effect Estimate (Toxicity Contribution)",
       title = "Subfamily-Level Contributions to Toxicity") +
  theme_minimal()

# By family

# Extract family (third field in the naming structure)
phylo_effects_df <- phylo_effects_df %>%
  mutate(Family = ifelse(
    grepl("^[^_]+_[^_]+_([^_]+)_", rownames(phylo_effects_df)),
    sub("^[^_]+_[^_]+_([^_]+)_.*$", "\\1", rownames(phylo_effects_df)),
    "Unknown"  # Assign "Unknown" if family can't be extracted
  ))
family_effects <- phylo_effects_df %>%
  group_by(Family) %>%
  summarize(
    Estimate = mean(Estimate.Intercept, na.rm = TRUE),
    Q2.5 = mean(Q2.5.Intercept, na.rm = TRUE),
    Q97.5 = mean(Q97.5.Intercept, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Estimate))


ggplot(family_effects, aes(x = reorder(Family, Estimate), y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
  coord_flip() +
  labs(x = "Family", y = "Aggregated Effect Estimate (Toxicity Contribution)",
       title = "Family-Level Contributions to Toxicity") +
  theme_minimal()

