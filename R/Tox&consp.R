library(dplyr)

### read data

tox <- read.csv("data/raw/phylo.csv")
cons <- read.csv("output/conspicuousness.csv")
cgb <- read.csv("data/raw/CGB.csv")

###Sorts 12 random Zizina otis to paste into spreadsheet
set.seed(123) # Sort the same value
random_rows <- cgb[sample(nrow(cgb), 12), ]

# Append the selected rows to tox
tox <- rbind(tox, random_rows)
###################### Remove sex from PS
# Locate the row where Binomial equals "Psychonotis caelius" and set the Sex value to NA
cons$Sex[cons$Binomial == "Psychonotis caelius"] <- NA

##################Create merging factor
cons$merge_factor <- paste(cons$Binomial, cons$Sex, sep = "_")

tox$merge_factor <- paste(tox$Binomial, tox$Sex, sep = "_")
############### Merge
# Define the exceptions
exceptions <- c("Catopsilia pomona", "Psychonotis caelius", "Hypolimnas bolina", "Zizina otis", "Zizula hylax")

# Step 1: Create merge factor for `cons`
cons <- cons %>%
  mutate(merge_factor = if_else(Binomial %in% exceptions, paste(Binomial, Sex, sep = " "), Binomial))

# Step 2: Create merge factor for `tox`
tox <- tox %>%
  mutate(merge_factor = if_else(Binomial %in% exceptions, paste(Binomial, Sex, sep = " "), Binomial))

# Step 3: Merge `cons` into `tox`, keeping all rows from `tox`
merged_data <- left_join(tox, cons, by = "merge_factor") %>%
  select(-merge_factor)  # Remove the temporary merge_factor column


#Remove unnecessary colums

columns_to_remove <- c("Binomial.y", "Dorsal.photo", "Dorsal.URL", "Ventral.photo", "Ventral.URL", "Sex.y")
merged_data <- merged_data[, !colnames(merged_data) %in% columns_to_remove]

#rename columns

colnames(merged_data)[colnames(merged_data) == "Binomial.x"] <- "Binomial"
colnames(merged_data)[colnames(merged_data) == "Sex.x"] <- "Sex"

#save
write.csv(merged_data, "data/processed/brms.dt.csv", row.names = FALSE)

