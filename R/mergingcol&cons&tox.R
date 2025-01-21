library(dplyr)

#Import data
cons <- read.csv("data/processed/brms.dt.csv")
hum_col<- read.csv("data/raw/Human_Colour_Data-merged.csv")
md <- read.csv("data/raw/processed_data_marilia.csv")
bt_col.s <- read.csv("data/raw/Batch_RNL_Bluetit 0.05_Simple-merged.csv")
bt_col.p <- read.csv("data/raw/Batch_RNL_Bluetit 0.05_Pattern-merged.csv")

# merge colour metrics

col <- merge(hum_col, bt_col.s, by = "MSPEC", all = TRUE)
col <- merge(col, bt_col.p, by = "MSPEC", all = TRUE)

# Merge col & md

col <- col %>%
  mutate(ID = MSPEC)

md_subset <- md[, c("ID", "Binomial", "Sex")]

col2 <- md_subset %>%
  inner_join(col, by = "ID")


col2 <- col2 %>%
  mutate(ID = MSPEC)

#Merged cons & col

special_cases <- c("Catopsilia pomona", 
                   "Hypolimnas bolina", "Zizina otis", "Zizula hylax", 
                   "Cethosia cydippe", "Eurema hecabe", 
                   "Eurema smilax", "Nacaduba berenice")
col2 <- col2 %>%
  mutate(merge_factor = if_else(Binomial %in% special_cases, 
                                 paste(Binomial, Sex, sep = " "), 
                                 Binomial))
cons <- cons %>%
  mutate(merge_factor = if_else(Binomial %in% special_cases, 
                                 paste(Binomial, Sex, sep = " "), 
                                 Binomial))
col2 <- col2 %>%
  select(-ID, -Binomial, -Sex)

col_avg <- col2 %>%
  group_by(merge_factor) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

all <- cons %>%
  left_join(col_avg, by = "merge_factor")


#Remove metrics we can't use

remove_patterns <- c("_0\\.", "_45\\.", "_90\\.", "_135\\.", "CIE", "_verticality", "Verticality", "MaxFreq", "Area", "Major", "Minor", "nbut")
pattern <- paste(remove_patterns, collapse = "|")
all <- all %>%
  select(-matches(pattern))


### Save

write.csv(all, "data/processed/All.metrics.csv", row.names = FALSE)


### Preping stuff for george
MD.Gh <- all[, c("Binomial", "Sex", "merge_factor", "ID", "Ndaphnia", "X240m")]
Md.Gh2 <- col2[, c("MSPEC", "ID", "merge_factor")]

write.csv(MD.Gh, "MD.Gh.csv", row.names = FALSE)
write.csv(Md.Gh2, "MD.Gh2.csv", row.names = FALSE)
