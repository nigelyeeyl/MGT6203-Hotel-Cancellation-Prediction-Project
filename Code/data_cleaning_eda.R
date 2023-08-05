library(car)
library(tidyr)

# Assuming you've setwd to parent directory of group project
source("Code/load_data_function.R")

df2 <- load_cleaned_df(file_path)
# Analysis of categorical variables removed
df2 %>% 
  select(all_of(factor_cols)) %>% 
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  summarize(n_val = n()) %>%
  ungroup() %>%
  group_by(name) %>%
  mutate(rel_prop = n_val / sum(n_val)) %>%
  arrange(name, -rel_prop) %>% View()

model <-
  glm(
    is_canceled ~ .,
    family = binomial(link = "logit"),
    data = df2
  )

# Surprisingly... nothing with VIF >= 5.
vif(model)
