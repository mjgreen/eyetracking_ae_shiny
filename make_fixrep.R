library(tidyverse)

set.seed(1)
f = expand.grid(
  subject_id = seq(1,2),
  trial_id = seq(1,2),
  fix_id=seq(1,10)
  ) %>% 
  arrange(subject_id, trial_id) %>% 
  as_tibble() %>% 
  mutate(
    x = sample(seq(50, 550), nrow(.)),
    y = sample(seq(50, 750), nrow(.))
  )
write_csv(f, "fixation_report.csv")
