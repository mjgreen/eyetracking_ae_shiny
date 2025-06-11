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
    fix_x = sample(seq(50, 550), nrow(.)),
    fix_y = sample(seq(50, 750), nrow(.)),
    jpg = ifelse(trial_id == 1, "C1_041.jpg", "C1_052.jpg")
  )
write_csv(f, "fixation_report.csv")
