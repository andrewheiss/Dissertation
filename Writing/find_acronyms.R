library(tidyverse)
library(stringr)

dissertation <- read_file(file.path(PROJHOME, "Writing", 
                                    "general_output", "compiled.md"))

potential_acronyms <- str_extract_all(dissertation,
                                      "\\b[A-Z][a-zA-Z\\.]+?[A-Z]\\b\\.?",
                                      simplify = TRUE) %>%
  t() %>% as_data_frame() %>% rename(acronym = V1) %>%
  mutate(acronym = str_replace(acronym, "\\.", "")) %>%
  count(acronym) %>%
  arrange(desc(n))
