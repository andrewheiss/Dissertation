library(tidyverse)
library(stringr)

# Load full text
dissertation <- read_file(file.path(PROJHOME, "Writing", 
                                    "general_output", "compiled.md"))

# Identify potential acronyms with regex
potential_acronyms <- str_extract_all(dissertation,
                                      "\\b[A-Z][a-zA-Z\\.]+?[A-Z]\\b\\.?",
                                      simplify = TRUE) %>%
  t() %>% as_data_frame() %>% rename(acronym = V1) %>%
  mutate(acronym = str_replace(acronym, "\\.", "")) %>%
  count(acronym) %>%
  arrange(desc(n))

# Save to CSV for manual parsing
write_csv(potential_acronyms, file.path(PROJHOME, "Writing", "acronyms_WILL_BE_OVERWRITTEN.csv"))

# Load manually defined acronyms and output a TeX file of abbreviations
clean_acronyms <- read_csv("~/Desktop/acronyms.csv") %>%
  filter(!is.na(full)) %>%
  arrange(acronym) %>%
  mutate(item = paste0("    \\item[", acronym, "] ", full))

file_out <- file.path(PROJHOME, "Writing", "duke", "abbreviations.tex")
cat("\\abbreviations\n\n\\begin{symbollist}\n", file=file_out)
cat(clean_acronyms$item, sep="\n", file=file_out, append=TRUE)
cat("\\end{symbollist}\n", file=file_out, append=TRUE)
