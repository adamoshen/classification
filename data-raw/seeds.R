library(tidyverse)

seeds <- read_lines("./data-raw/seeds_raw.txt") %>%
  str_replace_all(pattern="\\t{2,}", replacement="\t") %>%
  str_c("\n") %>%
  read_delim(col_names=FALSE) %>%
  set_names(c(
    "area", "peri", "comp", "length_k",
    "width_k", "asymmetry", "length_g", "type"
  )) %>%
  mutate(type = str_c("class", type)) %>%
  select(-comp) %>%
  filter(type %in% c("class1", "class2"))

usethis::use_data(seeds, overwrite=TRUE)
