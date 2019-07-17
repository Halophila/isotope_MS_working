library(here)
library(tidyverse)

list_of_fig_files <- list.files(here("sup")) %>% 
  str_subset(pattern = ".R")

for(i in seq_along(list_of_fig_files)){
  source(here("sup", list_of_fig_files[i]))
}


list_of_figs <- list.files(here()) %>% 
  str_subset(pattern = "png|pdf")

for(i in seq_along(list_of_figs)){
  file.rename(from = here(list_of_figs[i]), 
              to = here("sup", list_of_figs[i]))
}
