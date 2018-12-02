library(haven)
library(tidyverse)

ahrf <- read_sas("C:/Users/niwi8/OneDrive - cumc.columbia.edu/Practicum/opioid_prediction/data/raw/ahrf.sas7bdat")


ahrf_selected <- read_csv("data/ahrf_selected_variables.csv")
  
check_label <- function(x){
  if (attr(ahrf[[x]], "label") %in% ahrf_selected$label) {
    return(x)
  }
}

# check_label("f1529815")

keep_var <- list()

for (i in names(ahrf)) {
 keep_var[i] <- check_label(i)
}

keep_var <- names(keep_var)

ahrf <- ahrf %>% 
  select(keep_var)

<<<<<<< HEAD
data.table::setnames(ahrf, old = keep_var, new = ahrf_selected$label)
=======
keep_names <- list()

for (i in 1:ncol(ahrf)) {
  keep_names[[i]] <- attr(ahrf[[i]], "label")
}

for (i in 1:length(keep_names)) {
  names(keep_names)[i] <- "lulz"
}

keep_names <- data.frame(keep_names) %>% 
  gather(keep, label, lulz:lulz.17) %>% 
  select(label)

data.table::setnames(ahrf, old = keep_var, new = keep_names$label)
>>>>>>> 552cbca9b4d51bf3aa73d83ff51f914f9fa736e7

ahrf <- ahrf %>% 
  janitor::clean_names() %>% 
  filter(state_name == "New York")

ahrf %>% 
  write_csv("data/ahrf_select_data.csv")


