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

data.table::setnames(ahrf, old = keep_var, new = ahrf_selected$label)

ahrf %>% 
  write_csv("data/ahrf_select_data.csv")


