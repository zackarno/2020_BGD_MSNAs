library(dplyr)

# source ------------------------------------------------------------------

population<-c("host","refugee","IOM")[1]
write_output<-c("yes","no")[1]
day_to_run <- Sys.Date()
source("scripts/active_path.R")

# raw_data ----------------------------------------------------------------

raw_data_hh <- read.csv(hh_path,stringsAsFactors = FALSE, na.strings=c("", " ", NA)) %>% 
  filter(informed_consent == "yes")

clean_data_hh <- read.csv(hh_path_clean,stringsAsFactors = FALSE, na.strings=c("", " ", NA)) %>% 
  filter(informed_consent == "yes")



# cleaning_log ------------------------------------------------------------


butteR::check_cleaning_log(df = raw_data_hh,df_uuid = "X_uuid",cl = clean_data_hh,
                           cl_change_type_col = "A",
                           cl_change_col = "B",cl_uuid = "X_uuid",cl_new_val = "c")
