library(tidyverse)
library(butteR)

# READ IN A ALL COMPONENTS OF BOTH RAW AND CLEAN DATA
raw_iom_hh<- read.csv("inputs/host/raw_data/HC_IOM_raw_data_final_hh.csv",
                      stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))
raw_iom_indiv<- read.csv("inputs/host/raw_data/HC_IOM_raw_data_final_indiv.csv",
                         stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))
raw_unhcr_hh<- read.csv("inputs/host/raw_data/HC_UNHCR_raw_data_final_hh.csv",
                      stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))

raw_unhcr_indiv<- read.csv("inputs/host/raw_data/HC_UNHCR_raw_data_final_indiv.csv",
                         stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))
clean_iom_hh<- read.csv("inputs/host/clean_data/HC_IOM_CLEAN_hh.csv",
                      stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))
clean_iom_indiv<- read.csv("inputs/host/clean_data/HC_IOM_CLEAN_indiv.csv",
                         stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))
clean_unhcr_hh<- read.csv("inputs/host/clean_data/HC_UNHCR_CLEAN_hh.csv",
                      stringsAsFactors = F, na.strings = c(""," ",'n/a',NA), encoding = "UTF-8")
clean_unhcr_indiv<- read.csv("inputs/host/clean_data/HC_UNHCR_CLEAN_indiv.csv",
                         stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))

all(colnames(raw_iom_hh) %in% colnames(raw_unhcr_hh))
all(colnames(raw_unhcr_hh) %in% colnames(raw_iom_hh))
all(colnames(raw_unhcr_hh) %in% colnames(raw_iom_hh))

clean_unhcr_hh<- clean_unhcr_hh %>% 
  rename(information_barriers.language_that_i_don.t_understand="information_barriers.language_that_i_don.U.0092.t_understand")


#SINCE IOM AND UNHCR WAS SPLIT INTO TO TOOLS WE NEED TO MAKE SURE THERE ARE NO DUPLICATED UUIDS
if(any(clean_iom_hh$X_uuid %in%clean_unhcr_hh$X_uuid)|
any(clean_iom_indiv$X_submission__uuid %in% clean_unhcr_indiv$X_submission__uuid)|
any(clean_iom_indiv$repeat_instance_name %in% clean_unhcr_indiv$repeat_instance_name)){print("Houston we have a problem")} else{print("good to go")}

#COMBINE DATA SETS, FOR SOME REASON WE HAVE CLASS CONFLICT ON DEVICEID
raw_hh<- bind_rows(raw_iom_hh %>% mutate(deviceid=as.character(deviceid)),raw_unhcr_hh)
raw_indiv<- bind_rows(raw_iom_indiv, raw_unhcr_indiv)


clean_hh<- bind_rows(clean_iom_hh %>% mutate(deviceid=as.character(deviceid)), clean_unhcr_hh)
clean_indiv<-bind_rows(clean_iom_indiv, clean_unhcr_indiv)


#WE HAVE TO MAKE ONE SMALL TWEAK TO CLEAN DATA DUE TO ISSUE IN TOOL. NUTR_BARRIERS QUESTIONS SHOULD HAVE ONLY BEEN ASKED TO FAMILIES WITH A CHILD LESS THAN 5 (OR A PREGNANT/LACTATING INDIVIDUAL). HOWEVER THE QUESTION WAS ASKED TO FAMILIES WITH ONE CHILE LESS THAN OR EQUAL TO 5. FURTHER DESCRIPTION SHOULD BE NOTED IN CLEAN DATA SET README

hh_all_gte_5<-clean_indiv %>% 
  group_by(X_submission__uuid) %>% 
  summarise(
    lt_5_HH= if_else(any(individual_age<5),1,0)
  ) %>% 
  filter(lt_5_HH==0) %>% 
  pull(X_submission__uuid) 

#THESE ARE THE COLUMNS TO FIX 
nutr_barrier_cols<-clean_hh %>% select(starts_with("nutrition_barriers")) %>% colnames()
nutr_barrier_choices_col<-clean_hh %>% select(starts_with("nutrition_barriers.")) %>% colnames()

fix_nutr_cols<- function(x){
  x<-as.character(x)
  fixed_x_char<-case_when(clean_hh$plw_total>0|!is.na(clean_hh$plw_total)~x,
                          clean_hh$X_uuid %in% hh_all_gte_5~NA_character_,
                          TRUE~x) 
  return(fixed_x_char)
}

clean_hh_fixed<- clean_hh %>% 
  mutate_at(
    .vars = nutr_barrier_cols,.funs = fix_nutr_cols
  ) %>% 
  mutate_at(
    .vars = nutr_barrier_choices_col,
    .funs = ~as.integer(.)
  ) 

# ADDITIONALLY, RECORDS WERE DELETED FROM HH DATA, BUT NOT DELETED FROM INDIVIDUAL DATA SET, THESE NEED TO BE REMOVED FROM INDIVIDUAL DATASET

clean_indiv_fixed<-clean_indiv %>% 
  filter(X_submission__uuid %in% clean_hh_fixed$X_uuid)






# NOW WE CAN GENERATE THE CLEANING LOGS
cleaning_logs<-list()

cleaning_logs$hh<-butteR::df_comparison_log(raw_data = raw_hh,
                                            clean_data = clean_hh_fixed,
                                            raw_data_uuid = "X_uuid",
                                            clean_data_uuid = "X_uuid"
) %>% 
  mutate(data_type="HH")


# debugonce(butteR::df_comparison_log)
cleaning_logs$indiv<-butteR::df_comparison_log(raw_data = raw_indiv,
                                            clean_data = clean_indiv_fixed,
                                            raw_data_uuid = "X_submission__uuid",
                                            clean_data_uuid = "X_submission__uuid"
) %>% 
  mutate(data_type="Individual")

cleaning_logs_merged<-bind_rows(cleaning_logs) %>% 
  mutate(description= "") %>% 
  select(uuid, data_type, change_type, column_changed, old_value, new_value, description)

cleaning_logs_merged$column_changed %>% table() %>% sort()

cleaning_logs_merged_filtered<-cleaning_logs_merged %>% 
  filter(!column_changed %in% c("X_parent_index", "X_index"))

# cleaning_logs_merged_filtered %>% write.csv("outputs/20200818_Host_Community_MSNA_2020_Cleaning_Log.csv",row.names = F)

cleaning_logs_merged_filtered %>% 
  filter(str_detect(column_changed,"nutrition_barrier")) %>% print(n=nrow(.)) %>% 
  filter(is.na(new_value))






#  EVERYTHING BELOW IS SCRAP - SHOULD DELETE LATER

















cleaning_logs<-list()
cleaning_logs$iom_hh_cleaning_log<-butteR::df_comparison_log(raw_data = raw_iom_hh,
                          clean_data = clean_iom_hh,
                          raw_data_uuid = "X_uuid",
                          clean_data_uuid = "X_uuid"
                          ) %>% 
  mutate(data_type="HH")


cleaning_logs$unhcr_hh_cleaning_log<-butteR::df_comparison_log(raw_data = raw_unhcr_hh,
                                                             clean_data = clean_unhcr_hh,
                                                             raw_data_uuid = "X_uuid",
                                                             clean_data_uuid = "X_uuid"
) %>% 
  mutate(data_type="HH")



cleaning_logs$iom_indiv_cleaning_log<-butteR::df_comparison_log(raw_data = raw_iom_indiv,
                          clean_data = clean_iom_indiv,
                          raw_data_uuid = "repeat_instance_name",
                          clean_data_uuid = "repeat_instance_name"
                          ) %>% 
  mutate(data_type="Individual")

cleaning_logs$unhcr_indiv_cleaning_log<-butteR::df_comparison_log(raw_data = raw_unhcr_indiv,
                          clean_data = clean_unhcr_indiv,
                          raw_data_uuid = "repeat_instance_name",
                          clean_data_uuid = "repeat_instance_name"
                          ) %>% 
  mutate(data_type="Individual")

cleaning_logs %>% map_dbl(nrow) 



cleaning_logs_merged_filtered %>% slice(2439) %>% pull(uuid)

raw_unhcr_hh %>% filter(X_uuid=="ade482c9-523d-42b3-b2de-b5e8f9ecdfaa")


which(raw_unhcr_hh$X_uuid %in% raw_iom_hh$X_uuid)
which(raw_unhcr_indiv$repeat_instance_name %in%  raw_iom_indiv$repeat_instance_name)
