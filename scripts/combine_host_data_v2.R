
library(tidyverse)
library(butteR)

# READ IN A ALL COMPONENTS OF BOTH RAW AND CLEAN DATA
raw_iom_hh<- read.csv("inputs/IOM/raw_data/hh.csv",
                      stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))
raw_iom_indiv<- read.csv("inputs/IOM/raw_data/indv.csv",
                         stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))
raw_unhcr_hh<- read.csv("inputs/host/raw_data/hh.csv",
                        stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))

raw_unhcr_indiv<- read.csv("inputs/host/raw_data/indv.csv",
                           stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))


clean_iom_hh<- read.csv("inputs/IOM/clean_data/hh.csv",
                        stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))
clean_iom_indiv<- read.csv("inputs/IOM/clean_data/indv.csv",
                           stringsAsFactors = F, na.strings = c(""," ",'n/a',NA))

clean_unhcr_hh<- read.csv("inputs/host/clean_data/hh.csv",
                          stringsAsFactors = F, na.strings = c(""," ",'n/a',NA), encoding = "UTF-8")
clean_unhcr_indiv<- read.csv("inputs/host/clean_data/indv.csv",
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


write.csv(clean_hh_fixed,"inputs/host_combind/clean_data/hh.csv")
write.csv(clean_indiv_fixed,"inputs/host_combind/clean_data/indv.csv")
