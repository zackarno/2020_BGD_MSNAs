library(tidyverse)
library(butteR)

population<- c("host","refugee")[1]
# READ IN A ALL COMPONENTS OF BOTH RAW AND CLEAN DATA
if(population=="host"){
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

# FOUND A COLUMN WITH A DIFFERENT NAME IN THE UNHCR CLEAN DATASET
clean_unhcr_hh<- clean_unhcr_hh %>% 
  rename(information_barriers.language_that_i_don.t_understand="information_barriers.language_that_i_don.U.0092.t_understand")


#SINCE IOM AND UNHCR WAS SPLIT INTO TWO TOOLS WE NEED TO MAKE SURE THERE ARE NO DUPLICATED UUIDS
if(any(clean_iom_hh$X_uuid %in%clean_unhcr_hh$X_uuid)|
any(clean_iom_indiv$X_submission__uuid %in% clean_unhcr_indiv$X_submission__uuid)|
any(clean_iom_indiv$repeat_instance_name %in% clean_unhcr_indiv$repeat_instance_name)){print("Houston we have a problem")} else{print("good to go")}

#COMBINE DATA SETS, FOR SOME REASON WE HAVE CLASS CONFLICT ON DEVICEID
raw_hh<- bind_rows(raw_iom_hh %>% mutate(deviceid=as.character(deviceid)),raw_unhcr_hh)
raw_indiv<- bind_rows(raw_iom_indiv, raw_unhcr_indiv)


clean_hh<- bind_rows(clean_iom_hh %>% mutate(deviceid=as.character(deviceid)), clean_unhcr_hh)
clean_indiv<-bind_rows(clean_iom_indiv, clean_unhcr_indiv)}

if (population=="refugee"){
  # clean_hh<- read.csv("")
  # clean_indiv<- read.csv("")
  # raw_hh <- 
  # raw_indiv <- 
}




# SKIP LOGIC ERRORS IN TOOL NEED TO BE FIXED. LUCKILY, THESE ERRORS MEANT THAT QUESTIONS WERE ASKED TO TOO MANY HHs/INDIVIDUALS RATHER THAN TOO LITTLE.

# TO FIX:
# A.) NUTR_BARRIERS QUESTIONS SHOULD HAVE ONLY BEEN ASKED TO FAMILIES WITH A CHILD 6-59 MONTHS OLD (OR A PREGNANT/LACTATING INDIVIDUAL). HOWEVER THE QUESTION WAS ASKED TO ANY FAMILY WITH A CHILD 0-71 MONTHS OLD.
# B.) SAME ADJUSTMENT AS ABOVE BUT WITH OUT THE OR STATEMENT ON PREGNANT OR LACTATING WOMEN
# C.) INDIVIDUAL CHILD ENROLMENT QUESTIONS SHOULD HAVE ONLY BEEN ASKED TO 6-59 MONTH OLDS. THAT MEANS TOTAL COUNT IN HH DATASET SHOULD ALSO BE ADJUSTED.
#FURTHER DESCRIPTION SHOULD BE NOTED IN CLEAN DATA SET README


#HERE ARE EXAMPLES OF WHERE THE QUESTIONS SHOULD HAVE BEEN SKIPPED
clean_hh %>% filter(X_uuid=="9b7883e2-f38a-45b1-8c8d-67e8e3659e26") %>% select(starts_with("nutrition_barriers"))
clean_indiv %>% filter(X_submission__uuid=="9b7883e2-f38a-45b1-8c8d-67e8e3659e26") %>% select(individual_age_mo,individual_age)



# clean_indiv<-clean_indiv %>% 
#   filter(X_submission__uuid %in% clean_hh_fixed$X_uuid)

clean_indiv2<-clean_indiv %>%
  mutate(individual_age=ifelse(individual_age==0,individual_age_mo/12, individual_age),
         lt_5_yr_gte_6_mo= individual_age<5 & individual_age>=0.5,
         child_enrolment_nfp= ifelse(lt_5_yr_gte_6_mo==T,child_enrolment_nfp,NA),
         child_enrolment_nfp_count= ifelse(lt_5_yr_gte_6_mo==T,child_enrolment_nfp_count,0),
         child_nutrition_screened= ifelse(lt_5_yr_gte_6_mo==T,child_nutrition_screened,NA)

         ) %>% 
  select(-lt_5_yr_gte_6_mo)

clean_indiv2 %>% select(child_enrolment_nfp,child_enrolment_nfp_count)
clean_indiv %>% filter(!is.na(child_enrolment_nfp)) %>% nrow()
clean_indiv$child_enrolment_nfp %>% table(useNA = "ifany")
clean_indiv$child_enrolment_nfp_count %>% table(useNA = "ifany")

cbind(clean_indiv2$lt_5_yr_gte_6_mo,clean_indiv$child_enrolment_nfp,clean_indiv$child_enrolment_nfp_count,clean_indiv2$child_enrolment_nfp,clean_indiv2$child_enrolment_nfp_count)


clean_indiv2$child_enrolment_nfp %>% table(useNA = "ifany")
clean_indiv2$child_enrolment_nfp_count %>% table(useNA = "ifany")

clean_indiv %>% filter(!is.na(child_enrolment_nfp_count)) %>% nrow()

clean_indiv2 %>% filter(!is.na(child_enrolment_nfp)) %>% nrow()
clean_indiv2 %>% filter(!is.na(child_enrolment_nfp_count)) %>% nrow()





uuid_gte_5_yr_lt_6_mo<-clean_indiv2 %>%  #INDIVIDUAL DATASET
  mutate(individual_age=ifelse(individual_age==0,individual_age_mo/12, individual_age)) %>% 
  group_by(X_submission__uuid) %>%  # LOOK AT EACH FAMILY
  summarise(
    lt_5_HH= if_else(any(individual_age<5 & individual_age>=0.5),1,0) # IF ANY FAMILY HAS A CHILD LESS THAN 5, BUT GREATER THAN 6 MONTH GIVE THAT FAMILY A 1
  ) %>% 
  filter(lt_5_HH==0) %>% #NOW LOOK AT ALL FAMILIES THAT DO NOT HAV CHILD LESS THAN FICE, BUT GREATER THAN 6 MONTH
  pull(X_submission__uuid) #EXTRACT THOSE UUIDS

#THESE ARE THE COLUMNS TO FIX 


nutr_barrier_cols<-clean_hh %>% select(starts_with("nutrition_barriers")) %>% colnames()
nutr_barrier_choices_col<-clean_hh %>% select(starts_with("nutrition_barriers."),child_enrolment_nfp_total) %>% colnames()



fix_nutr_cols<- function(x){
  plw_total_na_replaced<- ifelse(is.na(clean_hh$plw_total),0,clean_hh$plw_total)#MAKE VAR WHERE PLW NAS ARE REPLACED WITH 0
  x<-as.character(x)
  fixed_x_char<-case_when(plw_total_na_replaced>0~x, #IF PLW_TOTAL >0, LEAVE IT BE
                          clean_hh$X_uuid %in% uuid_gte_5_yr_lt_6_mo~NA_character_, #IF UUID IS IN THE EXTRACTED UUID ABOVE MAKE SURE IT GOES TO NA
                          TRUE~x) 
  return(fixed_x_char)
}


#need  to include informed consent! 
child_enrolment_total_fixed_from_indiv<- clean_indiv2 %>% 
  # filter(X_submission__uuid %in% clean_hh_fixed$X_uuid) %>% 
  group_by(X_submission__uuid) %>% 
  summarise(
    # child_enrolment_nfp_total = if(all(is.na(child_enrolment_nfp_count))) NA_real_ else sum(child_enrolment_nfp_count, na.rm = TRUE)
    child_enrolment_nfp_total = sum(child_enrolment_nfp_count, na.rm = TRUE)
    ) %>% 
  select(X_uuid=X_submission__uuid,child_enrolment_nfp_total)



# JOIN FIXED  CHILD ENROLLMENT TOTAL WITH CLEAN HH DATA SET
clean_hh_chid_enrollment_nfp_fixed<- clean_hh %>%  
  select(-child_enrolment_nfp_total) %>% 
  left_join(child_enrolment_total_fixed_from_indiv, by="X_uuid") 

#CHECK SOMETHING.. CAN THROW OUT
clean_hh_chid_enrollment_nfp_fixed %>% 
  filter(X_uuid %in% clean_indiv2$X_submission__uuid ) %>% 
  pull(X_uuid) %>% length()



# clean_hh_chid_enrollment_nfp_fixed %>% select(child_enrolment_nfp_total.x,child_enrolment_nfp_total.y)


clean_hh_chid_enrollment_nfp_fixed %>% nrow()
child_enrolment_total_fixed_from_indiv %>% nrow()

clean_hh_fixed<-clean_hh_chid_enrollment_nfp_fixed  %>%
  select(colnames(clean_hh)) %>% # GET ORDER BACK TO ORIGINAL AGAIN
  mutate(
    nutrition_pack=ifelse(X_uuid %in% uuid_gte_5_yr_lt_6_mo,NA,nutrition_pack )
    ) %>%
  mutate_at(
    .vars = nutr_barrier_cols,.funs = fix_nutr_cols
  ) %>% 
  mutate_at(
    .vars = nutr_barrier_choices_col,
    .funs = ~as.integer(.)
  ) 


#MAKE SURE CHANGES WAS IMPLEMENTED CORRECTLY
clean_hh_fixed %>% filter(X_uuid =="79a9ff6d-d6b5-4e5e-9730-8f73bf239b83") %>% select(starts_with("nutrition_barriers"), plw_total)
# ADDITIONALLY, RECORDS WERE DELETED FROM HH DATA, BUT NOT DELETED FROM INDIVIDUAL DATA SET, THESE NEED TO BE REMOVED FROM INDIVIDUAL DATASET

clean_indiv_fixed<-clean_indiv2 %>% 
  filter(X_submission__uuid %in% clean_hh_fixed$X_uuid)

#CHECK TO MAKE SURE IT IS CORREFT LENGTH
clean_indiv_fixed$X_submission__uuid %>% unique() %>% length()




# NOW WE CAN GENERATE THE CLEANING LOGS
cleaning_logs<-list()

cleaning_logs$hh<-butteR::df_comparison_log(raw_data = raw_hh,
                                            clean_data = clean_hh_fixed,
                                            raw_data_uuid = "X_uuid",
                                            clean_data_uuid = "X_uuid"
) %>% 
  mutate(data_type="HH")

# all(raw_indiv$child_enrolment_nfp==clean_indiv_fixed$child_enrolment_nfp)
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

cleaning_logs_merged$column_changed %>% table() %>% sort() %>% data.frame() %>% filter(str_detect(.,"^child_enrolment_nfp"))

cleaning_logs_merged_filtered<-cleaning_logs_merged %>% 
  filter(!column_changed %in% c("X_parent_index", "X_index"))

# cleaning_logs_merged_filtered %>% write.csv("outputs/20200822_Host_Community_MSNA_2020_Cleaning_Log.csv",row.names = F)
# clean_hh_fixed %>% write.csv("inputs/host/clean_data/hh.csv",row.names = F)
# clean_indiv_fixed %>% write.csv("inputs/host/clean_data/indv.csv",row.names = F)

cleaning_logs_merged_filtered %>% 
  filter(str_detect(column_changed,"^nutrition_barriers."))  %>%
  filter(!str_detect(column_changed,"other$")) %>% 
  filter(is.na(new_value)) %>% pull(uuid) %>% unique()
  
  







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
