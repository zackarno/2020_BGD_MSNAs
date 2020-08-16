library(tidyverse)
library(srvyr)
library(butteR)

if (population== "refugee" ) {
  kc= readxl::read_xlsx("DAP/refugee/tool/Pilot_JMSNA2020_Refugee_v2.xlsx",sheet = "choices")
  ks= readxl::read_xlsx("DAP/refugee/tool/Pilot_JMSNA2020_Refugee_v2.xlsx",sheet = "survey")
  df<-read.csv("inputs/refugee/raw_data/hh.csv",
               na.strings = c(""," "), stringsAsFactors = F)
  junky_cols<-c("end_note", "X_id", "X_uuid", "upazila","camp_name",
                "X_submission_time", "X_validation_status", "X_index","survey_date", "survey_start", 
                "deviceid", "end_survey", "instance_name", "note_consent",
                "audit", "enum_organisation", "enumerator_id", "enum_gender", 
                "respondent_id" )
    
}

if (population== "host" ) {
  kc= readxl::read_xlsx("DAP/host/tool/Pilot_JMSNA2020_HC_v2.xlsx",sheet = "choices")
  ks= readxl::read_xlsx("DAP/host/tool/Pilot_JMSNA2020_HC_v2.xlsx",sheet = "survey")
  df<-read.csv("inputs/host/raw_data/hh.csv",
               na.strings = c(""," "), stringsAsFactors = F)
  junky_cols<-c("end_note", "X_id", "X_uuid", 
                "X_submission_time", "X_validation_status", "X_index","survey_date", "survey_start", "deviceid", "end_survey", "instance_name", 
                "audit", "enum_organisation", "enumerator_id", "enum_gender", 
                "respondent_id", "upazilla_name", "union_name", "ward_name", 
                "intro_text")
  
  
}




xls_dictionary<-butteR::make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::english")
df_refactored<-butteR::refactor_to_xlsform(data = df,kobo_survey = ks,kobo_choices = kc,"label::english")
cols_to_analyze<- df_refactored %>% select(-junky_cols) %>% select(-ends_with("_other")) %>% colnames()

dfsvy<-as_survey(df_refactored)
analysis_by_enumerator<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "enumerator_id")
# analysis_by_enumerator_gender<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "enum_gender")

write.csv(analysis_by_enumerator,paste0("outputs/",population,"/enumerator_analysis/",day_to_run,"_",population,"_analysis_by_enumerator.csv"))
