rm(list = ls())

library(tidyverse)
library(butteR)
library(survey)
library(srvyr)
library(dplyr)

population<-c("host","refugee")[2]
write_output<-c("yes","no")[1]
day_to_run <- Sys.Date()
source("scripts/active_path.R")

# read_data ---------------------------------------------------------------

hh_data <- read.csv(recoding_output_hh, stringsAsFactors = FALSE, na.strings=c("", " ", NA))
ind_data <- read.csv(recoding_output_indv, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

tool_survey_sheet <- readxl::read_xls(tool_path,sheet = "survey")
tool_choices_sheet <- readxl::read_xls(tool_path,sheet = "choices")

pop <- read.csv(pop_path,stringsAsFactors = FALSE, na.strings=c("", " ", NA))



# weighting ---------------------------------------------------------------

if(population == "refugee"){
  pop<-pop %>% 
    filter(!is.na(Camp),is.na(Block)) %>% 
    # filter(Camp!="Kutupalong RC") %>% 
    mutate(
      !!(sf_strata):=stringr::str_replace(Camp, "Total","") %>% trimws(),
      !!(sf_strata):= stringr::str_replace_all(Camp,"Extension","Ext"),
      Total.Families=readr::parse_number(Total.Families %>% stringr::str_replace_all(",","")),
      Total.Individuals= readr::parse_number(Total.Individuals %>% stringr::str_replace_all(",",""))
    ) 
  hh_data$camp_name_fix <- hh_data$camp_name %>% str_replace_all("_"," ") %>% str_replace_all("e","E") %>% 
    str_replace_all("w","W") %>% str_replace_all("camp ktp","Kutupalong RC") %>% 
    str_replace_all("camp nya","Nayapara RC")  %>% str_replace_all("c","C") %>% 
    str_replace_all("20E","20 Ext") %>% str_replace_all("4E","4 Ext")
 
  sf_with_weights<-hh_data %>% 
    group_by(!!sym(df_strata)) %>% 
    summarise(sample_strata_num=n()) %>% 
    right_join(pop, by=setNames(sf_strata,df_strata)) %>% 
    mutate(sample_global=sum(sample_strata_num),
           pop_global=sum(!!sym(sf_pop)),
           survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
    )   
write.csv(sf_with_weights,paste0("outputs/",population,"/weights/",population,"_weights.csv"))
sf_with_weights <- sf_with_weights %>% select(camp_name_fix,survey_weight)

}

if(population == "host"){
  
  pop <- pop %>% dplyr::group_by(Upazila) %>% dplyr::summarise(
    HH_pop = sum(HH_pop)
  )

hh_data$upazilla_name <- hh_data$upazilla_name %>% str_replace_all("teknaf","Teknaf") %>% 
  str_replace_all("ukhiya","Ukhiya")
  
  
sf_with_weights<-hh_data %>% 
    group_by(!!sym(df_strata)) %>% 
    summarise(sample_strata_num=n()) %>% 
    right_join(pop, by=setNames(sf_strata,df_strata)) %>% 
    mutate(sample_global=sum(sample_strata_num),
           pop_global=sum(!!sym(sf_pop)),
           survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
    )
write.csv(sf_with_weights,paste0("outputs/",population,"/weights/",population,"_weights.csv"))
sf_with_weights <- sf_with_weights %>% select(upazilla_name,survey_weight)
}


# data_for_butteR ---------------------------------------------------------

df <- hh_data %>% left_join(sf_with_weights)


# butter analysis  --------------------------------------------------------
dfsvy<-svydesign(ids = ~1,strata = formula(paste0("~",df_strata)),data = df,weights = formula(paste0("~", "survey_weight")))



if (population == "refugee") {
  dont_analyze<-c( "X", "survey_date", "survey_start", "deviceid", "end_survey", 
                 "instance_name", "enum_organisation", "enumerator_id", "datearrival_shelter",
                 "enum_gender", "respondent_id", "upazila", "camp_name", 
                 "informed_consent", "fcn_consent", "hh_fcn", "block_number", 
                 "camp_name_fix","referral_contact", "phone_number", "enum_comment", 
                 "X_id", "X_uuid", "X_submission_time", 
                 "X_index")}

if (population == "host") {
  dont_analyze<-c( "X", "survey_date", "survey_start", "deviceid", "end_survey", 
                   "instance_name", "enum_organisation", "enumerator_id", 
                   "enum_gender", "respondent_id",  "upazilla_name", "union_name", 
                   "ward_name", 
                   "informed_consent",  
                   "referral_contact", "phone_number", "enum_comment", 
                   "X_id", "X_uuid", "X_submission_time", 
                   "X_index")}

dont_analyze_in_data<-dont_analyze[dont_analyze %in% colnames(df)]
is_not_empty<-function(x){ all(is.na(x))==FALSE}


cols_to_analyze<-df %>% select(-starts_with("Other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% select(-dont_analyze_in_data) %>% colnames() 


if(population == "host"){
  dfsvy$variables$shelter_issues_other<- forcats::fct_expand(dfsvy$variables$shelter_issues_other,c( "Homeless", "other"))
  dfsvy$variables$shelter_materials_source_other<- forcats::fct_expand(dfsvy$variables$shelter_materials_source_other,
                                                                       c( "Received from community leader", "other"))
  dfsvy$variables$cooking_fuel_other<- forcats::fct_expand(dfsvy$variables$cooking_fuel_other,c( "Dry leaves", "other"))
  dfsvy$variables$masks_not_used_other<- forcats::fct_expand(dfsvy$variables$masks_not_used_other,c( "No COVID-19 reported in area", "other"))
  dfsvy$variables$i.food_source_assistance<- forcats::fct_expand(dfsvy$variables$i.food_source_assistance,c( "no", "yes"))
  dfsvy$variables$i.no_working_age<- forcats::fct_expand(dfsvy$variables$i.no_working_age,c( "no", "yes"))
  dfsvy$variables$feedback_problems_other<- forcats::fct_expand(dfsvy$variables$feedback_problems_other,c( "He is afraid to make a complaint ", "other"))
  
}


basic_analysis<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze)

if (write_output == "yes") {
  write.csv(basic_analysis,paste0("outputs/",population,"/butteR_basic_analysis/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_HH.csv"))
}


# individual loop ---------------------------------------------------------

df_weight <- df %>% select(X_uuid,survey_weight)
indv_with_weights <- ind_data %>% left_join(df_weight,by=c("X_submission__uuid"="X_uuid"))

indv_with_weights$i.age_groups

dfsvy_indv<-svydesign(ids = ~1,data = indv_with_weights,weights = formula(paste0("~", "survey_weight")))

dont_analyze_indv<-c( "X", "parent_instance_name","repeat_instance_name",
                      "X_index", "X_parent_table_name", "X_parent_index", "X_submission__id", 
                      "X_submission__uuid", "X_submission__submission_time")

dont_analyze_in_data_indv<-dont_analyze_indv[dont_analyze_indv %in% colnames(indv_with_weights)]
is_not_empty<-function(x){ all(is.na(x))==FALSE}


cols_to_analyze_indv<-indv_with_weights %>% select(-starts_with("Other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% select(-dont_analyze_in_data_indv) %>% colnames() 


basic_analysis_indv<-butteR::mean_prop_working(design = dfsvy_indv,list_of_variables = cols_to_analyze_indv)

if (write_output == "yes") {
  write.csv(basic_analysis_indv,paste0("outputs/",population,"/butteR_basic_analysis/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_INDV.csv"))
}
