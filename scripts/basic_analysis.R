rm(list = ls())

library(tidyverse)
library(butteR)
library(survey)
library(srvyr)
library(forcats)
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


# responserate ------------------------------------------------------------

if(population == "host"){
  hh_data2 <- hh_data %>% select(X_uuid,df_strata,resp_gender,I.HH_CHAR.gender_hoh.HH,I.HH_CHAR.adlt_male_in_hh.INDVHH)
}

if(population == "refugee"){
  hh_data2 <- hh_data %>% select(X_uuid,"upazila",resp_gender,I.HH_CHAR.gender_hoh.HH,I.HH_CHAR.adlt_male_in_hh.INDVHH)
}

ind_data2 <- ind_data %>% left_join(hh_data2,by =c("X_submission__uuid"= "X_uuid"))

hh_cols <- hh_data %>% colnames()
indv_cols <- ind_data2 %>% colnames()

overall_rate_hh <- hh_data %>% summarise_at(.vars = hh_cols,.funs = ~ sum(!is.na(.)))
overall_rate_indv <- ind_data2 %>% summarise_at(.vars = indv_cols,.funs = ~ sum(!is.na(.)))


overall_rate_by_resp_gender_hh <- hh_data %>% dplyr::group_by(resp_gender)%>% dplyr::summarise_at(names(hh_data %>% select(-resp_gender)),.funs = ~ sum(!is.na(.)))
overall_rate_by_resp_gender_indv <- ind_data2 %>% dplyr::group_by(resp_gender) %>% summarise_at(names(ind_data2 %>% select(-resp_gender)),.funs = ~ sum(!is.na(.)))

overall_rate_by_adult_male_hh <- hh_data %>% dplyr::group_by(I.HH_CHAR.adlt_male_in_hh.INDVHH)%>% dplyr::summarise_at(names(hh_data %>% select(-I.HH_CHAR.adlt_male_in_hh.INDVHH)),.funs = ~ sum(!is.na(.)))
overall_rate_by_adult_male_indv <- ind_data2 %>% dplyr::group_by(I.HH_CHAR.adlt_male_in_hh.INDVHH) %>% summarise_at(names(ind_data2 %>% select(-I.HH_CHAR.adlt_male_in_hh.INDVHH)),.funs = ~ sum(!is.na(.)))


overall_rate_by_hoh_gender_hh <- hh_data %>% dplyr::group_by(I.HH_CHAR.gender_hoh.HH)%>% dplyr::summarise_at(names(hh_data %>% select(-I.HH_CHAR.gender_hoh.HH)),.funs = ~ sum(!is.na(.)))
overall_rate_by_hoh_gender_indv <- ind_data2 %>% dplyr::group_by(I.HH_CHAR.gender_hoh.HH) %>% summarise_at(names(ind_data2 %>% select(-I.HH_CHAR.gender_hoh.HH)),.funs = ~ sum(!is.na(.)))

if(population == "host"){
  overall_rate_by_upazila_hh <- hh_data %>% dplyr::group_by(upazilla_name)%>% dplyr::summarise_at(names(hh_data %>% select(-upazilla_name)),.funs = ~ sum(!is.na(.)))
  overall_rate_by_upazila_indv <- ind_data2 %>% dplyr::group_by(upazilla_name) %>% summarise_at(names(ind_data2 %>% select(-upazilla_name)),.funs = ~ sum(!is.na(.)))
  
}

if(population == "refugee"){
  overall_rate_by_upazila_hh <- hh_data %>% dplyr::group_by(upazila)%>% dplyr::summarise_at(names(hh_data %>% select(-upazila)),.funs = ~ sum(!is.na(.)))
  overall_rate_by_upazila_indv <- ind_data2 %>% dplyr::group_by(upazila) %>% summarise_at(names(ind_data2 %>% select(-upazila)),.funs = ~ sum(!is.na(.)))
  
}

if(write_output == "yes"){
write.csv(overall_rate_hh,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_HH.csv"))
write.csv(overall_rate_indv,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_indv.csv"))

write.csv(overall_rate_by_resp_gender_hh,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_by_resp_genderHH.csv"))
write.csv(overall_rate_by_resp_gender_indv,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_by_resp_genderindv.csv"))

write.csv(overall_rate_by_hoh_gender_hh,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_by_hoh_genderHH.csv"))
write.csv(overall_rate_by_hoh_gender_indv,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_by_hoh_gender_indv.csv"))

write.csv(overall_rate_by_adult_male_hh,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_by_adult_male_HH.csv"))
write.csv(overall_rate_by_adult_male_indv,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_adult_male_indv.csv"))


write.csv(overall_rate_by_upazila_hh,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_by_upazila_HH.csv"))
write.csv(overall_rate_by_upazila_indv,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_response_rate_by_upazila_indv.csv"))

}

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
write.csv(df,paste0("outputs/",population,"/data_sets_with_weights/",population,"_data_set_weights_hh.csv"))

# lt<-butteR::make_xlsform_lookup_table(kobo_survey = tool_survey_sheet,kobo_choices = tool_choices_sheet
#                                       ,label_column = "label::english")
# 
# 
# select_multiples<-lt %>% filter(str_detect(question_type, "^select_mult")) %>% pull(xml_format_data_col) %>% 
#   unique()
# 
# select_multiples_hh<-select_multiples[select_multiples %in% colnames(hh_data)]
# 
#  
#  df<-df %>%
#    mutate_at(select_multiples_hh, function(x) fct_expand(as.factor(x),c("0","1")))


 # df<-df %>%
 #   mutate_at(.vars = select_multiples_hh, ~fct_expand("0","1"))

# 
#  df$modality_shelter.materials %>% AMR::freq()
#  df$shelter_issues.shelter_is_hard_to_access %>% AMR::freq()
#  
#  df$shelter_issues.shelter_is_hard_to_access %>% unique()
 

df <- butteR::refactor_to_xlsform(data = df,kobo_survey = tool_survey_sheet ,
                                  kobo_choices = tool_choices_sheet ,label_column = "label::english")



rank_cols<-df %>% select(starts_with("rank"),-ends_with("label")) %>% colnames()

df <- df %>% mutate_at(rank_cols,function(x) (as.character(x)))
df <- df %>% mutate_at(rank_cols,function(x) (as.integer(x)))
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


cols_to_analyze<-df %>% select(-starts_with("Other"), -ends_with("_other")) %>%
  select_if(.,is_not_empty) %>% select(-dont_analyze_in_data) %>% colnames() 

yes_other_cols <- df %>%  select(ends_with(".yes_other")) %>% colnames() %>% dput()

cols_to_analyze <- c(cols_to_analyze,"masks_source.mask_source_other",yes_other_cols,"other_reasons.yes_covid","other_reasons.no") 

if(population == "host"){
  
  dfsvy$variables$I.HH_CHAR.enough_information_for_all.HH<- forcats::fct_expand(dfsvy$variables$I.HH_CHAR.enough_information_for_all.HH,c( "no", "yes"))
  dfsvy$variables$I.FSL.food_source_assistance.HH<- forcats::fct_expand(dfsvy$variables$I.FSL.food_source_assistance.HH,c( "no", "yes"))
  dfsvy$variables$I.HH_CHAR.no_working_age.INDVHH<- forcats::fct_expand(dfsvy$variables$I.HH_CHAR.no_working_age.INDVHH,c( "no", "yes"))
}



basic_analysis<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze)
basic_analysis_by_hoh<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "I.HH_CHAR.gender_hoh.HH")
basic_analysis_by_adlt_male<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "I.HH_CHAR.adlt_male_in_hh.INDVHH")
basic_analysis_by_resp_gender<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                         aggregation_level = "resp_gender")

if (population == "host") {
basic_analysis_by_strata<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                    aggregation_level = df_strata)
basic_analysis_by_strata_hoh<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                        aggregation_level = c(df_strata,"I.HH_CHAR.gender_hoh.HH"))
}

if (population == "refugee") {
  basic_analysis_by_strata<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                      aggregation_level = "upazila")
  basic_analysis_by_strata_hoh<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                          aggregation_level = c("upazila","I.HH_CHAR.gender_hoh.HH"))
}

if (write_output == "yes") {
  write.csv(basic_analysis,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_HH.csv"))
  write.csv(basic_analysis_by_hoh,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_by_HoH_HH.csv"))
  write.csv(basic_analysis_by_resp_gender,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_by_resp_gender_HH.csv"))
  
  write.csv(basic_analysis_by_strata,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_by_strata_HH.csv"))
  write.csv(basic_analysis_by_strata_hoh,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_by_strata_HoH_HH.csv"))
  
  write.csv(basic_analysis_by_adlt_male,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_by_adlt_male_HH.csv"))
  
  }


# individual loop ---------------------------------------------------------
if(population == "host"){
df_weight <- df %>% select(X_uuid,survey_weight,df_strata,resp_gender,I.HH_CHAR.adlt_male_in_hh.INDVHH)
}

if(population == "refugee"){
  df_weight <- df %>% select(X_uuid,survey_weight,"upazila",resp_gender,I.HH_CHAR.adlt_male_in_hh.INDVHH)
}

indv_with_weights <- ind_data %>% left_join(df_weight,by=c("X_submission__uuid"="X_uuid"))

indv_with_weights<- indv_with_weights %>% filter(!is.na(survey_weight))

write.csv(indv_with_weights,paste0("outputs/",population,"/data_sets_with_weights/",population,"_data_set_weights_indv.csv"))


indv_with_weights <- butteR::refactor_to_xlsform(data = indv_with_weights,kobo_survey = tool_survey_sheet ,
                                  kobo_choices = tool_choices_sheet ,label_column = "label::english")


dfsvy_indv<-svydesign(ids = ~1,data = indv_with_weights,weights = formula(paste0("~", "survey_weight")))

dont_analyze_indv<-c( "X", "parent_instance_name","repeat_instance_name",
                      "X_index", "X_parent_table_name", "X_parent_index", "X_submission__id", 
                      "X_submission__uuid", "X_submission__submission_time")

dont_analyze_in_data_indv<-dont_analyze_indv[dont_analyze_indv %in% colnames(indv_with_weights)]
is_not_empty<-function(x){ all(is.na(x))==FALSE}


cols_to_analyze_indv<-indv_with_weights %>% select(-starts_with("Other"), -ends_with("_other")) %>%
  select_if(.,is_not_empty) %>% select(-dont_analyze_in_data_indv) %>% colnames() 


# if(population == "host"){
#   dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_f_60.INDV<- forcats::fct_expand(dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_f_60.INDV,c( "no", "yes"))
# }

if(population == "refugee"){
  
   dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_0_17.INDV<- forcats::fct_expand(dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_0_17.INDV,c( "no", "yes"))
   dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_18_59.INDV<- forcats::fct_expand(dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_18_59.INDV,c( "no", "yes"))
   dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_60.INDV<- forcats::fct_expand(dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_60.INDV,c( "no", "yes"))
   dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_m.INDV<- forcats::fct_expand(dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_m.INDV,c( "no", "yes"))
   dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_f.INDV<- forcats::fct_expand(dfsvy_indv$variables$I.HEALTH.ind_need_treatment_DONT_KNOW_f.INDV,c( "no", "yes"))

   }


basic_analysis_indv<-butteR::mean_prop_working(design = dfsvy_indv,list_of_variables = cols_to_analyze_indv)
basic_analysis_indv_by_resp_gender<-butteR::mean_prop_working(design = dfsvy_indv,list_of_variables = cols_to_analyze_indv,
                                                         aggregation_level = "resp_gender" )
basic_analysis_indv_by_adlt_male<-butteR::mean_prop_working(design = dfsvy_indv,list_of_variables = cols_to_analyze_indv,
                                                              aggregation_level = "I.HH_CHAR.adlt_male_in_hh.INDVHH" )

if (population == "host"){
  basic_analysis_indv_by_strata<-butteR::mean_prop_working(design = dfsvy_indv,list_of_variables = cols_to_analyze_indv,
                                                           aggregation_level = df_strata )
  
}
if (population == "refugee"){
  basic_analysis_indv_by_strata<-butteR::mean_prop_working(design = dfsvy_indv,list_of_variables = cols_to_analyze_indv,
                                                           aggregation_level = "upazila" )
  
}


if (write_output == "yes") {
  write.csv(basic_analysis_indv,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_INDV.csv"))
  write.csv(basic_analysis_indv_by_strata,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_by_strata_INDV.csv"))
  write.csv(basic_analysis_indv_by_resp_gender,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_by_resp_gender_INDV.csv"))
  write.csv(basic_analysis_indv_by_adlt_male,paste0("outputs/butteR_basic_analysis/",population,"/",str_replace_all(day_to_run,"-","_"),"_basic_analysis_by_adlt_male_INDV.csv"))
  
  }
