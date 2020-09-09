rm(list = ls())

library(dplyr)
library(stringr)
library(lubridate)
# -------------------------------------------------------------------------

population<-c("host","refugee")[2]
write_output<-c("yes","no")[1]
day_to_run <- Sys.Date()
source("scripts/merge_and_clean_final_dataset.R")
source("scripts/active_path.R")

# read_data ---------------------------------------------------------------

hh <- read.csv(hh_path_clean, stringsAsFactors = FALSE, na.strings=c("", " ", NA)) %>% 
  filter(informed_consent == "yes")
indv <- read.csv(indv_path_clean, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

if ( population == "host") {
  number_response_by_union = hh %>% dplyr::group_by(union_name) %>% summarise(
    number_of_survey = n()
  )
write.csv(number_response_by_union,"outputs/host/number_of_survey.csv")
  }


# recoding ----------------------------------------------------------------

single_hoh <- c( "widow", "separated", "divorced", "single")
disability<- c(" some difficulty", "lot_of_difficulty", "cannot_do_at_all" )
health_distant<- c("60_180","more_180")
waste<- c("often", "always")
food_base_cping <- c("eating_less_preferred_food" ,  "borrowing_food" ,  
                     "limiting_portion_size" ,  "reducing_number_of_meals_a_day" ,  
                     "limiting_adults_food_intake" ,  "limiting_women_food_intake" ,  
                     "limiting_men_food_intake")
food_source_cash<- hh %>%  select(starts_with("food_source.")) %>% colnames() %>% dput()
critical_handwashing<- c("handwashing_three_times.before_eating", 
                         "handwashing_three_times.before_cooking", "handwashing_three_times.after_defecation", 
                         "handwashing_three_times.before_breastfeeding", "handwashing_three_times.before_feeding_children", 
                         "handwashing_three_times.after_handling_a_childs_stool")    

# can_go_alone_work_market<- c("work_mobility.can_go_alone" ,"market_mobility.can_go_alone")  
# can_go_alone_helath_wfs <-  c("health_mobility.can_go_alone" ,"wfs_mobility.can_go_alone")
# 
# can_go_accompanied_by_someone_work_market <- c("work_mobility.can_go_accompanied_by_someone_else" ,  
#                                     "market_mobility.can_go_accompanied_by_someone_else" )  
# can_go_accompanied_by_someone_helath_wfs <- c( "health_mobility.can_go_accompanied_by_someone_else" ,  
#                                     "wfs_mobility.can_go_accompanied_by_someone_else" ) 
# 
# cannot_go_at_all_work_market<- c("work_mobility.can_never_go" ,  "market_mobility.can_never_go")   
# cannot_go_at_all_helath_wfs<-c("health_mobility.can_never_go" ,  "wfs_mobility.can_never_go")

mobility_col_work_market <- c("work_mobility","market_mobility")
mobility_col_health_wfs <- c("wfs_mobility","health_mobility")

income_sources_all1 <- hh %>% dplyr::select(dplyr::starts_with("income_source.")) %>% colnames() %>% dput()
income_sources_yes <- c("income_source.remittances_from_abroad","income_source.assistance_from_relatives_and_friends",
                        "income_source.other_cash_assistance","income_source.borrowed_money")
income_sources_no <- hh %>% dplyr::select(c(dplyr::starts_with("income_source."),-income_sources_yes)) %>% 
                                                                  colnames() %>% dput()

shelter_improvements_reason <- hh %>% dplyr::select(dplyr::starts_with("improvement.")) %>% 
  select(-contains("no_improvement")) %>% select(-contains("dont_know")) %>% colnames() %>% dput()

shelter_issue_cols <- hh %>% dplyr::select(dplyr::starts_with("shelter_issues.")) %>% 
  select(-contains("no_issues")) %>% select(-contains("dont_know")) %>% colnames() %>% dput()

shelter_mobility_cols <- hh %>% dplyr::select(dplyr::starts_with("shelter_mobility."))%>% 
  select(-contains("no_issues")) %>% select(-contains("dont_know")) %>% colnames() %>% dput()

cooking_fuel_cols <- hh %>% dplyr::select(dplyr::starts_with("cooking_fuel.")) %>% 
  select(-contains("none")) %>% select(-contains("dont_know")) %>% colnames() %>% dput()

market_problem <- hh %>% dplyr::select(dplyr::starts_with("market_problems.")) %>% 
  select(-contains("none")) %>% select(-contains("dont_know")) %>% colnames() %>% dput()
awareness_cols <-c("what_preparation_should_be_taken_beforehand", 
                   "what_are_the_different_early_warning_flags"  ,
                   "what_are_the_sources_of_information"  , 
                   "what_is_covid_19_the_symptoms"  , 
                   "what_precautions_should_be_taken"  , 
                   "where_to_go_or_whom_to_contact" )


if(population == "refugee"){
  enough_info <- c("food_assistance"  , "livelihood"  , "water"  , "sanitation"  , "shelter"  , "non_food_items"  ,
                   "health_services"  , "nutrition_services"  , "remote_education"  , "protection"  , "site_management" )
  edu_no_formal <- c("no_education","madrassa_only") 
  edu_some_primary <- c("kindergarten","standard_1", "standard_2", "standard_3", "standard_4")
  primary_and_above <- c("standard_5", "standard_6", "standard_7", "standard_8",
                         "standard_9", "standard_10", "standard_11", "tertiary_education")
  hh$datearrival_shelter <- hh$datearrival_shelter %>% ymd()
  
  Some_primary<- c ("standard_1", "standard_2", "standard_3", "standard_4","kindergarten")
  Primary_and_above<- c("standard_5", "standard_6", "standard_7", "standard_8",
                         "standard_9", "standard_10", "standard_11", "tertiary_education")
  no_formal_education <- c("no_education","madrassa_only")
  emergecy_coping_stra <- c("depending_on_food_rations","begging","selling_labor","reduced_nonessential_expenditures")
  
}


if(population == "host"){
  enough_info <- c("food_assistance"  , "livelihood"  , "water"  , "sanitation"  , "non_food_items", 
                   "health_services"  , "nutrition_services"  , "remote_education"  , "protection" )
  
  primary_or_less <- c("madrasah_only","1","2","3","4","5") 
  some_secondary <- c("vocational","6", "7", "8", "9","10","11")
  secondary_and_above <- c("12", "above_grade_12")
  
  Some_primary<- c ("1", "2", "3", "4")
  Primary_and_above<- c("vocational","5","6", "7", "8", "9", "10", "11", "12", "above_grade_12")
  no_formal_education <- c("madrasah_only")
  
  emergecy_coping_stra <- c("depending_on_food_rations","begging","selling_labor")
  
  
}


# household to household --------------------------------------------------

hh_to_hh <- hh %>% mutate(
  I.HH_CHAR.gender_hoh.HH = if_else(respondent_hoh == "yes",resp_gender,hoh_gender),
  I.HH_CHAR.education_level_similar_as_2019.HH = if_else(edu_highest %in% Some_primary, "Some primary", 
                                         if_else( edu_highest %in% Primary_and_above, "Primary and above",
                                                  if_else(edu_highest %in% no_formal_education, "No formal Education","dont_know",missing = NULL ))),
  
  I.HH_CHAR.age_hoh.HH = if_else(respondent_hoh == "yes",respondent_age,hoh_age),
  I.HH_CHAR.food_base_cping.HH = if_else(rowSums(hh[,food_base_cping]== "yes",na.rm = T)>0,"yes","no",NULL),
  I.HH_CHAR.single_hoh.HH= if_else(hoh_marital %in% single_hoh,"yes","no"),
  I.HH_CHAR.elderly_hoh.HH= if_else(I.HH_CHAR.age_hoh.HH >59,"yes","no"),
  I.HH_CHAR.large_hh.HH = if_else(hh_size >4,"yes","no"),
  
  I.HH_CHAR.can_never_go_alone_work_market.HH= if_else(rowSums(hh[,mobility_col_work_market]== "not_applicable",na.rm = T)==2,NA_character_,
                              if_else(rowSums(hh[,mobility_col_work_market]=="can_never_go",na.rm = T)==2,"yes","no",NULL)),
  
  I.HH_CHAR.can_never_go_alone_health_wfs.HH= if_else(rowSums(hh[,mobility_col_health_wfs]== "not_applicable",na.rm = T)==2,NA_character_,
                              if_else(rowSums(hh[,mobility_col_health_wfs]=="can_never_go",na.rm = T)==2,"yes","no",NULL)),
  
  I.HH_CHAR.received_awarness_information.HH = if_else(rowSums(hh[,awareness_cols] == "yes",na.rm = T)==6,"yes","no",NULL),
  
  I.HEALTH.disabled_hh.HH = if_else(disability_seeing %in% disability|disability_hearing %in% disability|
                            disability_walking%in% disability|disability_remembering %in% disability|
                            disability_self_care%in% disability| disability_speaking %in% disability,"yes","no",NULL ),
  I.FSL.no_income.HH = if_else(income_source.none == 1,"yes","no",NULL),
  I.FSL.remittances.HH = if_else(income_source.remittances_from_abroad == 1,"yes","no",NULL),
  shelter_improvements_reason_rs = rowSums(hh[,shelter_improvements_reason],na.rm = T),
  I.SNFI.atlst_one_shelter_improvements_reason.HH = if_else(shelter_improvements_reason_rs > 0,"yes","no",NULL),
  # I.SNFI.improvement_reason_none.HH= na_if(improvement_reason.no_need_to_improve,y = 1),
  shelter_issue_cols_rs = rowSums(hh[,shelter_issue_cols],na.rm = T),
  shelter_mobility_cols_rs = rowSums(hh[,shelter_mobility_cols],na.rm = T),
  I.HH_CHAR.atlst_one_shelter_mobility.HH = if_else(shelter_mobility_cols_rs > 0,"yes","no",NULL),
  I.HH_CHAR.atlst_one_shelter_issue.HH = if_else(shelter_issue_cols_rs > 0,"yes","no",NULL),
  I.HH_CHAR.incm_src_employ_business.HH	= if_else(income_source.labor_inside_the_camp == 1 | 
                                                    income_source.labor_outside_the_camp == 1 | 
                                                    income_source.own_business == 1,"yes","no",NULL),
    
  I.HH_CHAR.rarely_never_feeling_consulted.HH = if_else(consulted == "rarely" | consulted == "never","yes","no",NULL),
  I.SNFI.improvements_despite_reporting_issue.HH = if_else(improvement.no_improvement == 1 & 
                                                             shelter_issue_cols_rs >0, "yes","no",NULL),
  cooking_fuel_rs =rowSums(hh[,cooking_fuel_cols],na.rm = T),
  I.SNFI.lpg_only.HH = if_else(cooking_fuel_rs == 1 & (cooking_fuel.receiving_lpg_refills == 1 |
                                                         cooking_fuel.buying_lpg_refills == 1),"yes",
                                if_else(cooking_fuel_rs == 2 & (cooking_fuel.receiving_lpg_refills == 1 &
                                                         cooking_fuel.buying_lpg_refills == 1),"yes","no",NULL)),
  I.EDU.not_send_back_to_school_hh.HH= if_else(not_send_back_to_school_total >= 1, "no", "yes",NULL),
  market_problem_rs = rowSums(hh[,market_problem],na.rm = T),
  I.FSL.allst_one_market_problme= if_else(market_problem_rs > 0, "yes","no",NULL),
  I.HEALTH.health_dist_more_60.HH= if_else(health_distance  %in% health_distant, "yes","no"),
  I.HEALTH.plw_enrolment_nfp.HH= if_else( plw_enrolment_nfp >= 1,"yes","no" ),
  I.FSL.fcs.HH= (cereals_and_tubers*2 + pulses*3 +
            vegetables*1 + fruits*1 + milk_and_dairy * 4 +
            meat_or_fish *4 + oil_and_fats * 0.5 +sweets*.5),
  I.FSL.fcs_acceptable.HH= if_else(I.FSL.fcs.HH>42,"yes","no"),
  I.FSL.fcs_borderline.HH= if_else(I.FSL.fcs.HH>28 & I.FSL.fcs.HH <= 42,"yes","no"),
  I.FSL.fcs_poor.HH= if_else(I.FSL.fcs.HH <=28, "yes","no"),
  I.FSL.food_consumption_score.HH = if_else(I.FSL.fcs.HH > 42, "Acceptable",
                                            if_else (I.FSL.fcs.HH >28 ,  "Borderline",
                                                     if_else(I.FSL.fcs.HH <= 28 , "Poor","no" ))),
  I.WASH.visible_waste_often_always.HH= if_else(visible_waste %in% waste,"yes","no"),
  critical_handwashing_rs= rowSums(hh[,critical_handwashing],na.rm = T),
  I.WASH.handwashing_critical_times.HH=if_else(critical_handwashing_rs==3,"yes" ,"no",NULL),
  food_source_cash_rs=rowSums(hh[,food_source_cash],na.rm = T),
  I.FSL.food_source_cash.HH= if_else(food_source_cash_rs==1 & food_source.purchase_cash==1,"yes","no"),
  I.FSL.food_source_borrow.HH= if_else(food_source.purchase_credit==1| (food_source.purchase_cash==1 & food_source.support_from_relatives==1)|
                                  food_source.army_distributing_food==1 | food_source.borrowing==1|food_source.food_assistance_food_card==1,"yes","no" ),
  I.FSL.food_source_assistance.HH= if_else(food_source_cash_rs==1 &(food_source.army_distributing_food==1 |food_source.food_assistance_food_card==1),"yes",
                                    if_else(food_source_cash_rs == 2 & (food_source.army_distributing_food==1 & food_source.food_assistance_food_card==1),"yes" ,"no",NULL)),
  I.EDU.hh_with_school_children.HH= if_else(school_children_total>1, "yes","no"),
  I.HEALTH.plw_total_hh.HH= if_else(plw_total>0, "yes", "no"),
  income_sources_yes_rs = rowSums(hh[,income_sources_yes],na.rm = T),
  income_sources_no_rs = rowSums(hh[,income_sources_no],na.rm = T),
  I.FSL.income_source_borrow.HH = if_else(income_sources_yes_rs > 0 & income_sources_no_rs == 0,"yes","no",NULL),
  I.HEALTH.pregnant_women_hh.HH	=	if_else(pregnant_woman >= 1 , "yes","no",NULL),
  I.HEALTH.atleast_onepregnant_ANC.HH = if_else(pregnant_women_anc > 0, "yes","no"),
  I.HEALTH.all_pregnant_women_anc.HH	= if_else(pregnant_woman == pregnant_women_anc,"yes","no",NULL),
  I.HH_CHAR.emergency_cping_strategy.HH = if_else(rowSums(hh[,emergecy_coping_stra] == "yes",na.rm = T)>0,"yes","no",NULL),
  I.EDU.not_send_back_to_school_total.response= sum(not_send_back_to_school_total,na.rm = T) / sum(school_children_total,na.rm = T)
)


if (population == "refugee") {
  hh_to_hh <- hh_to_hh %>% dplyr::mutate(
    I.HH_CHAR.not_having_light.HH	=if_else(enough_light.yes != 1 & enough_light.dont_know != 1,"yes","no",NULL),
    I.HH_CHAR.pay_anythng_to_live.HH = if_else(shelter_paid.no_need != 1 & shelter_paid.dont_know != 1,"yes","no",NULL),
    I.HH_CHAR.enough_information_for_all.HH = if_else(rowSums(hh_to_hh[,enough_info]== "yes",na.rm = T) == 11,"yes","no",NULL),
    I.HH_CHAR.enough_information_atlst_one.HH = if_else(rowSums(hh_to_hh[,enough_info]== "yes",na.rm = T) > 0,"yes","no",NULL),
    I.HH_CHAR.arrival_date.HH = if_else(ymd(datearrival_shelter) < ymd("2017-08-01"),"Before_August_2017",
                             if_else(ymd(datearrival_shelter) %in% ymd("2017-08-01"):ymd("2018-07-31") ,"aug_17_to_jul_18",
                                     if_else(ymd(datearrival_shelter) %in% ymd("2018-08-01"):ymd("2019-07-31") ,"aug_18_to_jul_19",
                                             if_else(ymd(datearrival_shelter) %in% ymd("2019-08-01"):ymd("2020-02-29") ,"aug_19_to_feb_20",
                                                     if_else(ymd(datearrival_shelter) > ymd("2020-02-29"),"afer_feb_2020","error",NULL)
                                             )))),
    I.HH_CHAR.highest_education.HH= if_else(edu_highest %in% edu_no_formal, "No formal education",
                                 if_else(edu_highest %in% edu_some_primary, "Some primary",
                                         if_else(edu_highest %in% primary_and_above, "Primary and above","dont_know",missing = NULL ))),
    I.HH_CHAR.bangla_english.HH= if_else(language_member.bangla==1 | language_member.english==1, "yes","no"),
    I.EDU.remote_learning_hh.HH= if_else(remote_learning_total< school_children_total, "no", "yes"),
    I.EDU.remote_learning_all.response = sum(remote_learning_total,na.rm = T) / sum(school_children_total,na.rm = T)
    
    )
}


if (population == "host") {
  hh_to_hh <- hh_to_hh %>% dplyr::mutate(
    I.HH_CHAR.enough_information_for_all.HH = if_else(rowSums(hh_to_hh[,enough_info]== "yes",na.rm = T) == 9,"yes","no",NULL),
    I.HH_CHAR.enough_information_atlst_one.HH = if_else(rowSums(hh_to_hh[,enough_info]== "yes",na.rm = T) > 0,"yes","no",NULL),
    I.HH_CHAR.valid_id_hh= if_else(valid_id == over_18_HH_count,"yes","no"),
    I.HH_CHAR.highest_education.HH= if_else(edu_highest %in% primary_or_less, "primary_or_less",
                                 if_else(edu_highest %in% some_secondary, "some_secondary",
                                         if_else(edu_highest %in% secondary_and_above, "secondary_and_above","dont_know",missing = NULL )))
  )
}


# individual to individual  -----------------------------------------------
indv_to_indv<- indv %>% dplyr::mutate(
  I.INDV_CHAR.age_groups.INDV= if_else( individual_age %in% 0:4,"0-4",
                         if_else(individual_age %in% 5:11, "5-11",
                                 if_else(individual_age %in% 12:17, "12-17",
                                         if_else(individual_age %in% 18:24, "18-24",
                                                 if_else(individual_age %in% 25:59, "25-59",
                                                         if_else(individual_age>=60, "60+","error",missing = NULL)))))),
  I.ind_gender = na_if(ind_gender, y="other"),
  I.INDV_CHAR.age_group_gender.INDV = if_else(!is.na(I.ind_gender),paste0(I.INDV_CHAR.age_groups.INDV,"_",I.ind_gender),NULL,NULL),
  ######
  I.HEALTH.ind_need_treatment_YES_0_17.INDV= case_when(!individual_age %in% 0:17  ~ NA_character_,
                                                         ind_need_treatment== "yes" ~ "yes",T~ "no"),
  I.HEALTH.ind_need_treatment_YES_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
                                                          ind_need_treatment== "yes" ~ "yes",T~ "no"),
  I.HEALTH.ind_need_treatment_YES_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
                                                       ind_need_treatment== "yes" ~ "yes",T~ "no"),
  
  I.HEALTH.ind_need_treatment_NO_0_17.INDV= case_when(!individual_age %in% 0:17  ~ NA_character_,
                                                        ind_need_treatment== "no" ~ "yes",T~ "no"),
  I.HEALTH.ind_need_treatment_NO_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
                                                         ind_need_treatment== "no" ~ "yes",T~ "no"),
  I.HEALTH.ind_need_treatment_NO_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
                                                      ind_need_treatment== "no"~ "yes",T~ "no"),
  
  I.HEALTH.ind_need_treatment_DONT_KNOW_0_17.INDV= case_when(!individual_age %in% 0:17  ~ NA_character_,
                                                               ind_need_treatment== "dont_know"~ "yes",T~ "no"),
  I.HEALTH.ind_need_treatment_DONT_KNOW_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
                                                                ind_need_treatment== "dont_know" ~ "yes",T~ "no"),
  I.HEALTH.ind_need_treatment_DONT_KNOW_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
                                                                ind_need_treatment== "dont_know" ~ "yes",T~ "no"),
  
  ######
  # I.HEALTH.ind_need_treatment_YES_m_0_17.INDV= case_when(!individual_age %in% 0:17  ~ NA_character_,
  #                       ind_need_treatment== "yes" & ind_gender == "male"~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_YES_m_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
  #                       ind_need_treatment== "yes"& ind_gender == "male" ~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_YES_m_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
  #                       ind_need_treatment== "yes" & ind_gender == "male"~ "yes",T~ "no"),
  # 
  # I.HEALTH.ind_need_treatment_NO_m_0_17.INDV= case_when(!individual_age %in% 0:17  ~ NA_character_,
  #                                                          ind_need_treatment== "no" & ind_gender == "male"~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_NO_m_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
  #                                                           ind_need_treatment== "no" & ind_gender == "male"~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_NO_m_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
  #                                                        ind_need_treatment== "no" & ind_gender == "male"~ "yes",T~ "no"),
  # 
  # I.HEALTH.ind_need_treatment_DONT_KNOW_m_0_17.INDV= case_when(!individual_age %in% 0:17  ~ NA_character_,
  #                                                         ind_need_treatment== "dont_know"& ind_gender == "male"~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_DONT_KNOW_m_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
  #                                                          ind_need_treatment== "dont_know"& ind_gender == "male" ~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_DONT_KNOW_m_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
  #                                                       ind_need_treatment== "dont_know"& ind_gender == "male" ~ "yes",T~ "no"),
  # #####
  # 
  # #####
  # I.HEALTH.ind_need_treatment_YES_f_0_17.INDV= case_when(!individual_age %in% 0:17  ~ NA_character_,
  #                                                        ind_need_treatment== "yes" & ind_gender == "female"~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_YES_f_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
  #                                                         ind_need_treatment== "yes"& ind_gender == "female" ~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_YES_f_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
  #                                                      ind_need_treatment== "yes" & ind_gender == "female"~ "yes",T~ "no"),
  # 
  # I.HEALTH.ind_need_treatment_NO_f_0_17.INDV= case_when(!individual_age %in% 0:17  ~ NA_character_,
  #                                                       ind_need_treatment== "no" & ind_gender == "female"~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_NO_f_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
  #                                                        ind_need_treatment== "no" & ind_gender == "female"~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_NO_f_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
  #                                                     ind_need_treatment== "no" & ind_gender == "female"~ "yes",T~ "no"),
  # 
  # I.HEALTH.ind_need_treatment_DONT_KNOW_f_0_17.INDV= case_when(!individual_age %in% 0:17  ~ NA_character_,
  #                                                              ind_need_treatment== "dont_know"& ind_gender == "female"~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_DONT_KNOW_f_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
  #                                                              ind_need_treatment== "dont_know"& ind_gender == "female" ~ "yes",T~ "no"),
  # I.HEALTH.ind_need_treatment_DONT_KNOW_f_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
  #                                                           ind_need_treatment== "dont_know"& ind_gender == "female" ~ "yes",T~ "no"),
  #####
  # I.HEALTH.ind_need_treatment_gender.INDV= paste0(ind_need_treatment,"_",ind_gender),
  I.HEALTH.ind_need_treatment_YES_m.INDV = case_when(!ind_gender == "male" ~ NA_character_,
                                                 ind_need_treatment == "yes" ~ "yes",T~ "no"),
  I.HEALTH.ind_need_treatment_YES_f.INDV = case_when(!ind_gender == "female" ~ NA_character_,
                                                     ind_need_treatment == "yes"~ "yes",T~ "no"),
  
  I.HEALTH.ind_need_treatment_no_m.INDV = case_when(!ind_gender == "male" ~ NA_character_,
                                                     ind_need_treatment == "no"~ "yes",T~ "no"),
  I.HEALTH.ind_need_treatment_no_f.INDV = case_when(!ind_gender == "female" ~ NA_character_,
                                                     ind_need_treatment == "no"~ "yes",T~ "no"),
  
  I.HEALTH.ind_need_treatment_DONT_KNOW_m.INDV = case_when(!ind_gender == "male" ~ NA_character_,
                                                     ind_need_treatment == "dont_know"~ "yes",T~ "no"),
  I.HEALTH.ind_need_treatment_DONT_KNOW_f.INDV = case_when(!ind_gender == "female" ~ NA_character_,
                                                     ind_need_treatment == "dont_know"~ "yes",T~ "no"),
  
  # ind_need_treatment_age_grp_0_17= case_when(!individual_age %in% 0:17  ~ NA_character_,
  #                                            individual_age %in% 0:17~ "yes",T~ "no"),
  # ind_need_treatment_age_grp_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
  #                                                  individual_age %in% 18:59 ~ "yes",T~ "no"),
  # ind_need_treatment_age_grp_60.INDV= case_when(!individual_age > 59  ~ NA_character_,
  #                                               individual_age > 59 ~ "yes",T~ "no"),
 
  I.INDV_CHAR.ind_work_5_17.INDV= case_when(!individual_age %in% 5:17  ~ NA_character_,
                                          ind_work== "yes"~ "yes",T~ "no"),
  I.INDV_CHAR.ind_work_18_59.INDV= case_when(!individual_age %in% 18:59  ~ NA_character_,
                                          ind_work== "yes"~ "yes",T~ "no"),
  I.INDV_CHAR.ind_work_60.INDV= case_when(!individual_age >59  ~ NA_character_,
                                          individual_age <5 ~ NA_character_,
                                          ind_work== "yes"~ "yes",T~ "no"),
  I.INDV_CHAR.ind_work_m.INDV= case_when( ind_gender != "male" ~ NA_character_,
                                          individual_age <5 ~ NA_character_,
                                          ind_work == "yes" ~ "yes",T~ "no"),
  I.INDV_CHAR.ind_work_f.INDV= case_when( ind_gender != "female" ~ NA_character_,
                                          individual_age <5 ~ NA_character_,
                                          ind_work == "yes" ~ "yes",T~ "no"),
  I.INDV_CHAR.ind_work_5_older.INDV  = case_when(!individual_age >= 5 ~ NA_character_,
                                                 ind_work== "yes"~ "yes",T~ "no"),
  
  # I.INDV_CHAR.ind_3_5.INDV= if_else( individual_age %in% 3:5, "yes","no"),
  I.INDV_CHAR.ind_3_5.INDV= if_else( individual_age %in% 3:5, "yes","no"),
  I.INDV_CHAR.ind_6_14.INDV=if_else( individual_age %in% 6:14, "yes","no"),
  I.INDV_CHAR.ind_15_18.INDV=if_else( individual_age %in% 15:18, "yes","no"),
  I.INDV_CHAR.ind_19_24.INDV=if_else( individual_age %in% 19:24, "yes","no"),
  I.INDV_CHAR.ind_0_4.INDV=if_else( individual_age <5,"yes","no"),
  I.INDV_CHAR.ind_5_11.INDV=if_else( individual_age %in% 5:11, "yes","no"),
  I.INDV_CHAR.ind_12_17.INDV=if_else( individual_age %in% 12:17, "yes","no"),
  I.INDV_CHAR.ind_18_24.INDV=if_else( individual_age %in% 18:24, "yes","no"),
  I.INDV_CHAR.ind_4_24.INDV=if_else(individual_age %in% 4:24, "yes","no"),
  I.INDV_CHAR.ind_6_18.INDV=if_else(individual_age %in% 6:18, "yes","no"),
  I.INDV_CHAR.ind_5_17.INDV=if_else(individual_age %in% 5:17, "yes","no"),
  I.INDV_CHAR.ind_0_17.INDV=if_else(individual_age<18, "yes","no"),
  I.INDV_CHAR.ind_18_59.INDV=if_else(individual_age %in% 18:59, "yes","no"),
  I.INDV_CHAR.ind_60.INDV=if_else(individual_age>=60, "yes","no"),
  I.INDV_CHAR.ind_6_59_months.INDV= if_else(individual_age %in% 0:4|(individual_age==0 & individual_age_mo>=6 ),"yes","no"),
  I.INDV_CHAR.ind_18_59.INDV=if_else(individual_age %in% 18:59, "yes","no"),
  I.INDV_CHAR.ind_60.INDV= if_else(individual_age>=60, "yes","no")
)
 

if (population == "refugee") {
  indv_to_indv <- indv_to_indv %>% dplyr::mutate(
    I.EDU.ind_ed_TLC_m_3_5.INDV = case_when(!individual_age %in% 3:5 ~ NA_character_,
                                         ind_gender != "male" ~ NA_character_,
                                         ind_ed_TLC == "yes" ~ "yes",T~ "no"),
    I.EDU.ind_ed_TLC_m_6_14.INDV = case_when(!individual_age %in% 6:14 ~ NA_character_,
                                          ind_gender != "male" ~ NA_character_,
                                          ind_ed_TLC == "yes"~ "yes",T~ "no"),
    I.EDU.ind_ed_TLC_m_15_18.INDV = case_when(!individual_age %in% 15:18 ~ NA_character_,
                                          ind_gender != "male" ~ NA_character_,
                                          ind_ed_TLC == "yes"~ "yes",T~ "no"),
    I.EDU.ind_ed_TLC_m_19_24.INDV = case_when(!individual_age %in% 19:24 ~ NA_character_,
                                          ind_gender != "male" ~ NA_character_,
                                          ind_ed_TLC == "yes" ~ "yes",T~ "no"),
    
    I.EDU.ind_ed_madrassa_m_3_5.INDV = case_when(!individual_age %in% 3:5 ~ NA_character_,
                                              ind_gender != "male" ~ NA_character_,
                                              is.na(ind_ed_madrassa) ~ NA_character_,
                                              ind_ed_madrassa == "yes" ~ "yes",T~ "no"),
    I.EDU.ind_ed_madrassa_m_6_14.INDV = case_when(!individual_age %in% 6:14 ~ NA_character_,
                                                ind_gender != "male" ~ NA_character_,
                                                is.na(ind_ed_madrassa) ~ NA_character_,
                                                ind_ed_madrassa == "yes" ~ "yes",T~ "no"),
    I.EDU.ind_ed_madrassa_m_15_18.INDV = case_when(!individual_age %in% 15:18 ~ NA_character_,
                                                 ind_gender != "male" ~ NA_character_,
                                                 is.na(ind_ed_madrassa) ~ NA_character_,
                                                 ind_ed_madrassa == "yes"~ "yes",T~ "no"),
    I.EDU.ind_ed_madrassa_m_19_24.INDV = case_when(!individual_age %in% 19:24 ~ NA_character_,
                                                 ind_gender != "male" ~ NA_character_,
                                                 is.na(ind_ed_madrassa) ~ NA_character_,
                                                 ind_ed_madrassa == "yes"~ "yes",T~ "no"),
    
    I.EDU.ind_ed_nonformal_m_3_5.INDV = case_when(!individual_age %in% 3:5 ~ NA_character_,
                                                  ind_gender != "male" ~ NA_character_,
                                                  is.na(ind_ed_nonformal) ~ NA_character_,
                                                ind_ed_nonformal == "yes" & ind_gender == "male"~ "yes",T~ "no"),
    I.EDU.ind_ed_nonformal_m_6_14.INDV = case_when(!individual_age %in% 6:14 ~ NA_character_,
                                                   ind_gender != "male" ~ NA_character_,
                                                   is.na(ind_ed_nonformal) ~ NA_character_,
                                                 ind_ed_nonformal == "yes" & ind_gender == "male"~ "yes",T~ "no"),
    I.EDU.ind_ed_nonformal_m_15_18.INDV = case_when(!individual_age %in% 15:18 ~ NA_character_,
                                                    ind_gender != "male" ~ NA_character_,
                                                    is.na(ind_ed_nonformal) ~ NA_character_,
                                                  ind_ed_nonformal == "yes" & ind_gender == "male"~ "yes",T~ "no"),
    I.EDU.ind_ed_nonformal_m_19_24.INDV = case_when(!individual_age %in% 19:24 ~ NA_character_,
                                                    ind_gender != "male" ~ NA_character_,
                                                    is.na(ind_ed_nonformal) ~ NA_character_,
                                                  ind_ed_nonformal == "yes" & ind_gender == "male"~ "yes",T~ "no"),
    
    
    ###
    I.EDU.ind_ed_TLC_f_3_5.INDV = case_when(!individual_age %in% 3:5 ~ NA_character_,
                                            ind_gender != "female" ~ NA_character_,
                                            ind_ed_TLC == "yes"& ind_gender == "female" ~ "yes",T~ "no"),
    I.EDU.ind_ed_TLC_f_6_14.INDV = case_when(!individual_age %in% 6:14 ~ NA_character_,
                                             ind_gender != "female" ~ NA_character_,
                                             ind_ed_TLC == "yes"& ind_gender == "female" ~ "yes",T~ "no"),
    I.EDU.ind_ed_TLC_f_15_18.INDV = case_when(!individual_age %in% 15:18 ~ NA_character_,
                                              ind_gender != "female" ~ NA_character_,
                                              ind_ed_TLC == "yes" & ind_gender == "female"~ "yes",T~ "no"),
    I.EDU.ind_ed_TLC_f_19_24.INDV = case_when(!individual_age %in% 19:24 ~ NA_character_,
                                              ind_gender != "female" ~ NA_character_,
                                              ind_ed_TLC == "yes"& ind_gender == "female" ~ "yes",T~ "no"),
    
    I.EDU.ind_ed_madrassa_f_3_5.INDV = case_when(!individual_age %in% 3:5 ~ NA_character_,
                                                 ind_gender != "female" ~ NA_character_,
                                                 is.na(ind_ed_madrassa) ~ NA_character_,
                                                 ind_ed_madrassa == "yes"& ind_gender == "female" ~ "yes",T~ "no"),
    I.EDU.ind_ed_madrassa_f_6_14.INDV = case_when(!individual_age %in% 6:14 ~ NA_character_,
                                                  ind_gender != "female" ~ NA_character_,
                                                  is.na(ind_ed_madrassa) ~ NA_character_,
                                                  ind_ed_madrassa == "yes"& ind_gender == "female" ~ "yes",T~ "no"),
    I.EDU.ind_ed_madrassa_f_15_18.INDV = case_when(!individual_age %in% 15:18 ~ NA_character_,
                                                   ind_gender != "female" ~ NA_character_,
                                                   is.na(ind_ed_madrassa) ~ NA_character_,
                                                   ind_ed_madrassa == "yes"& ind_gender == "female" ~ "yes",T~ "no"),
    I.EDU.ind_ed_madrassa_f_19_24.INDV = case_when(!individual_age %in% 19:24 ~ NA_character_,
                                                   ind_gender != "female" ~ NA_character_,
                                                   is.na(ind_ed_madrassa) ~ NA_character_,
                                                   ind_ed_madrassa == "yes" & ind_gender == "female"~ "yes",T~ "no"),
    
    I.EDU.ind_ed_nonformal_f_3_5.INDV = case_when(!individual_age %in% 3:5 ~ NA_character_,
                                                  ind_gender != "female" ~ NA_character_,
                                                  is.na(ind_ed_nonformal) ~ NA_character_,
                                                  ind_ed_nonformal == "yes" & ind_gender == "female"~ "yes",T~ "no"),
    I.EDU.ind_ed_nonformal_f_6_14.INDV = case_when(!individual_age %in% 6:14 ~ NA_character_,
                                                   ind_gender != "female" ~ NA_character_,
                                                   is.na(ind_ed_nonformal) ~ NA_character_,
                                                   ind_ed_nonformal == "yes" & ind_gender == "female"~ "yes",T~ "no"),
    I.EDU.ind_ed_nonformal_f_15_18.INDV = case_when(!individual_age %in% 15:18 ~ NA_character_,
                                                    ind_gender != "female" ~ NA_character_,
                                                    is.na(ind_ed_nonformal) ~ NA_character_,
                                                    ind_ed_nonformal == "yes" & ind_gender == "female"~ "yes",T~ "no"),
    I.EDU.ind_ed_nonformal_f_19_24.INDV = case_when(!individual_age %in% 19:24 ~ NA_character_,
                                                    ind_gender != "female" ~ NA_character_,
                                                    is.na(ind_ed_nonformal) ~ NA_character_,
                                                    ind_ed_nonformal == "yes" & ind_gender == "female"~ "yes",T~ "no")
    
    ###
    )
}


if (population == "host") {
  indv_to_indv <- indv_to_indv %>% dplyr::mutate(
  
I.EDU.ind_school_dropout.INDV= case_when( !individual_age %in% 4:24 ~ NA_character_,
      ind_formal_learning == "none" & ind_formal_learning_none != "none" ~ "yes",T~ "no"),

I.EDU.ind_formal_learning_f_4.INDV=case_when(!individual_age == 4 ~ NA_character_,
                                             ind_gender != "female" ~ NA_character_,
                                              ind_formal_learning != "none" & ind_gender == "female"~ "yes",T~ "no"),
I.EDU.ind_formal_learning_f_5_11.INDV=case_when(!individual_age %in% 5:11  ~ NA_character_, 
                                                ind_gender != "female" ~ NA_character_,
                                              ind_formal_learning != "none" & ind_gender == "female"~ "yes", T~ "no"),
I.EDU.ind_formal_learning_f_12_17.INDV=case_when(!individual_age %in% 12:17 ~ NA_character_, 
                                                 ind_gender != "female" ~ NA_character_,
                                              ind_formal_learning != "none"& ind_gender == "female" ~ "yes",T~ "no"),
I.EDU.ind_formal_learning_f_18_24.INDV=case_when(!individual_age %in% 18:24  ~ NA_character_, 
                                                 ind_gender != "female" ~ NA_character_,
                                              ind_formal_learning != "none"& ind_gender == "female" ~ "yes",  T~ "no"),

I.EDU.ind_nonformal_learn_f_4.INDV=case_when(!individual_age == 4 ~ NA_character_, 
                                             ind_gender != "female" ~ NA_character_,
                                              ind_nonformal_learn != "none"& ind_gender == "female" ~ "yes",T~ "no"),
I.EDU.ind_nonformal_learn_f_5_11.INDV=case_when(!individual_age %in% 5:11 ~ NA_character_, 
                                                ind_gender != "female" ~ NA_character_,
                                              ind_nonformal_learn != "none" & ind_gender == "female"~ "yes", T~ "no"),
I.EDU.ind_nonformal_learn_f_12_17.INDV=case_when(!individual_age %in% 12:17  ~ NA_character_, 
                                                 ind_gender != "female" ~ NA_character_,
                                              ind_nonformal_learn != "none" & ind_gender == "female"~ "yes", T~ "no"),
I.EDU.ind_nonformal_learn_f_18_24.INDV=case_when(!individual_age %in% 18:24  ~ NA_character_, 
                                                 ind_gender != "female" ~ NA_character_,
                                               ind_nonformal_learn != "none"& ind_gender == "female" ~ "yes", T~ "no"),
####

I.EDU.ind_formal_learning_m_4.INDV=case_when(!individual_age == 4 ~ NA_character_,
                                             ind_gender != "male" ~ NA_character_,
                                               ind_formal_learning != "none" & ind_gender == "male"~ "yes",T~ "no"),
I.EDU.ind_formal_learning_m_5_11.INDV=case_when(!individual_age %in% 5:11  ~ NA_character_, 
                                                ind_gender != "male" ~ NA_character_,
                                                ind_formal_learning != "none" & ind_gender == "male"~ "yes", T~ "no"),
I.EDU.ind_formal_learning_m_12_17.INDV=case_when(!individual_age %in% 12:17 ~ NA_character_,
                                                 ind_gender != "male" ~ NA_character_,
                                                 ind_formal_learning != "none"& ind_gender == "male" ~ "yes",T~ "no"),
I.EDU.ind_formal_learning_m_18_24.INDV=case_when(!individual_age %in% 18:24  ~ NA_character_, 
                                                 ind_gender != "male" ~ NA_character_,
                                                 ind_formal_learning != "none"& ind_gender == "male" ~ "yes",  T~ "no"),

I.EDU.ind_nonformal_learn_m_4.INDV=case_when(!individual_age == 4 ~ NA_character_, 
                                             ind_gender != "male" ~ NA_character_,
                                             ind_nonformal_learn != "none"& ind_gender == "male" ~ "yes",T~ "no"),
I.EDU.ind_nonformal_learn_m_5_11.INDV=case_when(!individual_age %in% 5:11 ~ NA_character_, 
                                                ind_gender != "male" ~ NA_character_,
                                                ind_nonformal_learn != "none" & ind_gender == "male"~ "yes", T~ "no"),
I.EDU.ind_nonformal_learn_m_12_17.INDV=case_when(!individual_age %in% 12:17  ~ NA_character_, 
                                                 ind_gender != "male" ~ NA_character_,
                                                 ind_nonformal_learn != "none" & ind_gender == "male"~ "yes", T~ "no"),
I.EDU.ind_nonformal_learn_m_18_24.INDV=case_when(!individual_age %in% 18:24  ~ NA_character_, 
                                                 ind_gender != "male" ~ NA_character_,
                                                 ind_nonformal_learn != "none"& ind_gender == "male" ~ "yes", T~ "no")

####
  )  
  } 

# household to individual -------------------------------------------------

hh_to_indv1<- indv_to_indv %>% group_by(X_submission__uuid) %>% summarise(
  
  dependents = sum(individual_age<15,na.rm = T) + sum(individual_age>64,na.rm = T),
  non_dependent = sum(individual_age %in% 15:64,na.rm = T),
  I.HH_CHAR.adlt_male_in_hh.INDVHH = if_else(sum(individual_age>17 & ind_gender == "male",na.rm = T) >0 ,"yes","no"),
  I.HH_CHAR.atlst_no_child_nutrition_scrnd.INDVHH = if_else(any(child_nutrition_screened == "no",na.rm = T),"yes",
                                                            if_else(all(is.na(child_nutrition_screened)),NA_character_,"no",NULL)),
  I.HH_CHAR.dep_ratio.INDVHH= (dependents/non_dependent),
  I.HH_CHAR.no_working_age.INDVHH=if_else(non_dependent== 0, "yes","no"),
  I.HH_CHAR.no_male_working_age.INDVHH= if_else(any(individual_age %in% 15:64 & ind_gender== "male"),"no","yes"),
  I.HH_CHAR.high_dep_ratio.INDVHH=if_else(I.HH_CHAR.dep_ratio.INDVHH>2,"yes","no"),
  I.HH_CHAR.ind_work_5_17_hh.INDVHH = if_else(sum(I.INDV_CHAR.ind_work_5_17.INDV == "yes",na.rm = T) >0,"yes","no",NULL),
  I.HH_CHAR.ind_6_59_months_hh.INDVHH=if_else(sum(I.INDV_CHAR.ind_6_59_months.INDV == "yes",na.rm = T) >0,"yes","no",NULL),
  I.HH_CHAR.ind_5_17_hh.INDVHH=if_else(sum(I.INDV_CHAR.ind_5_17.INDV == "yes",na.rm = T) >0,"yes","no",NULL),
  I.HEALTH.ind_need_treatment_hh.INDVHH=if_else(sum(ind_need_treatment== "yes",na.rm = T) >0,"yes","no",NULL),
  I.NUTRITION.child_enrolment_nfp_tota.INDVHH = sum(child_enrolment_nfp == "yes",na.rm = T),
  I.HH_CHAR.ind_work_hh.INDVHH = if_else(any(ind_work == "yes" & individual_age > 17,na.rm = T),"yes",
                                         if_else(all(is.na(ind_work)),NA_character_,"no",NULL)),
  
  )

if (population== "refugee"){
  hh_to_indv2<- indv_to_indv %>% group_by(X_submission__uuid) %>% summarise(
    I.HH_CHAR.atlst_one_birth_plc_home.INDVHH = if_else(any(ind_birth_place== "at_home",na.rm = T),"yes",
                                                        if_else(all(is.na(ind_birth_place)),NA_character_,"no",NULL)),
    I.EDU.ind_out_of_school.INDVHH= if_else(sum(ind_ed_TLC=="no" & individual_age %in% 6:18,na.rm = T )>0, "yes","no",NULL)
  )
}


if (population == "host"){
  hh_to_indv2<- indv_to_indv %>% dplyr::group_by(X_submission__uuid) %>% dplyr::summarise(
  I.HH_CHAR.atlst_one_birth_plc_home.INDVHH = if_else(any(ind_birth_place== "at_home",na.rm = T),"yes",
                                                      if_else(all(is.na(ind_birth_place)),NA_character_,"no",NULL)),
    
    I.EDU.ind_out_of_school.INDVHH= if_else(sum(ind_formal_learning == "none" & individual_age %in% 5:17,na.rm = T)>0, "yes","no",NULL)
  )
}

hh_to_indv <- hh_to_indv1 %>% left_join(hh_to_indv2)


# compile and write dataset ---------------------------------------------------------

compile_dataset <- hh_to_hh %>% left_join(hh_to_indv,by =c ("X_uuid"="X_submission__uuid")) %>% mutate(
  I.NUTRITION.i.ind_6_59_months_plw_hh.HH = if_else(I.HH_CHAR.ind_6_59_months_hh.INDVHH == "yes" |
                                                      I.HEALTH.plw_total_hh.HH == "yes","yes","no",NULL),
  I.HH_CHAR.dep_percent_working_age.INDVHH= (non_dependent/hh_size)*100
  
)

rank_cols<-compile_dataset %>% select(starts_with("rank"),-ends_with("label")) %>% colnames()

compile_dataset <- compile_dataset %>% 
  mutate_at(
    rank_cols,~case_when(
      .==3~1,
      .==1~3,
      .==2~2,
      is.na(.)~0,
      TRUE~NA_real_)
  )
compile_dataset$I.HH_CHAR.dep_ratio.INDVHH <- na_if(compile_dataset$I.HH_CHAR.dep_ratio.INDVHH, y = Inf)

if (write_output == "yes") {
  write.csv(compile_dataset,paste0("outputs/",population,"/composite_indicator/",str_replace_all(day_to_run,"-","_"),"_composite_indicator.csv"))
  write.csv(compile_dataset,paste0("outputs/",population,"/composite_indicator/composite_indicator.csv"))
  
  write.csv(indv_to_indv,paste0("outputs/",population,"/composite_indicator/",str_replace_all(day_to_run,"-","_"),"_INDV_composite_indicator.csv"))
  write.csv(indv_to_indv,paste0("outputs/",population,"/composite_indicator/INDV_composite_indicator.csv"))
}


# response ----------------------------------------------------------------

if (population == "host"){
  
  response_by_upazilla <- hh %>% dplyr::group_by(upazilla_name) %>% dplyr::summarise(
    I.EDU.not_send_back_to_school_total.response= sum(not_send_back_to_school_total,na.rm = T) / sum(school_children_total,na.rm = T)
    )
}

if (population == "refugee"){

response_by_upazilla <- hh %>% dplyr::group_by(upazila) %>% dplyr::summarise(
  I.EDU.remote_learning_all.response= sum(remote_learning_total,na.rm = T) / sum(school_children_total,na.rm = T),
  I.EDU.not_send_back_to_school_total.response= sum(not_send_back_to_school_total,na.rm = T) / sum(school_children_total,na.rm = T)
  )
}

write.csv(response_by_upazilla,paste0("outputs/butteR_basic_analysis/",population,"/response_level_analysis_by_upzilaa.csv"))
