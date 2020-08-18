rm(list = ls())

library(dplyr)
library(stringr)
library(lubridate)
# -------------------------------------------------------------------------

population<-c("host","refugee")[2]
write_output<-c("yes","no")[1]
day_to_run <- Sys.Date()
if(population == "host") {source("scripts/combine_host_data.R")}
source("scripts/active_path.R")

# read_data ---------------------------------------------------------------

hh <- read.csv(hh_path_clean, stringsAsFactors = FALSE, na.strings=c("", " ", NA)) %>% 
  filter(informed_consent == "yes")
indv <- read.csv(indv_path_clean, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

# recoding ----------------------------------------------------------------

single_hoh <- c( "widow", "separated", "divorced", "single")
disability<- c(" some difficulty", "lot_of_difficulty", "cannot_do_at_all" )
health_distant<- c("60_180","more_180")
waste<- c("often", "always")
food_source_cash<- hh %>%  select(starts_with("food_source.")) %>% colnames() %>% dput()
critical_handwashing<- c("handwashing_three_times.before_eating", 
                         "handwashing_three_times.before_cooking", "handwashing_three_times.after_defecation", 
                         "handwashing_three_times.before_breastfeeding", "handwashing_three_times.before_feeding_children", 
                         "handwashing_three_times.after_handling_a_childs_stool")                                 

if(population == "refugee"){
  edu_no_formal <- c("no_education","madrassa_only") 
  edu_some_primary <- c("kindergarten","standard_1", "standard_2", "standard_3", "standard_4")
  primary_and_above <- c("standard_5", "standard_6", "standard_7", "standard_8",
                         "standard_9", "standard_10", "standard_11", "tertiary_education")
  hh$datearrival_shelter <- hh$datearrival_shelter %>% ymd()
  language_bangla_english<- c("language_member.bangla","language_member.english")
  
}

if(population != "refugee"){
  edu_no_formal <- c("vocational","madrassa_only") 
  edu_some_primary <- c("1", "2", "3", "4")
  primary_and_above <- c("5", "6", "7", "8",
                         "9", "10", "11","12", "above_grade_12"
  )
}


# household to household --------------------------------------------------

hh_to_hh <- hh %>% mutate(
  I.HH_CHAR.gender_hoh.HH
  = if_else(respondent_hoh == "yes",resp_gender,hoh_gender),
  I.HH_CHAR.age_hoh.HH = if_else(respondent_hoh == "yes",respondent_age,hoh_age),
  I.HH_CHAR.single_hoh.HH= if_else(hoh_marital %in% single_hoh,"yes","no"),
  I.HH_CHAR.elderly_hoh.HH= if_else(I.HH_CHAR.age_hoh.HH >59,"yes","no"),
  I.HH_CHAR.large_hh.HH = if_else(hh_size >4,"yes","no"),
  I.HH_CHAR.highest_education.HH = if_else(edu_highest %in% edu_no_formal,"no_formal_education",
                                if_else(edu_highest %in% edu_some_primary,"some_primary",
                                        if_else(edu_highest %in% primary_and_above,"primary_and_above","ERROR",NULL))),
  I.HEALTH_CHAR.disabled_hh.HH = if_else(disability_seeing %in% disability|disability_hearing %in% disability|
                            disability_walking%in% disability|disability_remembering %in% disability|
                            disability_self_care%in% disability| disability_speaking %in% disability,"yes","no",NULL ),
  I.FSL.no_income.HH = if_else(income_source.none == 1,"yes","no",NULL),
  I.FSL.remittances.HH = if_else(income_source.remittances_from_abroad == 1,"yes","no"),
  I.SNFI.improvement_reason_none.HH= na_if(improvement_reason.no_need_to_improve,y = 1),
  I.EDU.not_send_back_to_school_hh.HH= if_else(not_send_back_to_school_total >= 1, 0, 1),
  I.HEALTH.health_dist_more_60.HH= if_else(health_distance  %in% health_distant, "yes","no"),
  I.HEALTH.plw_enrolment_nfp.HH= if_else( plw_enrolment_nfp >= 1,"yes","no" ),
  I.FSL.fcs.HH= (cereals_and_tubers*2 + pulses*3 +
            vegetables*1 + fruits*1 + milk_and_dairy * 4 +
            meat_or_fish *3 + oil_and_fats * 0.5 +sweets*.5 ),
  I.FSL.fcs_acceptable.HH= if_else(I.FSL.fcs.HH>42,"yes","no"),
  I.FSL.fcs_borderline.HH= if_else(I.FSL.fcs.HH>28 & I.FSL.fcs.HH<42,"yes","no"),
  I.FSL.fcs_poor.HH= if_else(I.FSL.fcs.HH<=28, "yes","no"),
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
  I.EDU.not_send_back_to_school_total.HH= sum(not_send_back_to_school_total,na.rm = T) / sum(school_children_total,na.rm = T)
  
)



if (population == "refugee") {
  hh_to_hh <- hh_to_hh %>% dplyr::mutate(
    I.HH_CHAR.arrival_date.HH = if_else(ymd(datearrival_shelter) < ymd("2017-08-01"),"Before_August_2017",
                             if_else(ymd(datearrival_shelter) %in% ymd("2017-08-01"):ymd("2018-07-31") ,"aug_17_to_jul_18",
                                     if_else(ymd(datearrival_shelter) %in% ymd("2018-08-01"):ymd("2019-07-31") ,"aug_18_to_jul_19",
                                             if_else(ymd(datearrival_shelter) %in% ymd("2019-08-01"):ymd("2020-02-29") ,"aug_19_to_feb_20",
                                                     if_else(ymd(datearrival_shelter) > ymd("2020-02-29"),"afer_feb_2020","error",NULL)
                                             )))),
    I.HH_CHAR.highest_education.HH= if_else(edu_highest %in% edu_no_formal, "No formal education",
                                 if_else(edu_highest %in% edu_some_primary, "Some primary",
                                         if_else(edu_highest %in% primary_and_above, "Primary and above","",missing = NULL ))),
    language_bangla_english_rs= rowSums(hh_to_hh[,language_bangla_english], na.rm=T),
    I.HH_CHAR.bangla_english.HH= if_else(language_bangla_english_rs>=1 & (language_member.bangla==1 | language_member.english==1), "yes","no"),
    I.EDU.remote_learning_hh.HH= if_else(remote_learning_total< school_children_total, 0, 1),
    I.EDU.remote_learning_all.HH = sum(remote_learning_total,na.rm = T) / sum(school_children_total,na.rm = T)
    
    
    
  )
}
if (population != "refugee") {
  hh_to_hh <- hh_to_hh %>% dplyr::mutate(
    I.HH_CHAR.valid_id_hh= if_else(valid_id == over_18_HH_count,"yes","no"),
    I.HH_CHAR.highest_education.HH= if_else(edu_highest %in% edu_no_formal, "No formal education",
                                 if_else(edu_highest %in% edu_some_primary, "Some primary",
                                         if_else(edu_highest %in% primary_and_above, "Primary and above","",missing = NULL )))
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
  I.HEALTH.ind_need_treatment_f_0_17.INDV= if_else(ind_need_treatment== "yes"& ind_gender== "female"& individual_age<=17, "yes","no"),
  I.HEALTH.ind_need_treatment_m_0_17.INDV= if_else(ind_need_treatment== "yes"& ind_gender== "male"& individual_age<=17, "yes","no"),
  I.HEALTH.ind_need_treatment_f_18_59.INDV=if_else(ind_need_treatment== "yes"& ind_gender== "female"& individual_age %in% 18:59, "yes","no"),
  I.HEALTH.ind_need_treatment_m_18_59.INDV= if_else(ind_need_treatment== "yes"& ind_gender== "male"& individual_age %in% 18:59, "yes","no"),
  I.HEALTH.ind_need_treatment_f_60.INDV=if_else(ind_need_treatment== "yes"& ind_gender== "female"& individual_age>=60, "yes","no"),
  I.HEALTH.ind_need_treatment_m_60.INDV=if_else(ind_need_treatment== "yes"& ind_gender== "male"& individual_age>=60, "yes","no"),
  #i.child_nutrition_screened= if_else(child_nutrition_screened== "yes"& (individual_age %in% 1:4|individual_age_mo>=6), "yes","no"),
  #i.child_nutrition_not_screened= if_else(child_nutrition_screened== "no"& (individual_age %in% 1:4|individual_age_mo>=6), "yes","no"),
  #i.child_nutrition_screened_dont_know= if_else(child_nutrition_screened== "dont_know"& (individual_age %in% 1:4|individual_age_mo>=6), "yes","no"),
  #i.child_enrolment_nfp= if_else(child_enrolment_nfp== "yes"& (individual_age %in% 1:4|individual_age_mo>=6), "yes","no"),
  #i.child_enrolment_nfp_no= if_else(child_enrolment_nfp== "no"& (individual_age %in% 1:4|individual_age_mo>=6), "yes","no"),
  #i.child_enrolment_nfp_dont_know= if_else(child_enrolment_nfp== "dont_know"& (individual_age %in% 1:4|individual_age_mo>=6), "yes","no"),
  I.INDV_CHAR.ind_work_5_17.INDV= if_else(ind_work=="yes"& individual_age %in% 5:17, "yes","no"),
  I.INDV_CHAR.ind_work_18_59.INDV= if_else(ind_work=="yes"& individual_age %in% 18:59, "yes","no"),
  I.INDV_CHAR.ind_work_60.INDV= if_else(ind_work=="yes"& individual_age>=60, "yes","no"),
  I.INDV_CHAR.ind_f_3_5.INDV= if_else(ind_gender== "female"& individual_age %in% 3:5, "yes","no"),
  I.INDV_CHAR.ind_m_3_5.INDV=if_else(ind_gender== "male"& individual_age %in% 3:5, "yes","no"),
  I.INDV_CHAR.ind_f_6_14.INDV=if_else(ind_gender== "female"& individual_age %in% 6:14, "yes","no"),
  I.INDV_CHAR.ind_m_6_14.INDV=if_else(ind_gender== "male"& individual_age %in% 6:14, "yes","no"),
  I.INDV_CHAR.ind_f_15_18.INDV=if_else(ind_gender== "female"& individual_age %in% 15:18, "yes","no"),
  I.INDV_CHAR.ind_m_15_18.INDV=if_else(ind_gender== "male"& individual_age %in% 15:18, "yes","no"),
  I.INDV_CHAR.ind_f_19_24.INDV=if_else(ind_gender== "female"& individual_age %in% 19:24, "yes","no"),
  I.INDV_CHAR.ind_m_19_24.INDV=if_else(ind_gender== "male"& individual_age %in% 19:24, "yes","no"),
  I.INDV_CHAR.ind_f_4.INDV=if_else(ind_gender== "female"& individual_age==4,"yes","no"),
  I.INDV_CHAR.ind_m_4.INDV=if_else(ind_gender== "male"& individual_age==4,"yes","no"),
  I.INDV_CHAR.ind_f_5_11.INDV=if_else(ind_gender== "female"& individual_age %in% 5:11, "yes","no"),
  I.INDV_CHAR.ind_m_5_11.INDV=if_else(ind_gender== "male"& individual_age %in% 5:11, "yes","no"),
  I.INDV_CHAR.ind_f_12_17.INDV=if_else(ind_gender== "female"& individual_age %in% 12:17, "yes","no"),
  I.INDV_CHAR.ind_m_12_17.INDV=if_else(ind_gender== "male"& individual_age %in% 12:17, "yes","no"),
  I.INDV_CHAR.ind_f_18_24.INDV=if_else(ind_gender== "female"& individual_age %in% 18:24, "yes","no"),
  I.INDV_CHAR.ind_m_18_24.INDV=if_else(ind_gender== "male"& individual_age %in% 18:24, "yes","no"),
  I.INDV_CHAR.ind_4_24.INDV=if_else(individual_age %in% 4:24, "yes","no"),
  I.INDV_CHAR.ind_6_18.INDV=if_else(individual_age %in% 6:18, "yes","no"),
  I.INDV_CHAR.ind_5_17.INDV=if_else(individual_age %in% 5:17, "yes","no"),
  I.INDV_CHAR.ind_f_0_17.INDV=if_else(ind_gender== "female"& individual_age<18, "yes","no"),
  I.INDV_CHAR.ind_m_0_17.INDV=if_else(ind_gender== "male"& individual_age<18, "yes","no"),
  I.INDV_CHAR.ind_f_18_59.INDV=if_else(ind_gender== "female"& individual_age %in% 18:59, "yes","no"),
  I.INDV_CHAR.ind_m_18_59.INDV=if_else(ind_gender== "male"& individual_age %in% 18:59, "yes","no"),
  I.INDV_CHAR.ind_f_60.INDV=if_else(ind_gender== "female"& individual_age>=60, "yes","no"),
  I.INDV_CHAR.ind_m_60.INDV=if_else(ind_gender== "male"& individual_age>=60, "yes","no"),
  I.INDV_CHAR.ind_6_59_months.INDV= if_else(individual_age %in% 0:4|(individual_age==0 & individual_age_mo>=6 ),"yes","no"),
  I.INDV_CHAR.ind_18_59.INDV=if_else(individual_age %in% 18:59, "yes","no"),
  I.INDV_CHAR.ind_60.INDV= if_else(individual_age>=60, "yes","no")
) 

if (population == "refugee") {
  indv_to_indv <- indv_to_indv %>% dplyr::mutate(
    I.EDU.ind_ed_TLC_f_3_5.INDV=if_else( ind_ed_TLC == "yes" & individual_age >= 3 & individual_age <= 5 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_TLC_m_3_5.INDV=if_else( ind_ed_TLC == "yes" & individual_age >= 3 & individual_age <= 5 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_TLC_f_6_14.INDV=if_else( ind_ed_TLC == "yes" & individual_age >= 6 & individual_age <= 14 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_TLC_m_6_14.INDV=if_else( ind_ed_TLC == "yes" & individual_age >= 6 & individual_age <= 14 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_TLC_f_15_18.INDV=if_else( ind_ed_TLC == "yes" & individual_age >= 15 & individual_age <= 18 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_TLC_m_15_18.INDV=if_else( ind_ed_TLC == "yes" & individual_age >= 15 & individual_age <= 18 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_TLC_f_19_24.INDV=if_else( ind_ed_TLC == "yes" & individual_age >= 19 & individual_age <= 24 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_TLC_m_19_24.INDV=if_else( ind_ed_TLC == "yes" & individual_age >= 19 & individual_age <= 24 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_madrassa_f_3_5.INDV=if_else( ind_ed_madrassa == "yes" & individual_age >= 3 & individual_age <= 5 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_madrassa_m_3_5.INDV=if_else( ind_ed_madrassa == "yes" & individual_age >= 3 & individual_age <= 5 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_madrassa_f_6_14.INDV=if_else( ind_ed_madrassa == "yes" & individual_age >= 6 & individual_age <= 14 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_madrassa_m_6_14.INDV=if_else( ind_ed_madrassa == "yes" & individual_age >= 6 & individual_age <= 14 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_madrassa_f_15_18.INDV=if_else( ind_ed_madrassa == "yes" & individual_age >= 15 & individual_age <= 18 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_madrassa_m_15_18.INDV=if_else( ind_ed_madrassa == "yes" & individual_age >= 15 & individual_age <= 18 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_madrassa_f_19_24.INDV=if_else( ind_ed_madrassa == "yes" & individual_age >= 19 & individual_age <= 24 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_madrassa_m_19_24.INDV=if_else( ind_ed_madrassa == "yes" & individual_age >= 19 & individual_age <= 24 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_nonformal_f_3_5.INDV=if_else( ind_ed_nonformal == "yes" & individual_age >= 3 & individual_age <= 5 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_nonformal_m_3_5.INDV=if_else( ind_ed_nonformal == "yes" & individual_age >= 3 & individual_age <= 5 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_nonformal_f_6_14.INDV=if_else( ind_ed_nonformal == "yes" & individual_age >= 6 & individual_age <= 14 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_nonformal_m_6_14.INDV=if_else( ind_ed_nonformal == "yes" & individual_age >= 6 & individual_age <= 14 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_nonformal_f_15_18.INDV=if_else( ind_ed_nonformal == "yes" & individual_age >= 15 & individual_age <= 18 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_nonformal_m_15_18.INDV=if_else( ind_ed_nonformal == "yes" & individual_age >= 15 & individual_age <= 18 & ind_gender == "male","yes","no",NULL),
    I.EDU.ind_ed_nonformal_f_19_24.INDV=if_else( ind_ed_nonformal == "yes" & individual_age >= 19 & individual_age <= 24 & ind_gender == "female","yes","no",NULL),
    I.EDU.ind_ed_nonformal_m_19_24.INDV=if_else( ind_ed_nonformal == "yes" & individual_age >= 19 & individual_age <= 24 & ind_gender == "male","yes","no",NULL)
    
  )
}


if (population != "refugee") {
  indv_to_indv <- indv_to_indv %>% dplyr::mutate(
    I.EDU.ind_formal_learning_f_4.INDV=if_else( (ind_formal_learning != "none" & individual_age == 4 & ind_gender == "female"),"yes","no",NULL),
    I.EDU.ind_formal_learning_m_4.INDV=if_else( (ind_formal_learning != "none" & individual_age == 4 & ind_gender == "male"),"yes","no",NULL),
    I.EDU.ind_formal_learning_f_5_11.INDV=if_else( (ind_formal_learning != "none" & individual_age >= 5 & individual_age <= 11 & ind_gender == "female"),"yes","no",NULL),
    I.EDU.ind_formal_learning_m_5_11.INDV=if_else( (ind_formal_learning != "none" & individual_age >= 5 & individual_age <= 11 & ind_gender == "male"),"yes","no",NULL),
    I.EDU.ind_formal_learning_f_12_17.INDV=if_else( (ind_formal_learning != "none" & individual_age >= 12 & individual_age <= 17 & ind_gender == "female"),"yes","no",NULL),
    I.EDU.ind_formal_learning_m_12_17.INDV=if_else( (ind_formal_learning != "none" & individual_age >= 12 & individual_age <= 17 & ind_gender == "male"),"yes","no",NULL),
    I.EDU.ind_formal_learning_f_18_24.INDV=if_else( (ind_formal_learning != "none" & individual_age >= 18 & individual_age <= 24 & ind_gender == "female"),"yes","no",NULL),
    I.EDU.ind_formal_learning_m_18_24.INDV=if_else( (ind_formal_learning != "none" & individual_age >= 18 & individual_age <= 24 & ind_gender == "male"),"yes","no",NULL),
    I.EDU.ind_nonformal_learn_f_4.INDV=if_else( (ind_nonformal_learn != "none" & individual_age == 4 & ind_gender == "female"),"yes","no",NULL),
    I.EDU.ind_nonformal_learn_m_4.INDV=if_else( (ind_nonformal_learn != "none" & individual_age == 4 & ind_gender == "male"),"yes","no",NULL),
    I.EDU.ind_nonformal_learn_f_5_11.INDV=if_else( (ind_nonformal_learn != "none" & individual_age >= 5 & individual_age <= 11 & ind_gender == "female"),"yes","no",NULL),
    I.EDU.ind_nonformal_learn_m_5_11.INDV=if_else( (ind_nonformal_learn != "none" & individual_age >= 5 & individual_age <= 11 & ind_gender == "male"),"yes","no",NULL),
    I.EDU.ind_nonformal_learn_f_12_17.INDV=if_else( (ind_nonformal_learn != "none" & individual_age >= 12 & individual_age <= 17 & ind_gender == "female"),"yes","no",NULL),
    I.EDU.ind_nonformal_learn_m_12_17.INDV=if_else( (ind_nonformal_learn != "none" & individual_age >= 12 & individual_age <= 17 & ind_gender == "male"),"yes","no",NULL),
    I.EDU.ind_nonformal_learn_f_18_24.INDV=if_else( (ind_nonformal_learn != "none" & individual_age >= 18 & individual_age <= 24 & ind_gender == "female"),"yes","no",NULL),
    I.EDU.ind_nonformal_learn_m_18_24.INDV=if_else( (ind_nonformal_learn != "none" & individual_age >= 18 & individual_age <= 24 & ind_gender == "male"),"yes","no",NULL),
    I.EDU.ind_school_dropout.INDV= if_else(ind_formal_learning == "none" & ind_formal_learning_none != "none","yes","no",NULL)
    
  )} 

# household to individual -------------------------------------------------

hh_to_indv1<- indv_to_indv %>% group_by(X_submission__uuid) %>% summarise(
  
  dependents = (length(individual_age[individual_age<15]) + length(individual_age[individual_age>64])),
  non_dependent = (length(individual_age[individual_age %in% 15:64])),
  I.HH_CHAR.dep_ratio.INDVHH= (dependents/non_dependent),
  I.HH_CHAR.no_working_age.INDVHH=if_else(non_dependent== 0, "yes","no"),
  I.HH_CHAR.no_male_working_age.INDVHH= if_else(any(individual_age %in% 15:64 & ind_gender== "male"),"no","yes"),
  I.HH_CHAR.high_dep_ratio.INDVHH=if_else(I.HH_CHAR.dep_ratio.INDVHH>2,"yes","no"),
  I.HH_CHAR.ind_work_5_17_hh.INDVHH = if_else(sum(I.INDV_CHAR.ind_work_5_17.INDV == "yes",na.rm = T) >0,"yes","no",NULL),
  I.HH_CHAR.ind_6_59_months_hh.INDVHH=if_else(sum(I.INDV_CHAR.ind_6_59_months.INDV == "yes",na.rm = T) >0,"yes","no",NULL),
  I.HH_CHAR.ind_5_17_hh.INDVHH=if_else(sum(I.INDV_CHAR.ind_5_17.INDV == "yes",na.rm = T) >0,"yes","no",NULL),
  I.HEALTH.ind_need_treatment_hh.INDVHH=if_else(sum(ind_need_treatment== "yes",na.rm = T) >0,"yes","no",NULL)
)

if (population== "refugee"){
  hh_to_indv2<- indv_to_indv %>% group_by(X_submission__uuid) %>% summarise(
    I.EDU.ind_out_of_school.INDVHH= if_else(sum(ind_ed_TLC=="no" & individual_age %in% 6:18,na.rm = T )>0, "yes","no",NULL)
  )
}


if (population != "refugee"){
  hh_to_indv2<- indv_to_indv %>% dplyr::group_by(X_submission__uuid) %>% dplyr::summarise(
    I.EDU.ind_out_of_school.INDVHH= if_else(sum(ind_formal_learning == "none" & individual_age %in% 5:17,na.rm = T)>0, "yes","no",NULL)
  )
}

hh_to_indv <- hh_to_indv1 %>% left_join(hh_to_indv2)


# compile and write dataset ---------------------------------------------------------

compile_dataset <- hh_to_hh %>% left_join(hh_to_indv,by =c ("X_uuid"="X_submission__uuid"))


if (write_output == "yes") {
  write.csv(compile_dataset,paste0("outputs/",population,"/composite_indicator/",str_replace_all(day_to_run,"-","_"),"_composite_indicator.csv"))
  write.csv(compile_dataset,paste0("outputs/",population,"/composite_indicator/composite_indicator.csv"))
  
  write.csv(indv_to_indv,paste0("outputs/",population,"/composite_indicator/",str_replace_all(day_to_run,"-","_"),"_INDV_composite_indicator.csv"))
  write.csv(indv_to_indv,paste0("outputs/",population,"/composite_indicator/INDV_composite_indicator.csv"))
}
