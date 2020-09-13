library(butteR)
library(tidyverse)
library(googlesheets4)
library(srvyr)
library(lubridate)
library(extrafont)
population<- c("host","refugee")[1]
day_to_run <- Sys.Date()
write_output<-T
source("scripts/active_path.R")
source("functions/mean_prop2020.R")
source("functions/reach_style.R")
if(population=="refugee"){
  svy_strata= "camp_name"
}
if(population=="host"){
  svy_strata= "upazilla_name"
}


# Read in data ------------------------------------------------------------

# tool
ks<- readxl::read_xlsx(tool_path %>% str_replace(".xls",".xlsx"),"survey")
kc<-readxl::read_xlsx(tool_path %>%  str_replace(".xls",".xlsx"),"choices")
xls_lt<-make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::english")

# 2019 hh and individual
hh2019<-read.csv(paste0("inputs/",population,"/significance_testing/2019_hh_",population,"_with_composites.csv"), na.strings =c(""," ",NA,"NA"), stringsAsFactors=T)
indiv2019<-read.csv(paste0("inputs/",population,"/significance_testing/2019_indiv_",population,"_with_composites.csv"), na.strings =c(""," ",NA,"NA"), stringsAsFactors=T)

# 2020 hh and individual
hh2020<-read.csv(paste0("inputs/",population,"/significance_testing/2020_hh_",population,"_with_composites.csv"), na.strings =c(""," ",NA,"NA"), stringsAsFactors=T)
indiv2020<-read.csv(paste0("inputs/",population,"/significance_testing/2020_indiv_",population,"_with_composites.csv"), na.strings =c(""," ",NA,"NA"), stringsAsFactors=T)

# get rid of don't know for analysis
hh2020<-hh2020 %>% 
  mutate(
    I.HH_CHAR.education_level_similar_as_2019.HH= na_if(I.HH_CHAR.education_level_similar_as_2019.HH,"dont_know")
    )


# recode date arrival classes from 2019 msna to make it more comparable
if(population=="refugee"){
hh2019<-hh2019 %>%
  mutate( 
    datearrival_shelter=as_date(datearrival_shelter),
    I.HH_CHAR.datearrival_shelter.HH=case_when(
      datearrival_shelter<as_date("2017-08-01")  ~"Before_August_2017",
      datearrival_shelter<as_date("2018-08-01")  ~"aug_17_to_jul_18",
      datearrival_shelter<as_date("2019-08-01")  ~"aug_18_to_jul_19",
      TRUE  ~"aug_19_to_present"
      
    ) %>% as.factor) 

}


# Set up survey objects ---------------------------------------------------

# survey objects for 2019
hh2019_svy<- as_survey(.data= hh2019,strata=svy_strata, weights=weights)                 

# these if statements are annoying just because in the different data sets the strata was not included in either 2019/2020
if(population=="host"){
indiv2019<-indiv2019 %>% left_join(hh2019 %>% select(X_uuid,svy_strata), by=c("X_submission__uuid"="X_uuid" ))}

indiv2019_svy<- as_survey(.data= indiv2019,strata=svy_strata, weights=weights)                 

# survey objects for 2020
hh2020_svy<- as_survey(.data= hh2020,strata= svy_strata,weights=survey_weight)                 

if(population=="refugee"){
indiv2020<-indiv2020 %>% left_join(hh2020 %>% select(X_uuid,svy_strata), by=c("X_submission__uuid"="X_uuid" ))
}
indiv2020_svy<- as_survey(.data= indiv2020,strata= svy_strata,weights=survey_weight)                 
# indiv2020$survey_weight




# Set up comparison lookup table ------------------------------------------

# these lookup tables were meant to compile comparable indicators filled by AO
if(population=="refugee"){
comparison_lt<-readr::read_csv("inputs/refugee/significance_testing/dataset_compare_refugee_mh_ck_za.csv")}
if(population=="host"){
comparison_lt<- readxl::read_xlsx("inputs/host/significance_testing/dataset_compare_host_ck.xlsx")
}

# most of the commented out stuff in this section was me iterating through mistakes in the lookup tables

# sm_remove<-paste0(butteR::auto_detect_select_multiple(hh2020),".")
# sm_remove_helper<-paste0("^(", paste(sm_remove, collapse="|"), ")")

# lookup for individual analysis
comparison_lt_hh<-comparison_lt %>%
  filter(!is.na(column_name_2019)) %>% 
  filter(comparable!=0,!is.na(comparable)) %>%
  mutate(Group= toupper(Group),
         population= str_to_title(population)) %>% 
  # filter(!is.na(column_name_2019)) %>%
  filter(population==stringr::str_to_title(population),Group=="HH") %>% 
  # filter(!str_detect(column_name_2020,"\\.")) %>%
  # filter(!str_detect(column_name_2020,sm_remove_helper)) %>%
  # filter(!str_detect(column_name_2020,"_other$")) %>% 
  filter(!column_name_2020%in%c("X_uuid","X","X_index"))

# lookup for hh analysis
comparison_lt_indiv<-comparison_lt %>%
  filter(!is.na(column_name_2019)) %>% 
  filter(comparable!=0,!is.na(comparable)) %>%
  mutate(Group= ifelse(Group %in% c('indv',"INDV"),"INDIV",toupper(Group)),
         population= str_to_title(population)) %>% 
  filter(population==stringr::str_to_title(population),Group=="INDIV") %>% 
  filter(!column_name_2020%in%c("X_uuid","X","X_index"))


# lots of checks this was a bit painful

# comparison_lt_hh %>%filter(str_detect(column_name_2020,"edu"))
# comparison_lt_hh %>%filter(column_name_2020=="I.HH_CHAR.education_level_similar_as_2019.HH")
# comparison_lt_hh %>%filter(column_name_2020=="health_coping")
# hh2020_svy %>% select(comparison_lt_hh$column_name_2020)
# hh2019_svy %>% select(comparisonc_lt_hh$column_name_2019)
# comparison_lt_hh %>% View()

all(comparison_lt_hh$column_name_2019 %in% colnames(hh2019)) %>% length()
all(comparison_lt_indiv$column_name_2019 %in% colnames(hh2019)) %>% length()
all(comparison_lt_indiv$column_name_2020 %in% colnames(hh2020)) %>% length()
comparison_lt_hh$column_name_2019[!comparison_lt_hh$column_name_2019 %in% colnames(hh2019)]
comparison_lt_indiv$column_name_2019[!comparison_lt_indiv$column_name_2019 %in% colnames(indiv2019)]
comparison_lt_indiv$column_name_2020[!comparison_lt_indiv$column_name_2020 %in% colnames(indiv2020)]

# comparison_lt_hh$column_name_2020[!comparison_lt_hh$column_name_2020 %in% colnames(hh2020)]
# comparison_lt_hh$column_name_2019 %>% length()
comparison_lt_hh$column_name_2020[comparison_lt_hh$column_name_2020 %>% duplicated()]
comparison_lt_indiv$column_name_2019[comparison_lt_indiv$column_name_2019 %>% duplicated()]


#  remove values in lookup table that are all NA in either 2020 or 2019 data sset
if(population=="refugee"){
comparison_lt_hh<-comparison_lt_hh %>% 
  filter(! column_name_2019 %in% c("hh_priority_needs_other","gbv_support_other"),!column_name_2020%in%"language_member_other")
comparison_lt_indiv<-comparison_lt_indiv %>% 
  filter(! column_name_2020%in%c("treatment_location_other","ind_why_notreatment_other"))
}


# Harmonize comparable indicators across data sets ------------------------

# give all comparable indicators the 2020 name
hh2019_svy<- hh2019_svy %>% 
  select(comparison_lt_hh$column_name_2019) %>% 
  rename_at(.vars = comparison_lt_hh$column_name_2019,
            .funs = function(x){x<-comparison_lt_hh$column_name_2020})


indiv2019_svy<-indiv2019_svy %>% 
  select(comparison_lt_indiv$column_name_2019) %>% 
  rename_at(.vars = comparison_lt_indiv$column_name_2019,
            .funs = function(x){x<-comparison_lt_indiv$column_name_2020})

# make sure they are factors ( i think they already are)
hh2019_svy2<-hh2019_svy %>% 
  mutate_if(.predicate = is.character,.funs = ~as.factor)
indiv2019_svy2<-indiv2019_svy %>% 
  mutate_if(.predicate = is.character,.funs = ~as.factor)
            
# this was the check i did to help remove all NA values from lookup above -- if they all come back empty.. good to go


hh2019_svy2$variables %>% 
  select(comparison_lt_hh$column_name_2020) %>%
  select_if(is.factor) %>%
  map(~length(levels(.x))) %>% 
  keep(.==1) 

indiv2020_svy$variables %>% 
  select(comparison_lt_indiv$column_name_2020) %>%
  select_if(is.factor) %>%
  map(~length(levels(.x))) %>% 
  keep(.==1) 
indiv2019_svy$variables %>% 
  select(comparison_lt_indiv$column_name_2020) %>%
  select_if(is.factor) %>%
  map(~length(levels(.x))) %>% 
  keep(.==1) 

hh2020_svy$variables %>% 
  select(comparison_lt_hh$column_name_2020) %>%
  select_if(is.factor) %>%
  map(~length(levels(.x))) %>% 
  keep(.==1)



# we want out of whole population NA means no debt
debt_qs2019<- hh2019_svy2 %>% select(starts_with("debt_reason.")) %>% colnames()
debt_qs2020<- hh2020_svy %>% select(starts_with("debt_reason.")) %>% colnames()

hh2019_svy2<-hh2019_svy2 %>% 
  mutate_at(debt_qs2019,~ifelse(is.na(.),0,.))
hh2020_svy<-hh2020_svy %>% 
  mutate_at(debt_qs2020,~ifelse(is.na(.),0,.))


mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = debt_qs2020) %>% data.frame()
# analyze 2019
res_2019<-mean_prop2020(df = hh2019_svy2,aggregate_by = NULL,variables_to_analyze = comparison_lt_hh$column_name_2020)
indiv_res_2019<-mean_prop2020(df = indiv2019_svy2,aggregate_by = NULL,variables_to_analyze = comparison_lt_indiv$column_name_2020)

# analyze 2020
res_2020<-mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = comparison_lt_hh$column_name_2020)
indiv_res_2020<-mean_prop2020(df = indiv2020_svy,aggregate_by = NULL,variables_to_analyze = comparison_lt_indiv$column_name_2020)

# add columns and bind all together
indiv_res_2019<-indiv_res_2019 %>% 
  bind_rows %>% 
  mutate(assessment="msna_2019",
         group="indiv")
indiv_res_2020<-indiv_res_2020 %>% 
  bind_rows %>% 
  mutate(assessment="msna_2020",
         group="indiv"
         )

res_2019<-res_2019 %>% bind_rows() %>% 
  mutate(assessment="msna_2019",
         group="hh")
res_2020<-res_2020 %>% bind_rows() %>% 
  mutate(assessment="msna_2020",
         group="hh")



res_total<-bind_rows(res_2019,indiv_res_2019,res_2020,indiv_res_2020)
# remove this shouldnt have been in lookup table (its not the correct composite)
res_total<-res_total %>% filter(indicator!="datearrival_shelter") 
colnames(res_total)

# trying to fix issues where the indicators are of different type - select one vs select multiple 
# and only produce comparable sets of results -- it works pretty well, but if anything is missing i can provide res_total
# so far nothing missing
res_comparable<-res_total %>% 
  arrange(indicator) %>% #here we see a probelm with a few sm to so comparisons
  group_by(indicator) %>% 
  mutate(sm_v_so= "1" %in% option & "yes"%in% option & "no" %in% option,
         option_2= ifelse(sm_v_so, str_replace_all(option,"1","yes"),option)
         ) %>% 
  group_by(indicator, option_2) %>%
  arrange(indicator,option_2) %>% 
  mutate(num_group=n()) %>% 
  filter(num_group>1)


# Analysis done, now just adding some details to make output more  --------



# this was a half filled out lookup table to match indicators  and analysis to sector and descriptions
# i can use it here to help add sectors for later 
comparison_names<-read_sheet(ss = "1src6hpfU6h6WH97wATEP_4hmW_vHpgt4RY-Ps9zqCrs",
                             sheet="merged_lookup",guess_max = 449)

res_comparable<-res_comparable %>% 
  mutate(analysis_code=paste0(indicator,".",option),
         analysis_code2=paste0(indicator,".", option_2)
         )

# this join works pretty well, but i fil in more details below... this all doesnt matter from analysis perspective
res_comparable2<-res_comparable %>% 
  left_join(comparison_names %>% select(analysis_code,sector),by="analysis_code") 
res_comparable3<-res_comparable2 %>% 
  group_by(indicator,option_2) %>% 
  mutate(sector_2= sector[!is.na(sector)][1L],
         sector_2=ifelse(is.na(sector_2),"no_sector_assigned",sector_2))



res_comparable4<-res_comparable3 %>% 
  mutate(sector_3=
           case_when(
             sector_2=="no_sector_assigned"& str_detect(indicator,"shelter|SNFI|cooking")~"Shelter/NFI",
             sector_2=="no_sector_assigned"& str_detect(indicator,"income|FSL|food_source|debt|sell|expense")~"Food security/Livelihoods/Markets/Cash",
             sector_2=="no_sector_assigned"& str_detect(indicator,"HH_CHAR|edu_highest")~"Household characteristics",
             sector_2=="no_sector_assigned"& str_detect(indicator,"soap")~"WASH",
             TRUE~ sector_2
            
    
  )
  )


# These results to be used in analysis tables -----------------------------


if(population=="refugee" &write_output){
write_csv(res_comparable4, "outputs/refugee/refugee_2020_2019_semi_comparable_20200910.csv")
  }
if(population=="host" & write_output){
write_csv(res_comparable4, paste0("outputs/",population,"/",population,"_2020_2019_semi_comparable_20200910.csv"))
  }
  











res_comparable_factors<-res_comparable4 %>% 
  ungroup() %>% 
  filter(mean<=1)
res_comparable_numerics<-res_comparable4 %>% 
  ungroup() %>% 
  filter(mean>1)


# res_comparable_factors %>% 
  # filter(!str_detect(indicator,"^rank_priority")) %>% 
  # filter(indicator=="I.HH_CHAR.education_level_similar_as_2019.HH")
# I.HH_CHAR.education_level_similar_as_2019.HH

rgb(238,88,89,maxColorValue = 255)
rgb(8,88,89,maxColorValue = 255)
  # rgb(88,88,90,maxColorValue = 255)
res_comparable_split<-res_comparable_factors %>% 
  split(.$sector_3) #%>% 

# ceiling(0.34,digits=2)

res_comparable_factors<-res_comparable_factors %>% 
  mutate(analysis_lab=
           case_when(
             str_detect(analysis_code2,"I.HH_CHAR.education_level_similar_as_2019")~
               str_replace_all(analysis_code2,"I.HH_CHAR.education_level_similar_as_2019.",""),
             str_detect(analysis_code2,"I.HH_CHAR")~
               str_replace_all(analysis_code2,"I.HH_CHAR.",""),
             str_detect(analysis_code2,"I.HEALTH")~
               str_replace_all(analysis_code2,"I.HEALTH.",""),
             str_detect(analysis_code2,"hh_priority_needs.access_to_income_")~
               str_replace_all(analysis_code2,"hh_priority_needs.access_to_income_",""),
             TRUE~ analysis_code2
                     )
         )


res_comparable4<- res_comparable4 %>% 
  mutate(sector_3=janitor::make_clean_names(sector_3).
         arg_nudge_xif())
make_comparison_plots<-function(df=res_comparable_factors){
  
  
}
res_comparable_factors$sector_3 %>% unique()
nudge_args<-rev(c(0.8,0.4,0.6,0.5,0.5,0.7,0.2))

boom<-res_comparable_factors %>% 
  split(.$sector_3) %>%
  map( ~ggplot(.x,aes(x=analysis_lab,y=mean, color=assessment))+
        geom_point()+geom_errorbar(aes(ymin=mean_low, ymax=mean_upp))+ 
         scale_y_continuous(breaks = seq(0,1,0.1),labels=  scales::percent)+
         # geom_text(aes(label=round(mean*100,0)),nudge_x = .y,hjust= "inward",vjust="inward" ,size=2)+
         # ggrepel::geom_text_repel(aes(label=round(mean*100,0)),nudge_x = .y,hjust= "inward",vjust="inward" ,size=2)+
         coord_flip()+
         scale_color_manual(values = c("#EE5859","#085859"))+
  
         reach_style()

  )


names(boom)<-janitor::make_clean_names(names(boom))
boom$food_security_livelihoods_markets_cash
# ggsave(filename = "cwc")
walk2(paste0(names(boom),".pdf"),boom,~ggsave(filename =paste0("outputs/host/", .x),plot = .y,height = 5,width=8,device = "pdf"))

res_comparable_factors %>% 
  filter(!str_detect(indicator,"^rank_priority")) %>% 
  ggplot(aes(x=analysis_code2,y=mean, color=asessment))+
  geom_point()+geom_errorbar(aes(ymin=mean_low, ymax=mean_upp))+ 
  geom_text(aes(label=round(mean*100,0)),nudge_x = 0.4)+
  coord_flip()+
  theme(
    axis.text.x = element_text(angle=90,size = 8,hjust = 1),
    axis.text.y = element_text(size = 8,hjust = 0))
)

# hh2020 %>% select(datearrival_shelter)
# hh2019 %>% select(datearrival_shelter)



res_comparable_split$`Food security/Livelihoods/Markets/Cash` %>% 
  ggplot(aes(x=analysis_code2,y=mean, color=asessment))+
  geom_point()+geom_errorbar(aes(ymin=mean_low, ymax=mean_upp))+ coord_flip()+
  theme(
    axis.text.x = element_text(angle=90,size = 8,hjust = 1),
    axis.text.y = element_text(size = 8,hjust = 0))

res_comparable_numerics %>% print(n=nrow(.))
  


res_numerics<-list(res_2019$numerics %>% mutate(assessment="msna_2019"),res_2020$numerics %>% mutate(assessment="msna_2020"))
res_num_df<-bind_rows(res_numerics)

res_num_df %>% 
  filter(mean<=1) %>% 
  filter(!str_detect(indicator,"^hh_priority")) %>% 
  ggplot(aes(x=indicator,y=mean, color=assessment))+
  geom_point()+geom_errorbar(aes(ymin=ci_low, ymax=ci_upp))+ coord_flip()+
  theme(
    axis.text.x = element_text(angle=90,size = 8,hjust = 1),
    axis.text.y = element_text(size = 8,hjust = 0)
      
    
  ) 

# ggsave(filename = "all_numeric_percents.pdf",device = "pdf",width = 1189, height =841  ,units = "mm")

# THAT IS DECENT - WHAT IF WE  RESET ALL COL NAMES
comparison_lt<-readxl::read_xlsx("inputs/refugee/significance_testing/dataset_compare_refugee.xlsx")
comparison_lt_hh<-comparison_lt %>%
  filter(!is.na(column_name_2019)) %>% 
  filter(population==stringr::str_to_title(population),Group=="HH") %>% 
  # filter(!str_detect(column_name_2020,"\\.")) %>% 
  filter(!str_detect(column_name_2020,"_other$")) %>% 
  filter(!column_name_2020%in%c("X_uuid","X","X_index"))

comparison_lt_hh<-comparison_lt_hh %>% 
  slice(5:nrow(.))


hh2019<-read.csv(paste0("inputs/",population,"/significance_testing/2019_hh_",population,"_with_composites.csv"), na.strings =c(""," ",NA,"NA"), stringsAsFactors=T)


hh2020<-read.csv(paste0("inputs/",population,"/significance_testing/2020_hh_refugee_with_composites.csv"), na.strings =c(""," ",NA,"NA"), stringsAsFactors=T)
ind2020<-read.csv(paste0("inputs/",population,"/significance_testing/2020_indiv_refugee_with_composites.csv"), na.strings =c(""," ",NA,"NA"), stringsAsFactors=T)
ind2020<-ind2020 %>% 
  left_join(hh2020 %>% 
              select(X_uuid,camp_name_fix), by=c("X_submission__uuid"="X_uuid"))

ggsave(filename = "all_numeric_percents2.pdf",device = "pdf",width = 1189, height =841  ,units = "mm")

hh2019$gbv_support.legal_aid
hh2019<- hh2019%>% 
  rename_at(.vars = comparison_lt_hh$column_name_2019,
            .funs = function(x){x<-comparison_lt_hh$column_name_2020})

hh2019
hh2019_svy<- as_survey(.data= hh2019,strata= camp_name, weights=weights)                 
hh2020_svy<- as_survey(.data= hh2020,strata=camp_name_fix,weights=survey_weight)                 
ind2020_svy<-as_survey(.data= ind2020,strata=camp_name_fix,weights=survey_weight)                 

hh2019_svy2<-hh2019_svy %>% 
  mutate_if(.predicate = is.character,.funs = ~as.factor)

res_2019<-mean_prop2020(df = hh2019_svy,aggregate_by = NULL,variables_to_analyze = comparison_lt_hh$column_name_2020)
res_2020<-mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = comparison_lt_hh$column_name_2020)
res_numerics<-list(res_2019$numerics %>% mutate(assessment="msna_2019"),res_2020$numerics %>% mutate(assessment="msna_2020"))
res_facs<-list(res_2019$factors %>% mutate(assessment="msna_2019"),res_2020$factors %>% mutate(assessment="msna_2020"))
res_num_df<-bind_rows(res_numerics)
res_facs_df<-bind_rows(res_facs)
res_facs_df %>% 
  filter(indicator!= "datearrival_shelter") %>% 
  group_by(indicator,option) %>% 
  arrange(indicator,option)

hh2019 %>% count(I.HH_CHAR.gender_hoh.HH) %>% 
  mutate(n/sum(n))
hh2020 %>% count(resp_gender)
hh2020 %>% count(I.HH_CHAR.gender_hoh.HH) %>% mutate(n/sum(n))

hh2020 %>% summarise(mean(I.FSL.fcs.HH,na.rm=T))
hh2019 %>% summarise(mean(I.FSL.fcs.HH,na.rm=T))


res_num_df %>% 
  filter(mean<=1) %>% 
  filter(!str_detect(indicator,"^hh_priority")) %>% 
  ggplot(aes(x=indicator,y=mean, color=assessment))+
  geom_point()+geom_errorbar(aes(ymin=ci_low, ymax=ci_upp))+
  geom_text(
    aes(label=round(mean*100,1)
        ))+coord_flip()+
  theme(
    axis.text.x = element_text(angle=90,size = 8,hjust = 1),
    axis.text.y = element_text(size = 8,hjust = 0)
  ) 
ggsave(filename = "all_numeric_percents3.pdf",device = "pdf",width = 1189, height =841  ,units = "mm")





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

dont_analyze_in_data<-dont_analyze[dont_analyze %in% colnames(hh2019)]
is_not_empty<-function(x){ all(is.na(x))==FALSE}


cols_to_analyze<-hh2020 %>% select(-starts_with("Other"), -ends_with("_other")) %>%
  select_if(.,is_not_empty) %>% select(-dont_analyze_in_data) %>% colnames() 

yes_other_cols <- df %>%  select(ends_with(".yes_other")) %>% colnames() %>% dput()

cols_to_analyze <- c(cols_to_analyze,"masks_source.mask_source_other",yes_other_cols,"other_reasons.yes_covid","other_reasons.no") 

if(population == "host"){
  dfsvy$variables$I.FSL.food_source_assistance.HH<- forcats::fct_expand(dfsvy$variables$I.FSL.food_source_assistance.HH,c( "no", "yes"))
  dfsvy$variables$I.HH_CHAR.no_working_age.INDVHH<- forcats::fct_expand(dfsvy$variables$I.HH_CHAR.no_working_age.INDVHH,c( "no", "yes"))
}
comparison_names<-read_sheet(ss = "1src6hpfU6h6WH97wATEP_4hmW_vHpgt4RY-Ps9zqCrs",
                             sheet="msna_2019_2020_lookup",guess_max = 449) 


hh2019$I.FSL.livelihoods_atleast_one.INDVHH
mod_fcs_fsl<-svyglm(formula = I.FCS_score~ I.FSL.livelihoods_atleast_one_adult.INDVHH,
                    hh2019_svy)
mod_fcs_fsl<-svyglm(formula = I.FCS_score~ I.FSL.livelihoods_atleast_one.INDVHH,
                    hh2019_svy)
sjPlot::tab_model(mod_fcs_fsl, auto.label = FALSE,title = "Table 1. Kitchen Sink Model Summary")
library(effects)
plot(allEffects(mod_fcs_fsl))




comparison_names<-comparison_names %>% 
  filter(`population (host vs refugee)` %in% c(population,"Both"))

questions_to_analyze_2020<-comparison_names %>% 
  filter(is.na(issue)) %>% 
  filter(!is.na(msna_2020_indicator_code),!is.na(msna_2019_indicator_code)) %>% 
  mutate(
    question_names_2020= ifelse(str_detect(msna_2020_indicator_code,"\\."),
                                sub('.[^.]*$', '',msna_2020_indicator_code),
                                msna_2020_indicator_code),
    question_names_2019= ifelse(str_detect(msna_2019_indicator_code,"\\."),
                                sub('.[^.]*$', '',msna_2019_indicator_code),msna_2019_indicator_code)
  ) %>% select(msna_2019_indicator_code,msna_2020_indicator_code,question_names_2019,question_names_2020) #%>% 


auto_detect_select_multiple(hh2019_svy$variables)

# questions_to_analyze_2020 %>% filter(question_names_2019!=question_names_2020) %>% write.csv("ref_msna_2019_2020_comparisons_issues.csv")
#this isnt working for water sources
sm<- xls_lt %>% filter(str_detect(question_type,"^select_multiple")) %>% pull(question_name) %>% unique()
so<- xls_lt %>% filter(str_detect(question_type,"^select_one")) %>% pull(question_name) %>% unique()

#these will potentially be the funny ones!
questions_to_analyze_2020 %>% filter(question_names_2019!=question_names_2020) %>% 
  mutate(sm_2020= case_when(question_names_2020 %in%  sm~"sm",
                            question_names_2020 %in% so~ "so",
                            TRUE ~ "composite")) %>% 
  print(n=nrow(.))


hh_qs_2020<-questions_to_analyze_2020$question_names_2020[questions_to_analyze_2020$question_names_2020%in%colnames(hh2020_svy$variables)] %>% unique()
hh_qs_2020_sm<-questions_to_analyze_2020$msna_2020_indicator_code[questions_to_analyze_2020$msna_2020_indicator_code%in%colnames(hh2020_svy$variables)] %>% unique()
# hh_qs_2020_full<-data.frame(indicator=c(hh_qs_2020,hh_qs_2020_sm))

questions_to_analyze_2020<-questions_to_analyze_2020 %>% 
  filter(question_names_2020%in% hh_qs_2020|msna_2020_indicator_code%in% hh_qs_2020_sm) %>% 
  mutate(msna_2020_indicator_code2=ifelse(msna_2020_indicator_code %in%hh_qs_2020_sm,msna_2020_indicator_code,question_names_2020)) #%>% print(n=100)

hh_qs_2019<-questions_to_analyze_2020$question_names_2019[questions_to_analyze_2020$question_names_2019%in%colnames(hh2019_svy$variables)] %>% unique()
#SINCE SM COLS IN ANALYSIS ARE SAME AS SM COLS IN DATASET WE CAN SELECT THEM LIKE THIS
hh_qs_2019_sm<-questions_to_analyze_2020$msna_2019_indicator_code[questions_to_analyze_2020$msna_2019_indicator_code%in%colnames(hh2019_svy$variables)] %>% unique()
# hh_qs_2019_full<-data.frame(indicator=c(hh_qs_2019,hh_qs_2019_sm))

questions_to_analyze_2020<-questions_to_analyze_2020 %>% 
  mutate(msna_2019_indicator_code2=ifelse(msna_2019_indicator_code %in%hh_qs_2019_sm,msna_2019_indicator_code,question_names_2019)) 

hh2019_svy$variables$food_source
comparables<-questions_to_analyze_2020 %>% select(msna_2019_indicator_code2,msna_2020_indicator_code2) %>% unique()
comparables %>% print(n=nrow(.))

# hh_qs_2020<-questions_to_analyze_2020$msna_2020_indicator_code[questions_to_analyze_2020$msna_2020_indicator_code%in%colnames(hh2020_svy$variables)] %>% unique()

# debugonce(mean_prop2020)
comparables2<-comparables %>% filter(msna_2019_indicator_code2!="hh_coping_mechanism")
msna_2020_res<-mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = comparables$msna_2020_indicator_code2)
comparables2$msna_2019_indicator_code2[duplicated(comparables2$msna_2019_indicator_code2)]

comparables3<-comparables2 %>% filter(msna_2019_indicator_code2!="modality_shelter")

hh2019_svy_renamed<-hh2019_svy %>% rename_at(.vars = comparables3$msna_2019_indicator_code2,.funs = function(x){x<-comparables3$msna_2020_indicator_code2})

msna_2019_res<-mean_prop2020(df = hh2019_svy_renamed,aggregate_by = NULL,variables_to_analyze = comparables3$msna_2020_indicator_code2)

assessment_numeric<- bind_rows(msna_2019_res$numerics %>% mutate(assessment="2019"),
                               msna_2020_res$numerics %>% mutate(assessment="2020"))
assessment_numeric_cleaned1<- assessment_numeric %>% 
  rename(option="indicator") %>% 
  mutate(indicator=sub("\\..*", "", option)) %>% 
  select(indicator,everything())


assessment_factors<-bind_rows(msna_2019_res$factors %>% mutate(assessment="2019"),msna_2020_res$factors %>% mutate(assessment="2020"))
numerics_and_factors_combined<-bind_rows(assessment_factors,assessment_numeric_cleaned1)

numerics_and_factors_combined %>% print(n=nrow(.))
comparable_analyzed<-assessment_factors %>% 
  group_by(indicator) %>% 
  filter(sum(assessment=="2019")>1 &sum(assessment=="2020")>1) 

numerics_and_factors_comparable<-numerics_and_factors_combined %>% 
  group_by(indicator) %>% 
  filter(sum(assessment=="2019")>1 &sum(assessment=="2020")>1) 

indicators_to_map1<-comparable_analyzed$indicator %>% unique()
indicators_to_map2<-numerics_and_factors_comparable$indicator %>% unique()

comparable_analyzed %>% 
  filter(indicator=="hoh_marital")

pd <- position_dodge(0.8)

numeric_graphs1<-map(indicators_to_map, function(x) comparable_analyzed %>% 
                       filter( indicator==x) %>%
                       ggplot(aes(x=option,y=mean, fill=assessment))+
                       scale_y_continuous(breaks = seq(0,1, by=0.1),labels = scales::percent) +
                       geom_bar(stat="identity", position=pd)+
                       geom_errorbar(aes(ymin=ci_low, ymax=ci_upp), width=.1,position = pd)#+
                     # scale_y_continuous(labels= scales::percent())  
) %>% setNames(indicators_to_map)

all_graphs1<-map(indicators_to_map2, function(x) numerics_and_factors_comparable %>% 
                   filter( indicator==x) %>%
                   ggplot(aes(x=sub(".*?\\.", '', option),y=mean, fill=assessment))+
                   scale_y_continuous(breaks = seq(0,1, by=0.1),labels = scales::percent) +
                   geom_bar(stat="identity", position=pd)+
                   geom_errorbar(aes(ymin=ci_low, ymax=ci_upp), width=.1,position = pd)+ coord_flip()+
                   labs(x=x, y= "mean %")+
                   reach_style()+
                   theme(
                     axis.text.x = element_text(angle = 45)
                   )
) %>% setNames(indicators_to_map)



hh2020_svy$variables %>% 
  summarise(across(is.factor,cols_to_analyze,~length(levels(.))))

hh2020_svy$variables$shelter_issues
full2020<-mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = cols_to_analyze)

debugonce(mean_prop2020)
mean_prop2020(df = hh2020_svy,aggregate_by = "hoh_marital",variables_to_analyze = "shelter_issues")
mean_prop2020(df = hh2020_svy,aggregate_by = "hoh_marital",variables_to_analyze = "shelter_issues")


hh2019_svy2$variables$missing_child
hh20
mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = "I.HH_CHAR.gender_hoh.HH")
mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = "I.HH_CHAR.gender_hoh.HH")
mean_prop2020(df = hh2019_svy2$variables,aggregate_by = NULL,variables_to_analyze = "missing_child")

ind2020$ind_smoke
mean_prop2020(df = hh2019_svy2,aggregate_by = NULL,variables_to_analyze = "missing_child")
mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = "children_missing")

hh2020$I.HH_CHAR.gender_hoh.HH
mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = "children_missing")
mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = "I.HH_CHAR.gender_hoh.HH")



mean_prop2020(df = hh2020_svy,aggregate_by = "hoh_marital",variables_to_analyze = 'I.HH_CHAR.gender_hoh.HH')

hh2020$hoh_marital
mean_prop2020(df = ind2020_svy,aggregate_by=NULL,variables_to_analyze = "ind_smoke")
mean_prop2020(df = ind2020_svy %>% filter(individual_age>18),aggregate_by=NULL,variables_to_analyze = "ind_smoke")
mean_prop2020(df = ind2020_svy,aggregate_by = 'ind_smoke',variables_to_analyze = "ind_gender")
mean_prop2020(df = ind2020_svy %>% filter(individual_age>18),aggregate_by = 'ind_smoke',variables_to_analyze = "ind_gender")

ind2020 %>% filter(individual_age<5) %>% select(individual_age,)

library(FactoMineR)
library(factoextra)
hh2020 %>% dplyr::select(where(starts_with("I.")))
hh2020 %>% dplyr::select(starts_with("I.")) %>% select(!is.numeric) %>% colnames() %>% dput()
hh2020_comps<-hh2020 %>% dplyr::select(starts_with("I.SN")) %>% select(!is.numeric) 

comps_fac<-c("I.HH_CHAR.gender_hoh.HH", "I.HH_CHAR.education_level2.HH", 
   "I.HH_CHAR.elderly_hoh.HH", "I.HH_CHAR.large_hh.HH", 
  "I.HEALTH.disabled_hh.HH", "I.FSL.no_income.HH", "I.FSL.remittances.HH", 
  "I.SNFI.atlst_one_shelter_improvements_reason.HH", "I.SNFI.improvements_despite_reporting_issue.HH", 
  "I.SNFI.lpg_only.HH", "I.EDU.not_send_back_to_school_hh.HH", 
  "I.FSL.allst_one_market_problme", "I.HEALTH.health_dist_more_60.HH", 
  "I.HEALTH.plw_enrolment_nfp.HH", "I.FSL.fcs_acceptable.HH", "I.FSL.fcs_borderline.HH", 
  "I.FSL.fcs_poor.HH", "I.FSL.food_consumption_score.HH", "I.WASH.visible_waste_often_always.HH", 
  "I.WASH.handwashing_critical_times.HH", "I.FSL.food_source_cash.HH", 
  "I.FSL.food_source_borrow.HH", "I.FSL.food_source_assistance.HH", 
  "I.EDU.hh_with_school_children.HH", "I.HEALTH.plw_total_hh.HH", 
  "I.FSL.income_source_borrow.HH", "I.HEALTH.pregnant_women_hh.HH", 
  "I.HEALTH.atleast_onepregnant_ANC.HH", "I.HEALTH.all_pregnant_women_anc.HH", 
  "I.HH_CHAR.arrival_date.HH", "I.HH_CHAR.highest_education.HH", 
  "I.HH_CHAR.bangla_english.HH", "I.EDU.remote_learning_hh.HH", 
  "I.HH_CHAR.no_working_age.INDVHH", "I.HH_CHAR.no_male_working_age.INDVHH", 
  "I.HH_CHAR.high_dep_ratio.INDVHH", "I.HH_CHAR.ind_work_5_17_hh.INDVHH", 
  "I.HH_CHAR.ind_6_59_months_hh.INDVHH", "I.HH_CHAR.ind_5_17_hh.INDVHH", 
  "I.HEALTH.ind_need_treatment_hh.INDVHH", "I.HH_CHAR.ind_work_hh.INDVHH", 
  "I.EDU.ind_out_of_school.INDVHH", "I.NUTRITION.i.ind_6_59_months_plw_hh.HH"
)

res.mca <- MCA(hh2020_comps, graph = FALSE)
pdf(file = "My Plot.pdf",   # The directory you want to save the file in
    width = 33, # The width of the plot in inches
    height = 46) # The height of the plot in inches

fviz_mca_biplot(res.mca, 
                # re  pel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

dev.off()
fviz_mca_var(res.mca, choice = "mca.cor", 
             # repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

xls_lt %>% 
  filter(str_detect(question_name,"ind_smoke")) %>% pull(question_relevant)
ind2020$ind_smoke

xls_lt %>% 
  filter(str_detect(question_name,"children_missing")) %>% pull(`question_label::english`)


mean_prop2020(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = "I.HH_CHAR.education_level2.HH")

hh2020_svy$variables$children_missing






hh2020 %>% 
  mutate(
    single_female_yn= 
    hoh_gender_marital=paste0(I.HH_CHAR.gender_hoh.HH,"_" ,hoh_marital),
         ) %>% 
  count(hoh_gender_marital)

# all_graphs1 %>% length()
numerics_and_factors_comparable %>% filter( indicator=="income_source")
all_graphs1$hoh_marital
all_graphs1$income_source
hypegrammaR:::reach_style_barchart
all_graphs1$debt_reason
all_graphs1$hh_priority_needs
hh2019_svy$variables %>% count(I.HH_CHAR.gender_hoh.HH)
hh2019_svy$variables$hh_coping_mechanism.reduce_expenditure
hh2020_svy$variables %>% count(I.HH_CHAR.gender_hoh.HH)
hh2020_svy$variables %>% count(reduced_nonessential_expenditures)
hh2020_svy$variables %>% count(resp_gender)
hh2019_svy$variables %>% count(respondent_gender)
hh2020_svy$variables %>% select(contains("expend")) %>% colnames()
hh2020_svy$variables %>% select(contains("income")) %>% colnames()
hh2020_svy$variables %>%filter(income_source.none==1) %>% 
  select(contains("income")) %>% 
  View()# %>% colnames()
hh2020_svy$variables %>% count(income_source.none,expenditures_reduce.none) %>% mutate(sum_n=sum(n))
41/836
