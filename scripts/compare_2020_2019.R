library(butteR)
library(tidyverse)
library(googlesheets4)
library(srvyr)
population<- c("host","refugee")[2]
day_to_run <- Sys.Date()
source("scripts/active_path.R")


ks<- readxl::read_xlsx(tool_path %>% str_replace(".xls",".xlsx"),"survey")
kc<-readxl::read_xlsx(tool_path %>%  str_replace(".xls",".xlsx"),"choices")
xls_lt<-make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::english")


hh2019<-read.csv(paste0("inputs/",population,"/significance_testing/2019_hh_",population,"_with_composites.csv"), na.strings =c(""," ",NA,"NA"), stringsAsFactors=T)

hh2020<-read.csv(paste0("inputs/",population,"/significance_testing/2020_hh_refugee_with_composites.csv"), na.strings =c(""," ",NA,"NA"), stringsAsFactors=T)


hh2019_svy<- as_survey(.data= hh2019,weights=weights)                 

hh2020_svy<- as_survey(.data= hh2020,weights=weights)                 

comparison_names<-read_sheet(ss = "1src6hpfU6h6WH97wATEP_4hmW_vHpgt4RY-Ps9zqCrs",
                             sheet="msna_2019_2020_lookup",guess_max = 449) 

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
                   geom_errorbar(aes(ymin=ci_low, ymax=ci_upp), width=.1,position = pd)+
                   labs(x=x, y= "mean %")+
                   reach_style()+
                   theme(
                     axis.text.x = element_text(angle = 45)
                   )
) %>% setNames(indicators_to_map)




# try again with fixed comparison lookups ---------------------------------

comparison_lt<-readxl::read_xlsx("inputs/refugee/significance_testing/dataset_compare_refugee.xlsx")
comparison_lt_hh<-comparison_lt %>%
  filter(!is.na(column_name_2019)) %>% 
  filter(population==stringr::str_to_title(population),Group=="HH") %>% 
  filter(!str_detect(column_name_2020,"\\."))
comparison_lt_hh<-comparison_lt_hh %>% 
  slice(5:nrow(.))










remove_kobo_grouper
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
