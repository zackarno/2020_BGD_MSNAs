library(tidyverse)
library(srvyr)
library(butteR)
df<-read.csv("inputs/host/raw_data/hh.csv",
             na.strings = c(""," "), stringsAsFactors = F)


junky_cols<-c("end_note", "X_id", "X_uuid", 
              "X_submission_time", "X_validation_status", "X_index","survey_date", "survey_start", "deviceid", "end_survey", "instance_name", 
              "audit", "enum_organisation", "enumerator_id", "enum_gender", 
              "respondent_id", "upazilla_name", "union_name", "ward_name", 
              "intro_text")

kc= readxl::read_xlsx("DAP/refugee/tool/Pilot_JMSNA2020_Refugee_v2.xlsx",sheet = "choices")
ks= readxl::read_xlsx("DAP/refugee/tool/Pilot_JMSNA2020_Refugee_v2.xlsx",sheet = "survey")
xls_dictionary<-butteR::make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::english")
df_refactored<-butteR::refactor_to_xlsform(data = df,kobo_survey = ks,kobo_choices = kc,"label::english")
cols_to_analyze<- df_refactored %>% select(-junky_cols) %>% select(-ends_with("_other")) %>% colnames()

dfsvy<-as_survey(df_refactored)
analysis_by_enumerator<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "enumerator_id")
analysis_by_enumerator_gender<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "enum_gender")
#analysis_by_enumerator %>% write.csv("reach_bgd_pilot_refugee_analysis_by_enumerator.csv")
# analysis_by_enumerator_gender %>% write.csv("reach_bgd_pilot_refugee_analysis_by_enumerator_gender.csv")

analysis_by_enumerator_gender<-analysis_by_enumerator_gender %>% filter(enum_gender!="other")

analysis_by_enumerator_gender %>% t() %>% data.frame() %>% tibble::rownames_to_column() 

enum_gender_long<-analysis_by_enumerator_gender %>% pivot_longer(-enum_gender)
windows();enum_gender_long %>% 
  ggplot(aes(x=name,y=enum_gender, fill=value))+geom_tile()


enum_gender_split<-split(enum_gender_long, enum_gender_long$enum_gender)
colnames(enum_gender_split$female)[3]<-"value_female"
colnames(enum_gender_split$male)[3]<-"value_male"
male_female_wide<-enum_gender_split$male %>% left_join(enum_gender_split$female, by="name")

male_female_wide %>% ggplot(aes(x=value_male,y=value_female))+geom_point()
male_female_wide<-male_female_wide %>% 
  mutate(perc_diff=value_male-value_female)

ggplot(male_female_wide, aes(x = value_male,
              y = value_female,
              fill = name))+
  geom_tile() 
  
  

ggplot(male_female_wide, aes(x = value_male,
              y = value_female,
              fill = perc_diff,
              label = ifelse(is.na(perc_diff), "", paste0(perc_diff,"%")))) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("green", "white", "red"), na.value = NA) +
  geom_text() +
  theme_bw()


matrix(rnorm(100, 3, 1), ncol=10)





ggplot(male_female_wide, aes(name, name)) +
  geom_tile(aes(fill = perc_diff)) + 
  # geom_text(aes(label = round(perc_diff, 1))) +
  scale_fill_gradient(low = "white", high = "red")


colnames(analysis_by_enumerator) %>% dput()

wtf<- analysis_by_enumerator %>% select(-enumerator_id) %>% as.matrix()
wtf_names<- analysis_by_enumerator %>% select(-enumerator_id) %>% colnames()
names(wtf)<-wtf_names 
dat <- matrix(rnorm(100, 3, 1), ncol=10)
names(dat) <- paste("X", 1:10)
# install.packages('pheatmap') # if not installed already
library(pheatmap)
wtf_heatmap<-pheatmap(wtf, display_numbers = T)



disab<- c("disability_seeing.no_difficulty", "disability_seeing.some_difficulty", 
"disability_seeing.cannot_do_at_all", "disability_hearing.lot_of_difficulty", 
"disability_hearing.no_difficulty", "disability_hearing.some_difficulty", 
"disability_hearing.cannot_do_at_all", "disability_walking.cannot_do_at_all", 
"disability_walking.lot_of_difficulty", "disability_walking.no_difficulty", 
"disability_walking.some_difficulty", "disability_remembering.cannot_do_at_all", 
"disability_remembering.no_difficulty", "disability_remembering.some_difficulty", 
"disability_remembering.lot_of_difficulty", "disability_self_care.cannot_do_at_all", 
"disability_self_care.lot_of_difficulty", "disability_self_care.no_difficulty", 
"disability_self_care.some_difficulty", "disability_speaking.cannot_do_at_all", 
"disability_speaking.lot_of_difficulty", "disability_speaking.no_difficulty", 
"disability_speaking.some_difficulty")

wtf2<-analysis_by_enumerator %>% select(disab) %>% as.matrix()
names(wtf2)<-paste0("X",1:26)
wtf_heatmap<-pheatmap(wtf2, display_numbers = T,show_rownames = T,show_colnames = T)
wtf_heatmap$tree_row

df %>% 
  group_by(enumerator_id) %>% 
  summarise_all(.funs = ~var(.),
                
                )
number_unique<-df %>% 
  group_by(enumerator_id) %>% 
  summarise_all(
    .funs = ~length(unique(.))
                )

mean_number_unique<-number_unique %>% 
  summarise_all(~mean(.)) 

mean_number_unique %>% t()
