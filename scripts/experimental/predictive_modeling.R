# library(rgee)
# ee_install()
# ee_Initialize()
# n
library(tidymodels)
library(tidyverse)
population<-c("host","refugee")[2]
# write_output<-c("yes","no")[1]
day_to_run <- Sys.Date()
source("scripts/active_path.R")

# read_data ---------------------------------------------------------------

hh_data <- read.csv(recoding_output_hh, stringsAsFactors = FALSE, na.strings=c("", " ", NA))
hh_data %>% count(informed_consent)
hh_data %>% select(starts_with("i.")) %>% colnames() %>% dput()

vars_to_analyze<-c("I.FSL.fcs.HH","I.HH_CHAR.gender_hoh.HH", "I.HH_CHAR.age_hoh.HH", "I.HH_CHAR.single_hoh.HH", 
  "I.HH_CHAR.elderly_hoh.HH", "I.HH_CHAR.large_hh.HH", "I.HEALTH.disabled_hh.HH", 
  "I.FSL.no_income.HH", "I.HEALTH.plw_total_hh.HH",  "I.HEALTH.pregnant_women_hh.HH", 
  "I.HEALTH.all_pregnant_women_anc.HH", 
  "I.HH_CHAR.arrival_date.HH", "I.HH_CHAR.highest_education.HH")

hh_data %>% ggplot(aes(I.HH_CHAR.age_hoh.HH,I.FSL.fcs.HH))+geom_point()
hh_data %>% ggplot(aes(I.FSL.fcs.HH,fill=I.HH_CHAR.single_hoh.HH))+geom_boxplot()
hh_data %>% ggplot(aes(I.FSL.fcs.HH,fill=I.HH_CHAR.elderly_hoh.HH))+geom_boxplot()
hh_data %>% ggplot(aes(I.FSL.fcs.HH,fill=I.HH_CHAR.large_hh.HH))+geom_boxplot()
hh_data %>% ggplot(aes(I.FSL.fcs.HH,fill=I.FSL.no_income.HH))+geom_boxplot()
hh_data %>% ggplot(aes(I.FSL.fcs.HH,fill=I.HEALTH.pregnant_women_hh.HH))+geom_boxplot()
hh_data %>% ggplot(aes(I.FSL.fcs.HH,fill=I.HEALTH.all_pregnant_women_anc.HH))+geom_boxplot()
hh_data %>% ggplot(aes(I.FSL.fcs.HH,fill=I.HH_CHAR.highest_education.HH))+geom_boxplot()
hh_data %>% ggplot(aes(I.FSL.fcs.HH,fill=I.HH_CHAR.arrival_date.HH))+geom_boxplot()
hh_data %>% count(I.HH_CHAR.arrival_date.HH)

junk_cols<- c("X_uuid",
              "enum_comment",
              "X_submission_time",
              "instance_name",
              "phone_number", 
              "block_number_001",
              "enum_comment",
              "survey_date","survey_start","deviceid","end_survey")

concat_sm<-butteR::auto_detect_select_multiple(hh_data)
hh_data$health_coping
hh_data %>%
  select(-concat_sm) %>%
  select_if(~is.character(.)|starts_with(paste0(concat_sm,".")))# %>%
  select(-junk_cols) %>% colnames()
group_names<-hh_data %>%
  select(-concat_sm) %>% 
  select_if(is.character) %>%
  select(-junk_cols) %>%
  select(-ends_with("_other")) %>% 
  colnames()
sm<- hh_data %>% select(starts_with(paste0(concat_sm,"."))) %>% colnames()
group_names<-c(group_names,sm)

investigate_differences<- function(df,var_of_interest,list_of_vars){
 res_list<-list()
 res_df_list<-list()
 final_res<-list()
  for(i in 1:length(list_of_vars)){
    group_name_temp<- list_of_vars[i]
    print(group_name_temp)
    grouped_means<-hh_data %>% 
      group_by(!!sym(group_name_temp)) %>% 
      summarise(
        # mean_fcs=mean(!!sym(var_of_interest),na.rm=T),
        median_fcs=median(!!sym(var_of_interest),na.rm=T),
        n=n()
        )
      mean_diff<-grouped_means %>%   pull(median_fcs) %>% range(na.rm = T) %>% diff()
    res<- data.frame(indicator=group_name_temp,difference_fcs=mean_diff)
    res_list[[group_name_temp]]<- data.frame(indicator=group_name_temp,difference_fcs=mean_diff)
    res_df_list[[group_name_temp]]<-grouped_means %>%
      pivot_longer(!!sym(group_name_temp), names_to = "indicator",values_to="option") %>%
      mutate(option=as.character(option)) %>% 
      select(indicator,option,everything())
    
  }
 asdf<-list()
 asdf$differences_df<-bind_rows(res_list) %>% arrange(desc(difference_fcs)) #%>% slice(1:40)

 asdf$res_df<-bind_rows(res_df_list) %>% left_join(asdf$differences_df)
 # %>% filter(indicator %in% asdf$differences_df$indicator)
 
 return(asdf)
}

asdf<-investigate_differences(hh_data,var_of_interest = "I.FSL.fcs.HH",list_of_vars = group_names)

asdf$res_df %>% filter(!indicator %in% c("block_number","X.1","X.2","datearrival_shelter","edu_highest")) %>% 
  group_by(indicator) %>% 
  filter(!any(n<=3)) %>% 
  print(n=nrow(.))
asdf %>% filter(str_detect(indicator,"^X")==F)
asdf %>% count(indicator)
 res<-list()
for(i in 1:length(group_names)){
  group_name_temp<- group_names[i]
  print(group_name_temp)
  difference<-hh_data %>% 
    group_by(!!sym(group_name_temp)) %>% 
    summarise(
      mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
      median_fcs=median(I.FSL.fcs.HH)) %>% 
    pull(median_fcs) %>% range(na.rm = T) %>% diff()
  res[[group_name_temp]]<- data.frame(indicator=group_name_temp,difference_fcs=difference)
  
}
fcs_score_differences<-bind_rows(res)
fcs_score_differences %>% arrange(desc(difference_fcs)) %>% head(20)

hh_data %>% group_by(food_assistance_quality) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T))
hh_data %>% group_by(parent_committee) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T))

#why do peopl 
hh_data %>% group_by(I.HH_CHAR.no_working_age.INDVHH) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            median_fcs=median(I.FSL.fcs.HH,na.rm=T),
            number_obs=n())
hh_data %>% group_by(information_training_received ) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            median_fcs=median(I.FSL.fcs.HH,na.rm=T),
            number_obs=n())
hh_data %>% group_by(water_2 ) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            median_fcs=median(I.FSL.fcs.HH,na.rm=T),
            number_obs=n())
hh_data %>% group_by(water_access_to_safe_water ) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            median_fcs=median(I.FSL.fcs.HH,na.rm=T),
            number_obs=n())
hh_data %>% group_by(organisation_of_aid_distributions  ) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            median_fcs=median(I.FSL.fcs.HH,na.rm=T),
            number_obs=n())

hh_data %>% group_by(modality_fuel) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T))
hh_data %>% group_by(edu_highest) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T))
hh_data %>% group_by(hoh_marital) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            num_obs=n())
hh_data %>% group_by(registration_services_15) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            num_obs=n())
hh_data %>% group_by(registration_services) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            num_obs=n())
hh_data %>% group_by(modality_nfi) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            num_obs=n())
hh_data %>% group_by(I.HH_CHAR.arrival_date.HH) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T),
            num_obs=n())

hh_data %>% 
  group_by(I.HEALTH.all_pregnant_women_anc.HH) %>% 
  summarise(mean_fcs=mean(I.FSL.fcs.HH,na.rm=T)) %>% 
  pull(mean_fcs) %>% range(na.rm = T) %>% diff()
  
# hh_data %>% filter(is.na(I.FSL.fcs.HH))
composites<- hh_data %>% 
  select(vars_to_analyze) %>% 
  mutate(I.FCS_class= case_when(I.FSL.fcs.HH>42~"Good",
                                I.FSL.fcs.HH>28~"Borderline",
                                TRUE~"Poor"
                                )) %>% 
  mutate_if(is.character,~as.factor(.)) %>% 
  mutate_if(is.numeric,~as.factor(.)) %>% 
  select(-I.FSL.fcs.HH)


set.seed(1234)
composites_split<- initial_split(composites)
composite_train<- training(composites_split)
composite_test<- testing(composites_split)

composite_recipe<-recipe(I.FCS_class~.,data=composite_train) %>% 
  step_downsample(I.FCS_class) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric()) %>% 
  step_normalize(all_numeric()) %>% 
  prep()

bake(composite_recipe, new_data = composite_train)
test_proc<-bake(composite_recipe, new_data = composite_test)
juice(composite_recipe)   

knn_spec<-nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification") 
tree_spec<-decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification") 

tree_fit<- tree_spec %>% 
  fit(I.FCS_class~.,
      data=juice(composite_recipe))

#EVALUATE
set.seed(1234)
validation_splits<-mc_cv(juice(composite_recipe),prop=0.9)
tree_res<-fit_resamples(
  I.FCS_class~.,
  tree_spec, 
  validation_splits,
  control=control_resamples(save_pred=TRUE)
)

tree_res %>% 
  collect_metrics()

tree_res %>% 
  unnest(.predictions) %>% 
  mutate(
    model="tree"
  ) %>% 
  roc_curve(I.FCS_class,.pred_Good)

tree_res %>% 
  unnest(.predictions) %>%  
  conf_mat(I.FCS_class,.pred_class) %>% 
  autoplot()
tree_res %>% 
  unnest(.predictions) %>%  
  conf_mat(I.FCS_class,.pred_class) %>% 
  autoplot(type="heatmap")


