
dep_temp<-"security_issues.no_concerns"
svychisq_posthoc_test(design = hhsvy,independent_var = "I.HH_CHAR.arrival_date.HH",dependent_var = "security_issues.no_concerns") %>% 
  filter(value=="p_value") %>%
  mutate_all(~as.character(.)) %>% 
  pivot_longer(
    cols = starts_with(dep_temp),
    names_to = "asdfg",
    values_to = "rank",
    values_drop_na = TRUE
  ) %>% 
  separate(col=asdfg,sep="\\_(?!.*_)",into = c("dep_var","dep_var_choice")) 
  

colnames(all_res_data)
all_res_data %>% filter(ind_var_name=="I.HH_CHAR.arrival_date.HH") %>% 
  filter(indicator=="security_issues.no_concerns") %>% 
  data.frame()
                        

all_res_data$indicator %>% unique()
all_res_data$d %>% unique()

all_res_data %>% filter(ind_var_name=="I.HH_CHAR.arrival_date.HH", 
                        indicator=="I.HH_CHAR.enough_information_for_all.HH") %>% 
  select(ind_var_name,ind_var_name_value,indicator,option,p_value,p_value_pair=rank) #%>% View()
all_res_data %>% filter(ind_var_name=="I.HH_CHAR.arrival_date.HH", 
                        indicator=="I.HH_CHAR.enough_information_for_all.HH") %>% 
  select(ind_var_name,ind_var_name_value,indicator,option,mean,mean_low,mean_upp,n_unweighted,p_value,p_value_pair=rank) #%>% View()
all_res_data %>% filter(ind_var_name=="I.HH_CHAR.highest_education.HH", 
                        indicator=="I.EDU.not_send_back_to_school_hh.HH") %>% 
  select(ind_var_name,ind_var_name_value,indicator,option,mean,mean_low,mean_upp,n_unweighted,p_value,p_value_pair=rank) %>% View()

hh$I.EDU.not_send_back_to_school_hh.HH
j1<-all_res_data %>% filter(ind_var_name=="I.HH_CHAR.highest_education.HH", 
                        indicator=="I.EDU.ind_out_of_school.INDVHH") %>% 
  select(ind_var_name,ind_var_name_value,indicator,option,mean,mean_low,mean_upp,n_unweighted,p_value,p_value_pair=rank) #%>% View()


j2<-svychisq_posthoc_test(design = hhsvy,independent_var = "I.HH_CHAR.highest_education.HH",dependent_var = "I.EDU.ind_out_of_school.INDVHH" )%>% filter(value=="p_value") %>% 
  mutate_all(~as.character(.)) %>% 
  pivot_longer(
    cols = starts_with("I.EDU.ind_out_of_school.INDVHH"),
    names_to = "asdfg",
    values_to = "rank",
    values_drop_na = TRUE
  ) %>% 
  separate(col=asdfg,sep="\\_(?!.*_)",into = c("dep_var","dep_var_choice"))

colnames(j1)[colnames(j1)%in%colnames(j2)]
j1 %>% 
  left_join(j2,by=
              c("ind_var_name"="ind_var_name",
                "ind_var_name_value"="indep_var_option",
                "indicator"="dep_var",
                "option"="dep_var_choice"
                ))


hh$I.EDU.ind_out_of_school.INDVHH
debugonce(svychisq_posthoc_test)

[]
mean_prop2020(df = hhsvy,aggregate_by = "I.HH_CHAR.highest_education.HH",variables_to_analyze = "I.EDU.ind_out_of_school.INDVHH")

wat<-svychisq(formula = ~ I.HH_CHAR.arrival_date.HH+I.HH_CHAR.enough_information_for_all.HH,hhsvy)  
wat 
wat$stdres
?svychisq

hhsvy$variables$I.HH_CHAR.enough_information_for_all.HH
debugonce(mean_prop2020)
mean_prop2020(df = hhsvy,aggregate_by ="I.HH_CHAR.arrival_date.HH",variables_to_analyze = "I.HH_CHAR.enough_information_for_all.HH")

  # print(n=nrow(.))
all_res_data %>% filter(ind_var_name=="I.HH_CHAR.arrival_date.HH", indicator=="I.HH_CHAR.incm_src_employ_business.HH") %>% 
  select(ind_var_name,indicator,p_value,option,rank) %>% 
  print(n=nrow(.))
  
