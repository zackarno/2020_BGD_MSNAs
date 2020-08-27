population
if(population=="host"){
ks<-readxl::read_xlsx("DAP/host/tool/JMSNA2020_HC_TOOL_FINAL.xlsx","survey")
kc<-readxl::read_xlsx("DAP/host/tool/JMSNA2020_HC_TOOL_FINAL.xlsx","choices")}
if(population=="refugee"){
ks<-readxl::read_xlsx("DAP/refugee/tool/JMSNA2020_Refugee_TOOL_FINAL.xlsx","survey")
kc<-readxl::read_xlsx("DAP/refugee/tool/JMSNA2020_Refugee_TOOL_FINAL.xlsx","choices")
}

lt<-butteR::make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::english")

lt_so<-lt %>% filter(str_detect(question_type, "^select_one")) %>% select(xml_format_data_col,choice_name)
select_ones<-lt %>% filter(str_detect(question_type, "^select_one")) %>% pull(xml_format_data_col) %>% unique()




select_multiples<-lt %>% filter(str_detect(question_type, "^select_mult")) %>% pull(xml_format_data_col) %>% unique()

select_ones_in_hh<-colnames(clean_hh_fixed)[colnames(clean_hh_fixed) %in% select_ones]
select_ones_in_indiv<-colnames(clean_indiv_fixed)[colnames(clean_indiv) %in% select_ones]
select_multiple_in_hh<-colnames(clean_hh_fixed)[colnames(clean_hh_fixed) %in% select_multiples]
select_multiple_in_indiv<-colnames(clean_indiv_fixed)[colnames(clean_indiv_fixed) %in% select_multiples]
hh_so<-clean_hh_fixed %>% select(select_ones_in_hh)
indiv_so<-clean_indiv_fixed %>% select(select_ones_in_indiv)
hh_sm<-clean_hh_fixed %>% select(select_multiple_in_hh)
indiv_sm<-clean_indiv_fixed %>% select(select_multiple_in_indiv)

check_so_hh<-hh_so %>% map2(select_ones_in_hh, function(x,y){
  lt_temp<-lt_so %>% filter(xml_format_data_col==y) %>% pull(choice_name)
  x<-x[!is.na(x)]
  x[which(!x %in% lt_temp)]
}
)

check_so_hh %>% keep(~length(.)>0)
check_so_indiv<-indiv_so %>% map2(select_ones_in_indiv, function(x,y){
  lt_temp<-lt_so %>% filter(xml_format_data_col==y) %>% pull(choice_name)
  x<-x[!is.na(x)]
  x[which(!x %in% lt_temp)]
}
)
check_so_indiv %>% keep(~length(.)>0)
hh_sm

check_sm_hh<-hh_sm %>% map2(select_multiple_in_hh, function(x,y){
  lt_temp<-lt_so %>% filter(xml_format_data_col==y) %>% pull(choice_name)
  x<-x[!is.na(x)]
  lt_temp_list<-x[which(!x %in% c(0,1))]
  # lt_temp_list %>% keep(~length(.)>0)
  return(lt_temp_list)
}
)
check_sm_hh %>% keep(~length(.)>0)

#INDIV SM
check_sm_indiv<-indiv_sm %>% map2(select_multiple_in_indiv, function(x,y){
  lt_temp<-lt_so %>% filter(xml_format_data_col==y) %>% pull(choice_name)
  x<-x[!is.na(x)]
  lt_temp_list<-x[which(!x %in% c(0,1))]
  # lt_temp_list %>% keep(~length(.)>0)
  return(lt_temp_list)
}
)
check_sm_indiv %>% keep(~length(.)>0)







lt<-butteR::make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::english")
select_multiples<-lt %>% filter(str_detect(question_type, "^select_mult")) %>% pull(xml_format_data_col) %>% unique()
df %>% 
  mutate_at(.vars = select_multiples, ~fct_expand(1,0))



