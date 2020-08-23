
if(population == "refugee"){
  hh_path <- "inputs/refugee/raw_data/hh.csv"
  indv_path <- "inputs/refugee/raw_data/indv.csv"
  date_log_path <- "outputs/refugee/date_log/"
  path_unzip <- "other/audit_temp"
  audit_zip_dir<-"inputs/refugee/audit/"
  audit_zipfile <-paste0(audit_zip_dir,"ahmdEU6aWe9Kwxkt7kgkQq_",str_replace_all (day_to_run,"-","_"),".zip")
  copy_zip_to<-paste0("outputs/refugee/audit/",day_to_run,".zip")
  audit_node<-"/ahmdEU6aWe9Kwxkt7kgkQq/"
  hh_path_clean <- "inputs/refugee/clean_data/hh.csv"
  indv_path_clean <- "inputs/refugee/clean_data/indv.csv"
  tool_path <- "DAP/refugee/tool/JMSNA2020_Refugee_KoBo_Training_v1.xls"
  pop_path <- "DAP/refugee/population/pop_UNHCR_march_2020.csv"
  recoding_output_hh <- "outputs/refugee/composite_indicator/composite_indicator.csv"
  recoding_output_indv <- "outputs/refugee/composite_indicator/INDV_composite_indicator.csv"
  # analysis_strata<-"regional_strata"
  sf_strata<-"Camp"
  sf_pop<- "Total.Families"
  df_strata<- "camp_name_fix"
}

if(population == "IOM"){
  hh_path <- "inputs/IOM/raw_data/hh.csv"
  indv_path <- "inputs/IOM/raw_data/indv.csv"
  date_log_path <- "outputs/IOM/date_log/"
  path_unzip <- "other/audit_temp"
  audit_zip_dir<-"inputs/IOM/audit/"
  audit_zipfile <-paste0(audit_zip_dir,"aCPxJ4juLvoEGPp9CtNSC3_",str_replace_all (day_to_run,"-","_"),".zip")
  copy_zip_to<-paste0("outputs/IOM/audit/",day_to_run,".zip")
  audit_node<-"/aCPxJ4juLvoEGPp9CtNSC3/"
}

if(population == "host"){
  hh_path <- "inputs/host/raw_data/hh.csv"
  indv_path <- "inputs/host/raw_data/indv.csv"
  date_log_path <- "outputs/host/date_log/"
  path_unzip <- "other/audit_temp"
  audit_zip_dir<-"inputs/host/audit/"
audit_zipfile <-paste0(audit_zip_dir,"aBR5eNQQypf3BzASncrurM_",str_replace_all (day_to_run,"-","_"),".zip")
copy_zip_to<-paste0("outputs/host/audit/",day_to_run,".zip")
audit_node<-"/aBR5eNQQypf3BzASncrurM/"
hh_path_clean <- "inputs/host_combind/clean_data/hh.csv"
indv_path_clean <- "inputs/host_combind/clean_data/indv.csv"
tool_path <- "DAP/host/tool/JMSNA2020_HC_KoBo_Training_v1.xls"
pop_path <- "DAP/host/population/Population_Figures_2011_Census_HostCommunity.csv"
recoding_output_hh <- "outputs/host/composite_indicator/composite_indicator.csv"
recoding_output_indv <- "outputs/host/composite_indicator/INDV_composite_indicator.csv"
df_strata<-"upazilla_name"
sf_strata<-"Upazila"
sf_pop<- "HH_pop"
# analysis_strata<-"Thana"
}
