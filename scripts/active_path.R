
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
  hh_path_clean <- "inputs/IOM/clean_data/hh.csv"
  indv_path_clean <- "inputs/IOM/clean_data/indv.csv"
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
hh_path_clean <- "inputs/host/clean_data/hh.csv"
indv_path_clean <- "inputs/host/clean_data/indv.csv"
}
