rm(list = ls())
library(rmarkdown)

population<-c("host","refugee","IOM")[1]
write_output<-c("yes","no")[1]
day_to_run <- Sys.Date()

if (population == "refugee") {
title <- "MSNA 2020 (Refugee)"
render("02_DailyMonitoringReport.Rmd")
copy_to_drp<- "C:\\Users\\MEHEDI\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70XXX - J-MSNA 2020\\03 Data Collection and Planning\\daily_monitoring_report/refugee/"
renamed_copy_to_drp <- paste0(copy_to_drp,str_replace_all(day_to_run,"-","_"),
                              "_Daily Monitoring Report.html")
file.copy(from = "02_DailyMonitoringReport.html",to = renamed_copy_to_drp,overwrite = T)
}

if (population == "IOM") {
  title <- "MSNA 2020 (IOM)"
  render("02_DailyMonitoringReport.Rmd")
  copy_to_drp<- "C:\\Users\\MEHEDI\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70XXX - J-MSNA 2020\\03 Data Collection and Planning\\daily_monitoring_report/IOM/"
  renamed_copy_to_drp <- paste0(copy_to_drp,str_replace_all(day_to_run,"-","_"),
                                "_Daily Monitoring Report.html")
  file.copy(from = "02_DailyMonitoringReport.html",to = renamed_copy_to_drp,overwrite = T)
}

if(population == "host"){
  title <- "MSNA 2020 (Host)"
  render("02_DailyMonitoringReport.Rmd")
  copy_to_drp<- "C:\\Users\\MEHEDI\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70XXX - J-MSNA 2020\\03 Data Collection and Planning\\daily_monitoring_report/host/"
  renamed_copy_to_drp <- paste0(copy_to_drp,str_replace_all(day_to_run,"-","_"),"_Daily Monitoring Report.html")
  copy_frm <- "D:\\mh1\\REACH\\2020_BGD_MSNAs/02_DailyMonitoringReport.html"
  file.copy(from = copy_frm,to = renamed_copy_to_drp,overwrite = T)
}
