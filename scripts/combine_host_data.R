

iom_hh<- read.csv("inputs/IOM/clean_data/hh.csv",stringsAsFactors = FALSE, na.strings=c("", " ", NA))
host_hh <- read.csv("inputs/host/clean_data/hh.csv",stringsAsFactors = FALSE, na.strings=c("", " ", NA))
hh_colnames <- colnames(host_hh)

iom_hh <- iom_hh[hh_colnames]
host_hh <- host_hh[hh_colnames]

host_hh_combine <- rbind(host_hh,iom_hh)

iom_indv <- read.csv("inputs/IOM/clean_data/indv.csv",stringsAsFactors = FALSE, na.strings=c("", " ", NA))
host_indv<- read.csv("inputs/host/clean_data/indv.csv",stringsAsFactors = FALSE, na.strings=c("", " ", NA))

indv_colnames <- colnames(host_indv)

iom_indv <- iom_indv[indv_colnames]
host_indv <- host_indv[indv_colnames]

host_indv_combine <- rbind(host_indv,iom_indv)

write.csv(host_hh_combine,"inputs/host_combind/clean_data/hh.csv")
write.csv(host_indv_combine,"inputs/host_combind/clean_data/indv.csv")
