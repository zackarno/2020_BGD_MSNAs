rm(list = ls())

library(tidyverse)
library(butteR)
library(survey)
library(srvyr)
library(dplyr)

population<-c("host","refugee","IOM")[2]
write_output<-c("yes","no")[1]
day_to_run <- Sys.Date()
source("scripts/active_path.R")

# read_data ---------------------------------------------------------------

hh_data <- read.csv(recoding_output_hh, stringsAsFactors = FALSE, na.strings=c("", " ", NA))
ind_data <- read.csv(recoding_output_indv, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

tool_survey_sheet <- readxl::read_xls(tool_path,sheet = "survey")
tool_choices_sheet <- readxl::read_xls(tool_path,sheet = "choices")

pop <- read.csv(pop_path,stringsAsFactors = FALSE, na.strings=c("", " ", NA))



# weighting ---------------------------------------------------------------

if(population == "refugee"){
  pop<-pop %>% 
    filter(!is.na(Camp),is.na(Block)) %>% 
    # filter(Camp!="Kutupalong RC") %>% 
    mutate(
      !!(sf_strata):=stringr::str_replace(Camp, "Total","") %>% trimws(),
      !!(sf_strata):= stringr::str_replace_all(Camp,"Extension","Ext"),
      Total.Families=readr::parse_number(Total.Families %>% stringr::str_replace_all(",","")),
      Total.Individuals= readr::parse_number(Total.Individuals %>% stringr::str_replace_all(",",""))
    ) %>% 
    filter(!!sym(sf_strata)!="Kutupalong RC") 
  hh_data$camp_name2 <- hh_data$camp_name %>% str_replace_all("_"," ") %>% str_replace_all("e","E") %>% 
    str_replace_all("w","W") %>% str_replace_all("camp ktp","Kutupalong RC") %>% 
    str_replace_all("camp nya","Nayapara RC")  %>% str_replace_all("c","C")
}
