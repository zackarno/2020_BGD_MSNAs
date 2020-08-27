library(googlesheets4)
library(tidyverse)
lt1<-read_sheet(ss = "1src6hpfU6h6WH97wATEP_4hmW_vHpgt4RY-Ps9zqCrs",sheet="lookup")
lt2<-read_sheet(ss = "1src6hpfU6h6WH97wATEP_4hmW_vHpgt4RY-Ps9zqCrs",sheet="lookup2")
lookup_sectors<-read_sheet(ss = "1src6hpfU6h6WH97wATEP_4hmW_vHpgt4RY-Ps9zqCrs",sheet="lookup_sectors")
#LT1 IS OLD COLUMN NAMES WITH FILLED IN VALS
# LT 2 RELECTS NEW COLUMN NAMES FOR HC

lt1 <- lt1 %>% 
  mutate(analysis_code_rep=analysis_code)
lt2<-lt2 %>% 
  mutate(there=rep(1,nrow(.)))
lt12<-lt2 %>% left_join(lt1,by="analysis_code")
lt12 %>% 
  filter(is.na(analysis_code_rep)) %>% print(n=nrow(.))

#THESE ARE WHAT IS MISSING
lt12 %>% 
  filter(is.na(analysis_code_rep)) %>% pull(analysis_code)

# I CAN JUST WIRTE LT12 TO SHEET 3 AND FILL IN THE NA!
#BUT FIRST I SHOULD SEE IF I CAN MERGE THE SECTORS FROM THE ORIGINAL ANALYSIS
lookup_sectors<-lookup_sectors %>% 
  filter(analysis_code!="Missing")
lt123<-lt12 %>% left_join(lookup_sectors) %>% print(n=100)
lt123<- lt123 %>% 
  select(-there, -number, -analysis_code_rep, - ...7) %>% 
  select(broad_indicator,strata_1, strata_2, analysis_code, specific_indicator_label, sector)

# lt123 %>% write_csv("merged_lookuptable.csv",na="")
# googlesheets4::write_sheet(data = lt123,ss ="1src6hpfU6h6WH97wATEP_4hmW_vHpgt4RY-Ps9zqCr"" )