file_names<-list.files("scrap/host_basic_analysis",full.names = T)[1:5]
asdf<-map(file_names,~ aaa<-read_csv(.) %>% 
            select(-X1)
          colnames(aaa)[1]<-"analysis_strata"
          return(aaa))

analysis_levels<-c("respondent_gender", "gender_hoh","Upazila", "Upazila_gender_hoh", "overall")

for(i in 1:length(file_names)){
  df_temp<-read_csv(file_names)
  if(nrow(df_temp))
  
}
file_names %>% length
analysis_levels %>% length

asdf<-map2(file_names,analysis_levels, function(x,y){
  df_temp<-read_csv(x) %>% select(-X1)
  if (nrow(df_temp)>1){
    colnames(df_temp)[1]<-"analysis_strata"

  }
  if(nrow(df_temp)==1){
   df_temp<- df_temp %>% 
      mutate(analysis_strata="overall")
  }
  if(y=="Upazila_gender_hoh"){
    df_temp<-df_temp %>%
      mutate(analysis_strata=paste0(analysis_strata,"_",I.HH_CHAR.gender_hoh.HH)) %>% 
      select(-I.HH_CHAR.gender_hoh.HH)}
  
  df_temp<-df_temp %>% 
    mutate(analysis_level=y) %>% 
    select(analysis_level,analysis_strata, everything())

  return(df_temp)
  })

bind_rows(asdf) %>% write.csv("scrap/host_basic_analysis/2020_08_23_basic_HH_analysis_merged.csv", row.names = F,na="")
