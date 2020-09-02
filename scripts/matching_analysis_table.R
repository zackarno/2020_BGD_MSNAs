rm(list = ls())
library(dplyr)

# read_table --------------------------------------------------------------

indicator_2020 <- read.csv("inputs/analysis_table/2020/Analysis_tables_Refugee_full.csv",stringsAsFactors = FALSE, 
                           na.strings=c("", " ", NA)) %>% colnames() %>% sort()
indicator_2019_hh <- read.csv("inputs/analysis_table/2019/2019_10_02_Refugee_HH_DAP_Simple_overall_.csv",
                              stringsAsFactors = FALSE, na.strings=c("", " ", NA)) %>% colnames()
indicator_2019_indv <- read.csv("inputs/analysis_table/2019/2019_10_02_Refugee_Indiv_DAP_Simple_overall_.csv",
                                stringsAsFactors = FALSE, na.strings=c("", " ", NA)) %>% colnames()

indicator_2019 <- c(indicator_2019_hh,indicator_2019_indv) %>% sort()

max.len = max(length(indicator_2020), length(indicator_2019))

indicator_2020 = c(indicator_2020, rep(NA, max.len - length(indicator_2020)))
indicator_2019 = c(indicator_2019, rep(NA, max.len - length(indicator_2019)))

compare_data_frame <- data.frame(
  indicators_2020_I. = indicator_2020,
  indicators_2019_I. = indicator_2019
) %>% dplyr::mutate(
  indicators_2020_all_lower = tolower(indicators_2020_I.),
  indicators_2019_all_lower = tolower(indicators_2019_I.)
)



# exact_match-------------------------------------------------------------------------

exact_match <- compare_data_frame %>% filter(indicators_2019_all_lower %in% indicators_2020_all_lower) %>% 
  select(indicators_2019_all_lower) 


compare_data_frame_2020 <- compare_data_frame %>% filter(indicators_2020_all_lower %in% indicators_2019_all_lower) %>% 
  select(indicators_2020_all_lower,indicators_2020_I.) %>% arrange(indicators_2020_all_lower)

compare_data_frame_2019 <- compare_data_frame %>% filter(indicators_2019_all_lower %in% indicators_2020_all_lower) %>% 
  select(indicators_2019_all_lower,indicators_2019_I.) %>% arrange(indicators_2019_all_lower)

exact_match_df <- cbind(compare_data_frame_2019,compare_data_frame_2020)
exact_match_names <- compare_data_frame_2019$indicators_2019_all_lower %>% dput()

write.csv(exact_match_df,"inputs/analysis_table/exact_match.csv")


# strat_with_i ------------------------------------------------------------

indicator_2020 <- read.csv("inputs/analysis_table/2020/Analysis_tables_Refugee_full.csv",stringsAsFactors = FALSE, 
                           na.strings=c("", " ", NA))  %>% select(starts_with("I."))
indicator_2019_hh <- read.csv("inputs/analysis_table/2019/2019_10_02_Refugee_HH_DAP_Simple_overall_.csv",
                              stringsAsFactors = FALSE, na.strings=c("", " ", NA))%>% select(starts_with("I."))
indicator_2019_indv <- read.csv("inputs/analysis_table/2019/2019_10_02_Refugee_Indiv_DAP_Simple_overall_.csv",
                                stringsAsFactors = FALSE, na.strings=c("", " ", NA))%>% select(starts_with("I."))

indicator_2019 <- cbind(indicator_2019_hh,indicator_2019_indv)
indicator_2020_c_names <- indicator_2020 %>% colnames() %>% sort()
indicator_2019_c_names <- indicator_2019 %>% colnames()%>% sort()


max.len = max(length(indicator_2020_c_names), length(indicator_2019_c_names))

indicator_2020_c_names = c(indicator_2020_c_names, rep(NA, max.len - length(indicator_2020_c_names)))
indicator_2019_c_names = c(indicator_2019_c_names, rep(NA, max.len - length(indicator_2019_c_names)))

compare_data_frame_starts_with_I <- data.frame(
  indicators_2020_I. = indicator_2020_c_names,
  indicators_2019_I. = indicator_2019_c_names
)
write.csv(compare_data_frame_starts_with_I,"inputs/analysis_table/output_to_match/strat_withI.csv")
