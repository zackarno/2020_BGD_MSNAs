
# Working Analyze Numerics ------------------------------------------------

analyze_numerics<-function(df,aggregate_by=NULL,variables_to_analyze){

  df<-as_survey(df)
  if(!is.null((aggregate_by))){
  group_by_vars<-syms(aggregate_by)
  numerics_ready<-df %>% 
    group_by(!!!group_by_vars,.drop=F)}
  else{
    numerics_ready<-df
  }
  numerics_wide<-numerics_ready %>%
    summarise_at(.vars = variables_to_analyze,.funs = ~survey_mean(.,na.rm=TRUE,vartype="ci"))
  numerics_long<-numerics_wide %>% 
    pivot_longer(
      -aggregate_by,
      names_to = "indicator"
      
  ) %>% 
    mutate(stat=case_when(str_detect(indicator,"_low$")~"mean_low",
                          str_detect(indicator,"_upp$")~"mean_upp",
                          TRUE~"mean"),
           indicator=str_replace_all(indicator,"_low$|_upp$","")) %>% 
    pivot_wider(names_from = stat ,values_from = value) 
    
  numerics_long
}
analyze_factors<-function(df,aggregate_by=NULL,variables_to_analyze){
  df<-as_survey(df)
  
  res<-list()
  if(is.null(aggregate_by)){
    vars_to_analyze<-variables_to_analyze
  }
  
  for(i in 1:length(vars_to_analyze)){
    var_temp<- vars_to_analyze[i]
    res[[var_temp]]<-df %>% 
      group_by(!!sym(var_temp)) %>% 
      summarise(
        mean=survey_mean(na.rm = T, vartype="ci"),
        n= unweighted(n()) 
      ) %>% 
      pivot_longer(!!sym(var_temp), names_to = "indicator",values_to="option") %>%
      rename_at(.vars = c("mean_low","mean_upp"),.funs = function(x)x<-c("ci_low","ci_upp")) %>% 
      select(indicator,option, everything())
  }
  return(bind_rows(res))
}

# Working Analyze Factors -------------------------------------------------

analyze_factors<-function(df,aggregate_by=NULL,variables_to_analyze){
  df<-as_survey(df)
  res<-list()
  if(is.null(aggregate_by)){
    variables_to_analyze %>% 
      map_dfr(~df %>% 
                group_by(!!sym(.x)) %>% 
                summarise(
                  mean=survey_mean(na.rm=T,vartype="ci"),
                  n_unweighted= unweighted(n())
                ) %>% 
                mutate(indicator=.x) %>% 
                dplyr::select(indicator,option=.x,everything())
      )
  } else{
    aggregate_by<-syms(aggregate_by)
    
    variables_to_analyze %>% 
      map_dfr(~df %>% 
                group_by(!!!(aggregate_by),!!sym(.x)) %>% 
                summarise(
                  mean=survey_mean(na.rm=T,vartype="ci"),
                  n_unweighted= unweighted(n())
                ) %>% 
                mutate(indicator=.x) %>% 
                dplyr::select(!!!aggregate_by,indicator,option=.x,everything())
      )
  }
}


# Experimental Analyze Factors --------------------------------------------

analyze_factors_exp<-function(df,aggregate_by=NULL,variables_to_analyze){
  df<-as_survey(df)
  res<-list()
  group_var1<-quo(.data$x)
  group_var2<-quo(aggregate_by)
  aggregate_by<-c(group_var1, group_var2)
    
    variables_to_analyze %>% 
      map_dfr(~df %>% 
                group_by(!!aggregate_by) %>% 
                summarise(
                  mean=survey_mean(na.rm=T,vartype="ci"),
                  n_unweighted= unweighted(n())
                ) %>% 
                mutate(indicator=.x) %>% 
                select(indicator,option=.x,everything())
      )
  }
analyze_factors_exp2<-function(df,aggregate_by=NULL,variables_to_analyze){
  
  df<-as_survey(df)
  if(!is.null(aggregate_by)){
    dummy_group_rep<-rep(aggregate_by,length(variables_to_analyze))
  }
  if(is.null(aggregate_by)){
    df<-df %>% 
      mutate(dummy_group="dummy_group")
    dummy_group_rep<-rep("dummy_group",length(variables_to_analyze))
  }
  # agg_by<-sym(a)
  res<-list()
  
      mapped_res<-map2_df(variables_to_analyze, dummy_group_rep, function(x,y){
        agby<-syms(c(y,x))
        df %>% 
                group_by(!!!agby) %>%
                summarise(
                  mean=survey_mean(na.rm=T,vartype="ci"),
                  n_unweighted= unweighted(n())
                )     %>% 
                 mutate(indicator=!!sym(x)) #%>% 
                 # select(indicator),everything())
        }
      )
      if(is.null(aggregate_by)){
      mapped_res %>% dplyr::select(indicator, everything(),-dummy_group)
      }else{
  mapped_res %>% dplyr::select(indicator,aggregate_by, everything())
      }
      }


analyze_factors(df = hh2020_svy,aggregate_by = c("hoh_gender","hoh_marital"),variables_to_analyze = test_cols[-c(1,2)])
analyze_factors(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = test_cols[-c(1,2)])

analyze_factors_exp2 %>% debugonce()
debugonce(analyze_factors_exp2)
analyze_factors_exp2(df = hh2020_svy,aggregate_by = c("hoh_gender"),variables_to_analyze = test_cols[-c(1,2)])
analyze_factors_exp2(df = hh2020_svy,aggregate_by = NULL,variables_to_analyze = test_cols[-c(1,2)])

encode_sm_in_list_to_data_format<- function(df, variables_to_analyze, select_multiple_in_data){
  select_multiples_in_list_of_variables<-variables_to_analyze[which(variables_to_analyze%in%select_multiple_in_data)]
  list_of_variables_no_concatenated_select_multiple<-variables_to_analyze[which(variables_to_analyze%in%select_multiple_in_data==FALSE)]
  if(length(select_multiples_in_list_of_variables)>0){
    select_multiples_in_data_with_dot<-paste0(select_multiple_in_data,".")
    select_multiples_in_given_list_with_dot<-paste0(select_multiples_in_list_of_variables, ".")
    vars_selection_helper <- paste0("^(", paste(select_multiples_in_given_list_with_dot, collapse="|"), ")")
    
    select_multiple_logical_names<-select(df$variables, matches(vars_selection_helper)) %>%
      select(-ends_with("_other")) %>% colnames()
    
    list_of_variables<-unique(c(list_of_variables_no_concatenated_select_multiple,select_multiple_logical_names))
  }else{
    list_of_variables<-list_of_variables_no_concatenated_select_multiple
  }
}

mean_prop2020<- function(df, aggregate_by, variables_to_analyze){
  df<-as_survey(df)
  res<-list()
  
  select_multiple_in_data<-auto_detect_select_multiple(df$variables)
  list_of_variables<-encode_sm_in_list_to_data_format(df = df,variables_to_analyze = variables_to_analyze,select_multiple_in_data = select_multiple_in_data)
  
  all_vars_data<-df %>% 
    select(list_of_variables)
  numeric_vars<-all_vars_data %>% select_if(is.numeric) %>% colnames()
  factor_vars<-all_vars_data %>% select_if(~is.character(.)|is.factor(.)) %>% colnames()
  
  
  if(length(numeric_vars)>0){
  numerics_analyzed<-analyze_numerics(df, aggregate_by, numeric_vars)
  res$numerics<- numerics_analyzed}
  if(length(factor_vars)>0){

  factors_analyzed<-analyze_factors(df, aggregate_by, factor_vars)
  res$factors<- factors_analyzed
  }
  return(res)
  
}







#THIS WORKS
hh2020_svy %>% 
  group_by(!!!syms(c("hoh_gender","hoh_marital"))) %>% 
  summarise(
    mean= survey_mean(na.rm=T)
  )

eval(quo("2+2"))
enquo("2020")
rlang::enquo()
rlang::quo()
#WHY CANT I MAP IT LIKE THIS?
test_cols<-c("hoh_gender", "hoh_marital", "datearrival_shelter", "edu_highest")
library(rlang)

group_by_quo<-quo(group_by(!!sym(.x)))
gear_filter <- quo(gear == 5)

asdf<-".x"
asdfg<-sym(asdf)
adsfgh<-quo(asdfg)
aa<-quo(.x)
symx<-sym(".data$x")
enquox<-enquo(.x)

test_fun<-function(df,x,y=NULL){
  if(!is.null(y)){
    aggregate_by<-syms(c(x,y))
    
  }
  df %>%
    group_by(!!!aggregate_by) %>% 
    summarise(
      mean=survey_mean(na.rm=T,vartype="ci"),
      n_unweighted= unweighted(n())
    ) %>% 
    mutate(indicator=.x) %>% 
    select(indicator,option=.x,everything())
}

t2<-test_cols[-1]
debugonce(test_fun)
t2 %>% 
  map_dfr(.f = function(x){test_fun(df = hh2020_svy,x=.x,y = "hoh_gender")})

test_cols %>% 
  map_dfr(~hh2020_svy %>% 
            # group_by(!!sym(.x)) %>% 
            # group_by(!!!symx) %>%
            group_by(!!sym(.x)) %>%
        # !!parse_quo("group_by(!!sym(.x))", env = caller_env()) %>% 
        # !!parse_quo("group_by(!!sym(.x))", env = caller_env()) %>%
        summarise(
        mean=survey_mean(na.rm=T,vartype="ci"),
        n_unweighted= unweighted(n())
        ) %>% 
        mutate(indicator=.x) %>% 
        select(indicator,option=.x,everything())
  )
test_cols %>% 
  map_dfr(~function(hh2020_svy %>% 
            # group_by(!!sym(.x)) %>% 
            # group_by(!!!symx) %>%
            group_by(!!sym(.x)) %>%
        # !!parse_quo("group_by(!!sym(.x))", env = caller_env()) %>% 
        # !!parse_quo("group_by(!!sym(.x))", env = caller_env()) %>%
        summarise(
        mean=survey_mean(na.rm=T,vartype="ci"),
        n_unweighted= unweighted(n())
        ) %>% 
        mutate(indicator=.x) %>% 
        select(indicator,option=.x,everything())
  )

rlang::parse_quosure()


hh2020_svy$variables %>% 
  group_by(hoh_gender) %>% 
  summarise(
    mean=mean(na.rm=T)
  )
test_cols %>% 
  map(~hh2020_svy$variables %>% 
        group_by(!!sym(.)) %>% 
        summarise(
        mean=survey_mean(na.rm=T)
        )
  )


questions_to_analyze_2020<-comparison_names %>% 
  filter(!is.na(msna_2020_indicator_code),!is.na(msna_2019_indicator_code)) %>% 
  mutate(
    question_names_2020= ifelse(str_detect(msna_2020_indicator_code,"\\."),
                                sub('.[^.]*$', '',msna_2020_indicator_code),
                                msna_2020_indicator_code),
    question_names_2019= ifelse(str_detect(msna_2019_indicator_code,"\\."),
                                sub('.[^.]*$', '',msna_2019_indicator_code),msna_2019_indicator_code)
  ) %>% select(msna_2019_indicator_code,msna_2020_indicator_code,question_names_2019,question_names_2020) %>% 
  pull(unique(question_names_2020))


hh_qs_2020<-questions_to_analyze_2020[questions_to_analyze_2020%in%colnames(dfsvy$variables)]

# debugonce(mean_prop2020)
asdf<-mean_prop2020(df = dfsvy, aggregate_by = NULL, variables_to_analyze =hh_qs_2020 )
asdf$numerics %>% print(n=nrow(.))

select_multiple_in_data<-auto_detect_select_multiple(dfsvy$variables)
list_of_variables<-encode_sm_in_list_to_data_format(df = dfsvy,variables_to_analyze = hh_qs_2020,
                                                    select_multiple_in_data = select_multiple_in_data)

all_vars_data<-as_survey(dfsvy) %>% 
  select(list_of_variables)
numeric_vars<-all_vars_data %>% select_if(is.numeric) %>% colnames()
numeric_vars2<- unique(numeric_vars)
debugonce(analyze_numerics)

analyze_numerics(df = dfsvy,aggregate_by = NULL, variables_to_analyze = numeric_vars2
                 )

try_this<- analyze_numerics(df = dfsvy,
                            aggregate_by = c("I.HH_CHAR.gender_hoh.HH","upazila"),
                            variables_to_analyze = numerical_vars[100:200]
)


analyze_factors(df = dfsvyr,
                variables_to_analyze =c('I.FSL.food_source_borrow.HH',"I.FSL.remittances.HH") )

dfsvyr  %>% 
  group_by(I.FSL.food_source_borrow.HH) %>% 
  summarise(
    asdf=survey_mean(na.rm = T),
    n= unweighted(n())
  ) %>% 
  pivot_longer(I.FSL.food_source_borrow.HH,names_to = "indicator",values_to = "value") %>% 
  select(indicator, everything())

survey_tally(dfsvyr)
as_survey(df) %>% 
  group_by(I.FSL.food_source_borrow.HH) %>% 
  summarise(
    asdf=survey_total(,level=0.95,vartype="ci")
  )
#weighted
dfsvyr %>% 
  group_by(I.FSL.food_source_borrow.HH) %>% 
  summarise(
    asdf=survey_total(wt=1,level=0.95,vartype="ci") ,
    n=unweighted(n())
  )
?unweighted
?survey_total
  
?svytotal
dfsvyr$variables %>% count(I.FSL.food_source_borrow.HH)  
  nrow(dfsvy$variables)
?survey_total
my_function <- function(x, y) {
  x %>% 
    group_by(!!sym(y)) %>% 
    summarise(
      survey_mean(mean,na.rm=T)
    )
}

dfsvyr %>% 
  group_by_at(.vars = c("I.FSL.food_source_borrow.HH","I.FSL.remittances.HH") )%>% 
  summarise(asdf=survey_mean(na.rm=T, vartype="ci"))
function

sumthese<-c("I.FSL.food_source_borrow.HH","I.FSL.remittances.HH")
sumthese %>% 
  map(~dfsvyr %>%
        group_by_at(c("I.FSL.food_source_borrow.HH","I.FSL.remittances.HH")) %>%
        summarise(asdf=survey_mean(na.rm=T)))

sumthese %>% 
  map(~dfsvyr %>%
        group_by(c(!!sym(.))) %>%
        summarise(
          mean=survey_mean(na.rm=T))
      )

my_fun <- function(x, choice) 
  
{x %>%
    group_by(!!sym(choice)) %>% 
    summarise(
      asdf=survey_mean(na.rm=T))}

dfsvyr %>% 
  group_by(I.FSL.remittances.HH) %>% 
  summarise(
    asdf=survey_mean(na.rm=T)
  )

list_results <- map(dfsvyr %>% select("I.FSL.remittances.HH"), my_fun, choice = quo("I.FSL.remittances.HH"))

dftest<-dfsvyr %>% select(colnames_df)
pmap(dftest,colnames_df,~my_function(.x,.y))

dfsvyr %>% map2(colnames_df,~my_function)

param_df %>% pmap(~my_function(.x,.y,3))
# debugonce(analyze_factors)
dfsvyr$variables$I.FSL.food_source_borrow.HH<-as.factor(dfsvyr$variables$I.FSL.food_source_borrow.HH)
analyze_factors(df = dfsvyr,variables_to_analyze =  "I.FSL.food_source_borrow.HH")
  
  map(variables_to_analyze)
  
  if(!is.null((aggregate_by))){
    group_by_vars<-syms(aggregate_by)
    numerics_ready<-df %>% 
      group_by(!!!group_by_vars,.drop=F)}
  else{
    numerics_ready<-df
  }
  numerics_wide<-numerics_ready %>%
    summarise_at(.vars = variables_to_analyze,.funs = ~survey_mean(.,na.rm=TRUE,vartype="ci"))
  numerics_long<-numerics_wide %>% 
    pivot_longer(
      -aggregate_by,
      names_to = "indicator"
      
    ) %>% 
    mutate(stat=case_when(str_detect(indicator,"_low*")~"ci_low",
                          str_detect(indicator,"_upp*")~"ci_upp",
                          TRUE~"mean"),
           indicator=str_replace_all(indicator,"_low|_upp","")) %>% 
    pivot_wider(names_from = stat ,values_from = value)
  numerics_long
}
?across
dfsvyr %>% select(where(is.character))
#slight issue here.
numerical_vars<-df %>% select_if(~is.numeric(.)) %>% colnames()
sample_numerical_vars<-numerical_vars[5:20]

try_this<- analyze_numerics(df = dfsvy,
                            aggregate_by = c("I.HH_CHAR.gender_hoh.HH","upazila"),
                            variables_to_analyze = numerical_vars[100:200]
                            )


function(df,vars){
  map(vars,functin(x){})
}
#THIS WORKS
try_this$all %>% 
  pivot_longer(-I.HH_CHAR.gender_hoh.HH
  ) %>% 
  mutate(stat=case_when(str_detect(name,"low")~"ci_low",
                        str_detect(name,"upp")~"ci_upp",
                        TRUE~"mean"),
         name=str_replace_all(name,"_low|_upp","")) %>% 
  pivot_wider(names_from = stat ,values_from = value)







try_this<- analyze_numerics(df = dfsvy,aggregate_by = c("I.HH_CHAR.gender_hoh.HH"),variables_to_analyze = c("hoh_age","respondent_age"))
pivot_longer(try_this$all, 
             cols = -I.HH_CHAR.gender_hoh.HH, 
             names_to = c(".value", "group"),
             # names_prefix = "DateRange",
             names_sep = "_*")

  
  arrange(name)
try_this$all %>% 
  pivot_longer(
               cols = c(!ends_with("low"), !ends_with("upp")),
               names_to= c(".value","level",'z'),
               # names_pattern = "(.*)_(.*)"
               )


df$I.HH_CHAR.gender_hoh.HH

sumthese

try_this %>% 
  pivot_longer(-I.HH_CHAR.gender_hoh.HH) %>% 
  separate(name, c("mean","ci_low","ci_high"),sep = "-")
 ) %>% 
  select(-mean) %>%
  pivot_wider(names_from = mean, values_from = value)

dfsvyr<-as_survey(dfsvy)
dfsvyr %>% 
  group_by(I.HH_CHAR.gender_hoh.HH) %>% 
  summarise_at(.vars = c("hoh_age", "respondent_age","child_enrolment_nfp_total"), ~survey_mean(.,na.rm=T,vartype="ci"))
dfsvyr %>% 
  group_by(I.HH_CHAR.gender_hoh.HH) %>% 
  map()



group_vars <- c("vs", "gear", "carb")
group_syms <- rlang::syms(group_vars)
sym_am <- rlang::sym("am")
mtcars

map2_df(list(sym_am), group_syms, ~ mtcars %>%
          group_by(!!.x, !!.y) %>% 
          summarise(mean_mgp = mean(mpg), median_mpg = median(mpg),count = n()))

nums_analyze<-c("hoh_age", "respondent_age","child_enrolment_nfp_total")

nums_analyze %>% 
  map(~dfsvyr %>% 
        group_by(I.HH_CHAR.gender_hoh.HH) %>%
        summarise( asd= survey_mean(!!sym(.),na.rm=T))
        )

symtest<-sym("hoh_age")
nums_analyze_quo<- quo(nums_analyze)
symtest %>% 
  map(~dfsvyr %>% 
        group_by(I.HH_CHAR.gender_hoh.HH) %>%
        summarise( asd= survey_total(.x,na.rm=T))
  )

svymean_long_batch<- function(x,y,z){
  x %>% 
    group_by(!!sym(y)) %>% 
    summarise(
    survey_mean(asd=!!sym(z))
    )
}

nums_analyze %>% map(~svymean_long_batch(.))

nums_analyze %>% class()
nums_analyze %>% 
  map(~dfsvyr %>% 
        group_by(I.HH_CHAR.gender_hoh.HH) %>% 
        summarise(
          asdf=survey_mean({.},na.rm=T)
        )
  )
        
dfsvy %>% 
  group_by(I.HH_CHAR.gender_hoh.HH) %>% %>% 
  summarise(map(~.))

sadf<-map(list(nums_analyze),~dfsvyr %>%
    group_by(factor(I.HH_CHAR.gender_hoh.HH)) %>% 
    summarise(
      asdf=survey_mean(!!.x,na.rm=T,vartype="ci")
    ))
dfsvyr %>% 
  group_by(I.HH_CHAR.gender_hoh.HH) %>% 
  summarise(
    asdf=survey_mean(!!sym("hoh_age"),na.rm=T)
  )
domeans<-function(x){
  dfsvyr %>% 
    group_by(I.HH_CHAR.gender_hoh.HH) %>% 
    summarise(
      asdf=survey_mean(x,na.rm=T)
    )}
  
map(nums_analyze,~domeans(.)  

sadf<-map(syms(nums_analyze),~dfsvyr %>%
    group_by(factor(I.HH_CHAR.gender_hoh.HH)) %>% 
    summarise(
      asdf=survey_mean(!!.x,na.rm=T,vartype="ci")
    ))

dfsvyr$variables[["hoh_age"]]
  
}
    ~dfsvyr %>%
      group_by(factor(I.HH_CHAR.gender_hoh.HH)) %>% 
      summarise(
        survey_mean(!!sym(.),na.rm=T,vartype="ci")
      )
    )

dfsvyr %>%
  group_by(as.factor(I.HH_CHAR.gender_hoh.HH)) %>% 
  summarise(
    survey_mean(hoh_age,na.rm=T,vartype="ci")
  ) 


    map(nums_analyze, 
        ~dfsvyr %>%
          group_by(I.HH_CHAR.gender_hoh.HH) %>% 
          summarise(
            survey_mean(.x,na.rm=T,vartype="ci")
          )    

map_df(list(sym_am), ~ mtcars %>%
          group_by(!!.x, !!.y) %>% 
          summarise(mean_mgp = mean(mpg), median_mpg = median(mpg),count = n()))
ex_group<-dfsvyr %>% 
  group_by(I.HH_CHAR.gender_hoh.HH) 

ex_group %>% purrr::map(function(x)~survey_mean(.))


df1 <- tribble(
  ~"np_id", ~"np_city_size", ~"cc_hf_1", ~"cc_hf_2", ~"cc_hf_3", ~"cc_hf_4", ~"cc_hf_5", ~"cc_hf_6", ~"cc_ac_1", ~"cc_ac_2", ~"cc_ac_3", ~"cc_ac_4", ~"cc_ac_5", ~"cc_ac_6",
  "81", "village", NA, NA, 1L, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  "82", "village", 1L, NA, NA, NA, 1L, NA, NA, NA, NA, 1L, NA, NA,
  "83", "more than 500k inhabitants", NA, 1L, NA, NA, NA, NA, NA, 1L, NA, NA, NA, NA,
  "85", "more than 500k inhabitants", NA, 1L, NA, NA, NA, NA, NA, 1L, NA, NA, NA, NA,
  "87", "more than 500k inhabitants", NA, 1L, NA, NA, NA, NA, NA, NA, 1L, NA, NA, NA,
  "89", "village", 1L, NA, NA, 1L, NA, NA, 1L, NA, NA, NA, NA, NA,
  "90", "village", 1L, NA, NA, NA, NA, NA, 1L, NA, NA, NA, NA, NA,
  "91", "village", 1L, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L, NA,
  "92", "village", NA, NA, NA, NA, NA, 1L, NA, NA, NA, NA, NA, 1L
)

df1 %>%
  pivot_longer(cols = -(starts_with("np_"))) %>%
  separate(name, into = c("cc", "type", "num"), sep = "_") %>% 
  select(-cc) %>%
  pivot_wider(names_from = type, values_from = value)


data <- mtcars 

data$condition <- as.factor(c(rep("control", 16), rep("treat", 16))) 

data %>%  
  group_by(condition) %>%
  summarise_at(vars(mpg, cyl, wt), 
               funs(mean = mean, se=sd(.)/sqrt(n())))

data %>% 
  group_by(condition) %>%
  summarise_at(vars(mpg, cyl, wt), list(MEAN = ~ mean(.), 
                                        SE = ~sd(.)/sqrt(n()))) %>% 
  gather(key, val, -condition) %>% 
  separate(key, into = c("key1", "key2")) %>%        
  unite(cond, key2, condition, sep=".") %>% 
  spread(cond, val) %>%
  column_to_rownames('key1')
