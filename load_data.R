library(tidyverse)
library(janitor)

train_df <- read_csv("data/cs-training.csv")
test_df <- read_csv("data/cs-test.csv")




process_data <- function(data){
  data %>% 
    mutate(unknown_number_of_dependents = as.integer(is.na(number_of_dependents)),
           unknown_monthly_income = as.integer(is.na(monthly_income)),
           no_dependents = as.integer(number_of_dependents == 0)) %>% 
    replace_na(list(number_of_dependents = 0,no_dependents = 0)) %>% 
    mutate(no_income = as.integer(monthly_income == 0)) %>% 
    replace_na(list(no_income = 0,monthly_income = 0)) %>% 
    
    mutate(zero_debt_ratio = as.integer(debt_ratio == 0),
           unknown_income_debt_ratio = ifelse(!is.na(monthly_income),0,debt_ratio),
           debt_ratio = ifelse(is.na(monthly_income),0,debt_ratio)) %>%
    
    mutate(weird_revolving_utilization = 
             ifelse(log(revolving_utilization_of_unsecured_lines) > 3,
                    revolving_utilization_of_unsecured_lines,
                    0),
           zero_revolving_utilization = as.numeric(revolving_utilization_of_unsecured_lines ==0),
           revolving_utilization_of_unsecured_lines =
             ifelse(log(revolving_utilization_of_unsecured_lines) > 3,
                    0,revolving_utilization_of_unsecured_lines)) %>% 
    
    mutate(log_debt = log(pmax(monthly_income, 1) * debt_ratio),
           log_debt = ifelse(!is.finite(log_debt),0,log_debt),
           revolving_lines = number_of_open_credit_lines_and_loans - number_of_open_credit_lines_and_loans,
           has_revolving_lines = as.integer(revolving_lines > 0),
           has_real_estate_loans = as.integer(number_real_estate_loans_or_lines > 0),
           has_multiple_real_estate_loans = as.integer(number_real_estate_loans_or_lines > 2),
           elegible_ss = as.integer(age >60),
           dti_over_33 = as.integer(no_income == 0 & debt_ratio > 0.33),
           dti_over_43 = as.integer(no_income == 0 & debt_ratio > 0.43),
           disposable_income = ifelse(no_income == 1, 0, (1 - debt_ratio) * monthly_income),
           revolving_to_real_estate = revolving_lines / (1 + number_real_estate_loans_or_lines)) %>% 
    
    mutate(number_of_time30_59days_past_due_not_worse_large = as.integer(number_of_time30_59days_past_due_not_worse > 90),
           number_of_time30_59days_past_due_not_worse_96 = as.integer(number_of_time30_59days_past_due_not_worse == 96),
           number_of_time30_59days_past_due_not_worse_98 = as.integer(number_of_time30_59days_past_due_not_worse == 98),
           never_30_59_days_past_due_not_worse = as.integer(number_of_time30_59days_past_due_not_worse == 0),
           number_of_time30_59days_past_due_not_worse = ifelse(number_of_time30_59days_past_due_not_worse > 90,
                                                               0,
                                                               number_of_time30_59days_past_due_not_worse),
           
           number_of_time60_89days_past_due_not_worse_large = as.integer(number_of_time60_89days_past_due_not_worse > 90),
           number_of_time60_89days_past_due_not_worse_96 = as.integer(number_of_time60_89days_past_due_not_worse == 96),
           number_of_time60_89days_past_due_not_worse_98 = as.integer(number_of_time60_89days_past_due_not_worse == 98),
           never_60_89_days_past_due_not_worse = as.integer(number_of_time60_89days_past_due_not_worse == 0),
           number_of_time60_89days_past_due_not_worse = ifelse(number_of_time60_89days_past_due_not_worse > 90,
                                                               0,
                                                               number_of_time60_89days_past_due_not_worse),
           
           number_of_times90days_late_large = as.integer(number_of_times90days_late > 90),
           number_of_times90days_late_96 = as.integer(number_of_times90days_late == 96),
           number_of_times90days_late_98 = as.integer(number_of_times90days_late == 98),
           never_90days_late = as.integer(number_of_times90days_late == 0),
           number_of_times90days_late = ifelse(number_of_times90days_late > 90,
                                                               0,
                                                               number_of_times90days_late)) %>% 
    
    mutate(income_div_by_10 = as.integer(monthly_income %% 10 == 0),
           income_div_by_100 = as.integer(monthly_income %% 100 == 0),
           income_div_by_1000 = as.integer(monthly_income %% 1000 == 0),
           income_div_by_5000 = as.integer(monthly_income %% 5000 == 0)) %>% 
    
    mutate(weird_0999_utilization = as.integer(revolving_utilization_of_unsecured_lines == 0.9999999),
           full_utilization = as.integer(revolving_utilization_of_unsecured_lines == 1),
           excess_utilization = as.integer(revolving_utilization_of_unsecured_lines > 1),
           
           number_of_time30_89days_past_due_not_worse = number_of_time60_89days_past_due_not_worse + number_of_time30_59days_past_due_not_worse,
           number_of_times_past_due = number_of_time60_89days_past_due_not_worse + number_of_time30_59days_past_due_not_worse + number_of_times90days_late,
           never_past_due = as.integer(number_of_times_past_due == 0),
           log_revolving_utilization_times_lines = log1p(revolving_lines * revolving_utilization_of_unsecured_lines),
           log_revolving_utilization_of_unsecured_lines = log(revolving_utilization_of_unsecured_lines),
           log_revolving_utilization_of_unsecured_lines = ifelse(is.na(log_revolving_utilization_of_unsecured_lines)|is.infinite(log_revolving_utilization_of_unsecured_lines),
                                                                 0,
                                                                 log_revolving_utilization_of_unsecured_lines),
           
           delinquencies_per_line = ifelse(number_of_open_credit_lines_and_loans == 0,
                                           0,
                                           number_of_times_past_due / number_of_open_credit_lines_and_loans),
           major_delinquencies_per_line = ifelse(number_of_open_credit_lines_and_loans == 0,
                                                 0,
                                                 number_of_times90days_late / number_of_open_credit_lines_and_loans),
           minor_delinquencies_per_line = ifelse(number_of_open_credit_lines_and_loans == 0,
                                                 0,
                                                 number_of_time30_89days_past_due_not_worse / number_of_open_credit_lines_and_loans),
           
           delinquencies_per_revolving_line = ifelse(revolving_lines == 0,
                                                     0,
                                                     number_of_times_past_due / revolving_lines),
           major_delinquencies_per_revolivng_line = ifelse(revolving_lines == 0,
                                                           0,
                                                           number_of_times90days_late / revolving_lines),
           minor_delinquencies_per_revolving_line = ifelse(revolving_lines == 0,
                                                           0,
                                                           number_of_time30_89days_past_due_not_worse / revolving_lines)) %>% 
    
    mutate(log_debt_per_lines = log_debt - log1p(number_of_open_credit_lines_and_loans),
           log_debt_per_real_estate_lines = log_debt - log1p(number_real_estate_loans_or_lines),
           log_debt_per_person = log_debt - log1p(number_of_dependents),
           revolving_lines_per_person = revolving_lines / (1 + number_of_dependents),
           real_estate_loans_per_person = number_real_estate_loans_or_lines / (1 + number_of_dependents),
           years_of_age_per_dependent = age / (1 + number_of_dependents),
           log_monthly_income = ifelse(!is.finite(log(monthly_income))|is.na(log(monthly_income)),0,log(monthly_income)),
           
           log_income_per_person = log_monthly_income - log1p(number_of_dependents),
           log_income_age = log_monthly_income - log1p(age),
           
           log_number_of_times_past_due = ifelse(!is.finite(log(number_of_times_past_due)),
                                                 0,
                                                 log(number_of_times_past_due)),
           log_number_of_times90days_late = ifelse(!is.finite(log(number_of_times90days_late)),
                                                 0,
                                                 log(number_of_times90days_late)),
           log_number_of_time60_89days_past_due_not_worse = ifelse(!is.finite(log(number_of_time60_89days_past_due_not_worse)),
                                                 0,
                                                 log(number_of_time60_89days_past_due_not_worse)),
           log_number_of_time30_59days_past_due_not_worse = ifelse(!is.finite(log(number_of_time30_59days_past_due_not_worse)),
                                                 0,
                                                 log(number_of_time30_59days_past_due_not_worse)),
           
           log_ratio_90_to_30_59_days_late = log_number_of_times90days_late - log_number_of_time30_59days_past_due_not_worse,
           log_ratio_90_to_60_89_days_late = log_number_of_times90days_late - log_number_of_time60_89days_past_due_not_worse) %>% 
        
    mutate(any_open_credit_lines_or_loans = as.integer(number_of_open_credit_lines_and_loans > 0),
           log_number_of_open_credit_lines_and_loans = ifelse(!is.finite(log(number_of_open_credit_lines_and_loans)),
                                                              0,
                                                              log(number_of_open_credit_lines_and_loans)),
           log_number_of_open_credit_lines_and_loans_per_person = log_number_of_open_credit_lines_and_loans - log1p(number_of_dependents),
           
           has_dependents = as.integer(number_of_dependents > 0),
           log_household_size = log1p(number_of_dependents),
           log_debt_ratio = ifelse(!is.finite(log(debt_ratio)),
                                   0,
                                   log(debt_ratio)),
           log_debt_per_delinquency = log_debt - log1p(number_of_times_past_due),
           log_debt_per_90_days_late = log_debt - log1p(number_of_times90days_late),
           log_unknown_income_debt_ratio = ifelse(!is.finite(log(unknown_income_debt_ratio)),
                                                  0,
                                                  log(unknown_income_debt_ratio)),
           unkwown_income_debt_ratio_per_person = log_unknown_income_debt_ratio - log_household_size,
           unkwown_income_debt_ratio_per_line = log_unknown_income_debt_ratio - log1p(number_of_open_credit_lines_and_loans),
           unkwown_income_debt_ratio_per_real_estate_line = log_unknown_income_debt_ratio - log1p(number_real_estate_loans_or_lines),
           unkwown_income_debt_ratio_per_delinquency = log_unknown_income_debt_ratio - log1p(number_of_times_past_due),
           unkwown_income_debt_ratio_per_90_days_late = log_unknown_income_debt_ratio - log1p(number_of_times90days_late),
           
           log_number_real_estate_loans_or_lines = ifelse(!is.finite(log(number_real_estate_loans_or_lines)),
                                                  0,
                                                  log(number_real_estate_loans_or_lines)),
           low_age = as.integer(age < 18),
           log_age = log(age - 17),
           log_age = ifelse(low_age == 1, 0, log_age)) %>% 
    select(-revolving_utilization_of_unsecured_lines,
           -monthly_income,
           -number_of_dependents,
           -debt_ratio,
           -number_real_estate_loans_or_lines,
           -number_of_open_credit_lines_and_loans,
           -number_of_times_past_due,
           -number_of_times90days_late,
           -number_of_time30_59days_past_due_not_worse,
           -number_of_time60_89days_past_due_not_worse,
           -age)
}

train_df <- train_df %>% clean_names() %>% process_data()
test_df <- test_df %>% clean_names() %>% process_data()