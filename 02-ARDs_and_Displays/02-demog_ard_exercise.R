## Name: Demography ARD
#
# Input: adsl

# Load Libaries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Load source adsl AdAM dataset ----
adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt")

# demog ARD Generation ----

## Initialize by filtering out participants that pass safety
adsl_pop <- adsl %>%
  filter( SAFFL == "Y" )

## Create a new "sub" dataset, renaming all of "TRT01A" as "Total" - this allows creating a
## "Total" column in our dataset!
adsl_total <- adsl_pop %>%
  mutate(
    TRT01A = "Total"
  )

## Bind datasets together
adsl_trt_and_total <- bind_rows(adsl_pop, adsl_total)


# Exercise 1 --------------------------------------------------------------

## Let's add some variables we want. First we are going to start with AGE

### Lets create a function to generate continuous variable summaries. Update the
### "______" with the correct function!

ard_num_summary <- function(data, var, var_name){
  data %>%
    summarize(
      var = var_name,
      'n' = ______(!is.na({{var}})),
      'Mean' = ______({{var}}, na.rm = TRUE),
      'SD' = ______({{var}}, na.rm = TRUE),
      'Median' = ______({{var}}, na.rm = TRUE),
      'Q1' = ______({{var}}, .25, na.rm = TRUE),
      'Q3' = ______({{var}}, .75, na.rm = TRUE),
      'Min' = ______({{var}}, na.rm = TRUE),
      'Max' = ______({{var}}, na.rm = TRUE),
      'Missing' = ______(is.na({{var}}))
    ) %>%
    pivot_longer(
      cols = c(n:Missing),
      names_to = "param",
      values_to = "value"
    ) %>%
    mutate(
      label = case_when(
        param %in% c("Mean","SD") ~ "Mean (sd)",
        param %in% c("Q1","Q3") ~ "Q1, Q3",
        param %in% c("Min","Max") ~ "Min, Max",
        param %in% c("n") ~ "n",
        .default = stringr::str_to_title(param)
      )
    )
}

# Exercise 2 --------------------------------------------------------------

## Apply the function to adsl_trt_and_total, calculating for every `TRT01A` group
## the numeric summary results for `AGE`, and name the variable `Age (years)` to
## create `ard_demog_age_num`. Update the "______" with the correct values.

ard_demog_age_num <- adsl_trt_and_total %>%
  group_by(_____) %>%
  ard_num_summary(
    var = _____,
    var_name = ______
  )

# Exercise 3 --------------------------------------------------------------

### Lets now create a function for categorical variable summaries.

ard_cat_summary <- function(data, var, var_name){

  ## var totals
  var_vals <- data %>%
    reframe(
      var = var_name,
      label = c("n","Missing"),
      param = c("n"),
      value = c(sum(!is.na({{var}})), sum(is.na({{var}})))
    )

  cat_vals <- data %>%
    reframe(
      var = var_name,
      'label' = unique({{var}}),
      'n' = map_dbl(label, \(x, ref){sum(ref == x, na.rm = TRUE)}, {{var}}),
      'pct' = map_dbl(label, \(x, ref, n_values){sum(ref == x, na.rm = TRUE)/n_values}, {{var}}, sum(!is.na({{var}}))),
    ) %>%
    pivot_longer(
      cols = c(n, pct),
      names_to = "param",
      values_to = "value"
    )

  bind_rows(
    var_vals,
    cat_vals
  )

}

## Apply the categorical function to adsl_trt_and_total, calculating for every `TRT01A` group
## the categorial summary results for `AGEGR1`, and name the variable `Age (years)` to
## create `ard_demog_age_cat`. Update the "______" with the correct values.

ard_demog_age_cat <- adsl_trt_and_total %>%
  group_by(______) %>%
  ard_cat_summary(
    var = ______,
    var_name = ______
  )

# Exercise 4 --------------------------------------------------------------

# In this exercise, create the demographics Sex ARD called `ard_demog_sex`.
# Calculate the statistics for `SEX` with the label 'Sex' by treatment group and
# total.

## Hints: is this continuous or numeric? How do you identify the variable label?
## See above for comments

ard_demog_sex <- adsl_trt_and_total %>%
  ## add code here





# Exercise 5 --------------------------------------------------------------

# In this exercise, create the demographics baseline weight ARD called
# `ard_demog_wt_bl`. Calculate the statistics for  for `WEIGHTBL` with the label
# 'Weight (kg)' by treatment group and total.

## Hints: is this continuous or discrete? How do you identify the variable
## label? See above for comments

## Add Code Here






# Exercise 6 --------------------------------------------------------------

## Build the ARD from all the demography component ards
ard_demog <- bind_rows(
  ______,
  ______,
  ______,
  ______
)

## Save ARD file to "demog_ard.csv"
ard_demog %>%
  write_csv(file = "demog_ard.csv")



