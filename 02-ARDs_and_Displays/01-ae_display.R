## Name: Adverse Events ARD and Table
#
#
# Input: adae

#
library(haven)
library(dplyr)
library(tidyr)
library(tfrmt)
library(gt)

# Load source adae and adsl AdAM datasets ----
adae <- read_xpt("data/02-ARDs_and_Displays/adae.xpt")
adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt")

# AE ARD Generation ----

## Get total number of participants
big_n <- adsl %>%
  filter(SAFFL == "Y") %>%
  group_by(TRT01A) %>%
  summarize(
    Big_N = length(unique(USUBJID))
  ) %>%
  pivot_longer(
    cols = c(Big_N),
    names_to = "param",
    values_to = "value"
  )

## Implement ARDs in the form of a cake!
## Add layers to build out your ARD!

## Create an "Any Body System"
## Layer By overriding AETERM and AEBODSYS to a standard value, get the unique
## number of participants, total number of AEs, and pct of population

ard_ae_any <- adae %>%
  filter(SAFFL == "Y") %>%
  mutate(
    AETERM = "ANY BODY SYSTEM",
    AEBODSYS = "ANY BODY SYSTEM"
  ) %>%
  group_by(TRT01A, AETERM, AEBODSYS) %>%
  summarize(
    N = length(unique(USUBJID)), ## get total unique number of participants
    N_tot = n(), ## Get total number of entries
    N_pct = N/big_n[big_n$TRT01A == first(TRT01A) & big_n$param == "Big_N", "value"][[1]],
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(N, N_tot, N_pct),
    names_to = "param",
    values_to = "value"
  )

## Now for every individual AE Body System/AE Term, get the same parameters

ard_ae_all <- adae %>%
  filter(SAFFL == "Y") %>%
  group_by(TRT01A, AETERM, AEBODSYS) %>%
  summarize(
    N = length(unique(USUBJID)),
    N_tot = n(),
    N_pct = N/big_n[big_n$TRT01A == first(TRT01A) & big_n$param == "Big_N", "value"][[1]],
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(N, N_tot, N_pct),
    names_to = "param",
    values_to = "value"
  )

## Post Processing ARD
## Combine components together into usable format
## because we want to combine parameters together, create `sub_col_label`
## column defining "N_and_N_pct" as the combination of "N" and "N_pct"
## and "AEs" as "N_tot"
## Finally create ordering columns for AETERM (AETERM_ORD) and ABODSYS (AEBODSYS_ORD)

ae_ard_processed <- bind_rows(
  ard_ae_any,
  ard_ae_all
  ) %>%
  mutate(
    ## define n (%) and total AE cols
    sub_col_label  = case_when(
      param %in% c("N","N_pct") ~ 'N_and_N_pct',
      param %in% c("N_tot") ~ "AEs"
    )
  ) %>%
  mutate(
    AETERM_ORD = as.numeric(factor(AETERM, levels = c("ANY BODY SYSTEM", unique(adae$AETERM)))),
    AEBODSYS_ORD = as.numeric(factor(AEBODSYS, levels = c("ANY BODY SYSTEM", unique(adae$AEBODSYS))))
  ) %>%
  ungroup()

# Save ARD
ae_ard_processed %>%
  write_csv(file = "data/02-ARDs_and_Displays/answers/ae_ard.csv")

## Filter to remove cases where pct is less than 5% for all groups
ae_ard_filtered <- ae_ard_processed %>%
  arrange(AETERM_ORD, AEBODSYS_ORD) %>%
  group_by(AEBODSYS, AETERM) %>%
  mutate(
    keep_groups = any(value[param  == "N_pct"] > .05),
  ) %>%
  ungroup() %>%
  filter(
    keep_groups
  ) %>%
  select(-keep_groups)

# Build the AE Table ----

## Initialize AE tfrmt

## From columns in ae_ard_processed, what are the basics
ae_tfrmt <- tfrmt(
  group = AEBODSYS,
  label = AETERM,
  param = param,
  column = c(TRT01A, sub_col_label),
  value = value,
  sorting_cols = c(AETERM_ORD, AEBODSYS_ORD),
  title = "AE Table for CDISC Pilot Data",
  subtitle = "Source: adae, adsl"
)

print_to_gt(ae_tfrmt, .data = ae_ard_filtered)

## Define the body plan
ae_tfrmt <- ae_tfrmt %>%
  tfrmt(
    body_plan = body_plan(

      ## for n_pct column define format
      frmt_structure(
        group_val = ".default", # all groups
        label_val = ".default", # all labels
        frmt_combine(
          "{N} ({N_pct})", ## combine the N and N_pct params into a single value
          N = frmt("XX"),
          N_pct = frmt("x.x %", transform = ~.*100) ## percentages from ardis are out of 1, not 100
        )
      ),

      # for n_aes columns define format
      frmt_structure(
        group_val = ".default",  # all groups
        label_val = ".default", # all labels
        N_tot = frmt("[XXX]")
      )
    )
  )

print_to_gt(ae_tfrmt, .data = ae_ard_filtered)

## Define the row group plan
ae_tfrmt <- ae_tfrmt %>%
  tfrmt(
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location="indented")) ## indented is the default
  )

print_to_gt(ae_tfrmt, .data = ae_ard_filtered)


## Define column ordering

ae_tfrmt <- ae_tfrmt %>%
  tfrmt(
    col_plan = col_plan(
      ## For spanned columns we can define their order like this
      span_structure(
        TRT01A = c(starts_with("Xanomeline"), Placebo),
        sub_col_label = c("n (%)" = `N_and_N_pct` , "[AEs]" = `AEs`)
      ),

      ## Tidy select nomenclature works here!
      -ends_with("_ORD")
    )
  )

print_to_gt(ae_tfrmt, .data = ae_ard_filtered)


## Define column alignment

ae_tfrmt <- ae_tfrmt %>%
  tfrmt(
    col_style_plan = col_style_plan(

      # Tidyselect semantics
      col_style_structure(
        col = starts_with("Xanomeline"), ## all columns
        align = c("(","["), ## align on parenthesis and square braces
      ),

      # Another selection afterwards overrides previous styling
      col_style_structure(
        col = span_structure(TRT01A = "Placebo"), ## all placebo columns
        align = "right", ## align to right side of column
      )
    )
  )

print_to_gt(ae_tfrmt, .data = ae_ard_filtered)


### Save the table into a preferred format. since this is a gt, it can go to
### a wide variety of outputs, or be customized futher.

print_to_gt(ae_tfrmt, .data = ae_ard_filtered) %>%
  gtsave("ae_table.docx")

### Save the tfrmt as a json so we can potentially use it in the future.
print_to_json(ae_tfrmt, "ae_tfrmt.json")


