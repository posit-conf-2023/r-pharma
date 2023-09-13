#############################
# Create a teal app with a basic AE table using previous ARD exercise.
# For time reasons: let's stop right before "body plan" step.
# For time reasons: let's not use `quenv`, reporter feature etc.
#############################
library(teal)
library(haven)
library(dplyr)
library(tidyr)
library(tfrmt)
library(gt)

adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt")
adae <- read_xpt("data/02-ARDs_and_Displays/adae.xpt") 

app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adsl.xpt\")"),
    cdisc_dataset("ADAE", adae, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adae.xpt\")")
  ),
  modules = list(
    module(
      label = "AE Table",
      ui = function(id) {
        ns <- NS(id)
        gt_output(ns("table"))
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          
          ae_ard_big_n_r <- reactive({
            adsl <- data$ADSL()
            ## add code here!
            
          })
          
          ae_ard_any_r <- reactive({
            adae <- data$ADAE()
            ## add code here!
            ## use: ae_ard_big_n_r() 
            
          })
          
          ae_ard_all_r <- reactive({
            adae <- data$ADAE()
            ## add code here!
            ## use: ae_ard_big_n_r()

          })
          
          ae_ard_r <- reactive({
            ## add code here!
            ## use: ard_ae_any_r(), ard_ae_all_r()
          })

          ae_ard_processed_r <- reactive({
            ## add code here!
            ## use: ae_ard_r()
          })

          ae_ard_filtered_r <- reactive({
            ## add code here!
            ## use: ae_ard_processed_r()
          })

          ae_tfrmt_r <- reactive({
            ## add code here!
          })

          output$table <- render_gt({
            print_to_gt(ae_tfrmt_r(), .data = ae_ard_filtered_r())
          })
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)










#############################
# ANSWER:
#############################
library(teal)
library(haven)
library(dplyr)
library(tidyr)
library(tfrmt)
library(gt)

adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt")
adae <- read_xpt("data/02-ARDs_and_Displays/adae.xpt") 

app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adsl.xpt\")"),
    cdisc_dataset("ADAE", adae, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adae.xpt\")")
  ),
  modules = list(
    module(
      label = "AE Table",
      ui = function(id) {
        ns <- NS(id)
        gt_output(ns("table"))
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          
          ae_ard_big_n_r <- reactive({
            
            adsl <- data$ADSL()
            
            adsl %>%
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
            
          })
          
          ae_ard_any_r <- reactive({
            adae <- data$ADAE()
            big_n <- ae_ard_big_n_r()   
            
            adae %>%
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
            
          })
          
          ae_ard_all_r <- reactive({
            adae <- data$ADAE()
            big_n <- ae_ard_big_n_r()
            
            adae %>%
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
            
          })
          
          ae_ard_r <- reactive({
            bind_rows(
              ae_ard_any_r(),
              ae_ard_all_r()
            )
          })
          
          ae_ard_processed_r <- reactive({
            ae_ard_r() %>%
              mutate(
                ## define n (%) and total AE cols
                sub_col_label  = case_when(
                  param %in% c("N","N_pct") ~ 'N_and_N_pct',
                  param %in% c("N_tot") ~ "AEs"
                )
              ) %>%
              mutate(
                AETERM_ORD = as.numeric(factor(AETERM, levels = unique(c("ANY BODY SYSTEM", unique(AETERM))))),
                AEBODSYS_ORD = as.numeric(factor(AEBODSYS, levels = unique(c("ANY BODY SYSTEM", unique(AEBODSYS)))))
              ) %>%
              ungroup()
          })

          ae_ard_filtered_r <- reactive({
            ae_ard_processed_r() %>%
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
          })

          ae_tfrmt_r <- reactive({
            tfrmt(
              group = AEBODSYS,
              label = AETERM,
              param = param,
              column = c(TRT01A, sub_col_label),
              value = value,
              sorting_cols = c(AETERM_ORD, AEBODSYS_ORD),
              title = "AE Table for CDISC Pilot Data",
              subtitle = "Source: adae, adsl"
            )
          })

          output$table <- render_gt({
            print_to_gt(ae_tfrmt_r(), .data = ae_ard_filtered_r())
          })
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)
