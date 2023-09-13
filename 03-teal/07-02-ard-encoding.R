#############################
# Create a teal app with a basic AE table using previous ARD exercise.
# For time reasons: let's stop right before "body plan" step.
# For time reasons: let's not use `quenv`, reporter feature etc.
# Use encoding panel with following options:
# - title
# - subtitle
# - treatment arm column selector
# - AE body system column selector
# - AE term column selector
#############################
library(teal)
library(teal.widgets)
library(haven)
library(dplyr)
library(tidyr)
library(tfrmt)
library(gt)

adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt")
adae <- read_xpt("data/02-ARDs_and_Displays/adae.xpt")

app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl),
    cdisc_dataset("ADAE", adae)
  ),
  modules = list(
    module(
      label = "AE Table",
      ui = function(id) {
        ns <- NS(id)
        # helper function from teal.widgets
        standard_layout(
          output = div(
            gt_output(ns("table"))
          ),
          encoding = div(
            textInput(
              ns("title"),
              "Title",
              "AE Table for CDISC Pilot Data"
            ),
            textInput(
              ns("subtitle"),
              "Subtitle",
              "Source: adae, adsl"
            ),
            selectInput(
              ns("trt_var"),
              label = "Select treatment column",
              choices = c("ARM", "ARMCD", "TRT01A", "TRT01P"),
              selected = "TRT01A"
            ),
            selectInput(
              ns("ae_body_sys"),
              label = "Select AE Body System column",
              choices = c("AEBODSYS", "AEBODSYS_2"),
              selected = "AEBODSYS"
            ),
            selectInput(
              ns("ae_term"),
              label = "Select AE Term",
              choices = c("AETERM", "AETERM_2"),
              selected = "AETERM"
            )
          )
        )
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          # inputs
          trt_var_r <- reactive(as.name(input$trt_var))
          ae_body_sys_r <- reactive(as.name(input$ae_body_sys))
          ae_term_r <- reactive(as.name(input$ae_term))
          
          
          ae_ard_big_n_r <- reactive({
            adsl <- data$ADSL()
            
            ## add code here!
            ## use: treat_var = !!trt_var_r()            
            
          })
          
          ae_ard_any_r <- reactive({
            adae <- data$ADAE()

            ## add code here!
            ## use: ae_ard_big_n_r()
            ## use: treat_var = !!trt_var_r()
            ## use: target_var = vars(!!ae_body_sys_r(), !!ae_term_r())
            
            
          })
          
          ae_ard_all_r <- reactive({
            adae <- data$ADAE()
            
            ## add code here!
            ## use: ae_ard_big_n_r()
            ## use: treat_var = !!trt_var_r()
            ## use: target_var = vars(!!ae_body_sys_r(), !!ae_term_r())
            
          })

          ae_ard_r <- reactive({
            ## add code here!
            ## use: ae_ard_any_r(), ae_ard_all_r()
            
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
            ## use: input$title, input$subtitle
            
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
library(teal.widgets)
library(haven)
library(dplyr)
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
        # helper function from teal.widgets
        standard_layout(
          output = div(
            gt_output(ns("table"))
          ),
          encoding = div(
            textInput(
              ns("title"),
              "Title",
              "AE Table for CDISC Pilot Data"
            ),
            textInput(
              ns("subtitle"),
              "Subtitle",
              "Source: adae, adsl"
            ),
            selectInput(
              ns("trt_var"),
              label = "Select treatment column",
              choices = c("ARM", "ARMCD", "TRT01A", "TRT01P"),
              selected = "TRT01A"
            ),
            selectInput(
              ns("ae_body_sys"),
              label = "Select AE Body System column",
              choices = c("AEBODSYS", "AEBODSYS_2"),
              selected = "AEBODSYS"
            ),
            selectInput(
              ns("ae_term"),
              label = "Select AE Term",
              choices = c("AETERM", "AETERM_2"),
              selected = "AETERM"
            )
          )
        )
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          # inputs
          trt_var_r <- reactive(as.name(input$trt_var))
          ae_body_sys_r <- reactive(as.name(input$ae_body_sys))
          ae_term_r <- reactive(as.name(input$ae_term))
          
          ae_ard_big_n_r <- reactive({
            
            adsl <- data$ADSL()
            
            adsl %>%
              filter(SAFFL == "Y") %>%
              group_by(!!trt_var_r()) %>%
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
                !!ae_term_r() := "ANY BODY SYSTEM",
                !!ae_body_sys_r() := "ANY BODY SYSTEM"
              ) %>%
              group_by(!!trt_var_r(), !!ae_term_r(), !!ae_body_sys_r()) %>%
              summarize(
                N = length(unique(USUBJID)), ## get total unique number of participants
                N_tot = n(), ## Get total number of entries
                N_pct = N/big_n[big_n[[trt_var_r()]] == first(!!trt_var_r()) & big_n$param == "Big_N", "value"][[1]],
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
              group_by(!!trt_var_r(), !!ae_term_r(), !!ae_body_sys_r()) %>%
              summarize(
                N = length(unique(USUBJID)),
                N_tot = n(),
                N_pct = N/big_n[big_n[[trt_var_r()]] == first(!!trt_var_r()) & big_n$param == "Big_N", "value"][[1]],
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
              title = input$title,
              subtitle = input$subtitle
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
