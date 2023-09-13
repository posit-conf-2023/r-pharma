#############################
# No exercise - demo of everything putted all together
#############################
library(teal)
library(teal.widgets)
library(teal.code)
library(teal.reporter)
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
        standard_layout(
          output = div(
            gt_output(ns("table"))
          ),
          encoding = div(
            simple_reporter_ui(ns("reporter")),
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
          ),
          forms = verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      },
      server = function(id, data, reporter) {
        moduleServer(id, function(input, output, session) {
          # inputs
          trt_var_r <- reactive(as.name(input$trt_var))
          ae_body_sys_r <- reactive(as.name(input$ae_body_sys))
          ae_term_r <- reactive(as.name(input$ae_term))

          qenv_r <- reactive({
            new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
              eval_code(
                substitute(
                  expr = {
                    trt_var <- as.name(x)
                    ae_body_sys <- as.name(y)
                    ae_term <- as.name(z)
                  },
                  env = list(
                    x = input$trt_var,
                    y = input$ae_body_sys,
                    z = input$ae_term
                  )
                )
              )
          })

          qenv_ae_ard_big_n_r <- reactive({
            
            qenv_r() %>%
              eval_code(
                quote({
                  ADSL %>%
                    filter(SAFFL == "Y") %>%
                    group_by(!!trt_va) %>%
                    summarize(Big_N = length(unique(USUBJID))) %>%
                    pivot_longer(cols = c(Big_N),
                                 names_to = "param",
                                 values_to = "value")
                  })
                )
            
          })
          
          qenv_ae_ard_any_r <- reactive({
            
            qenv_r() %>%
              eval_code(
                quote({
                  
                  big_n <- qenv_ae_ard_big_n_r()   
            
                  ADAE %>%
                    filter(SAFFL == "Y") %>%
                    mutate(
                      !!ae_term := "ANY BODY SYSTEM",
                      !!ae_body_sys := "ANY BODY SYSTEM"
                    ) %>%
                    group_by(!!trt_var, !!ae_term, !!ae_body_sys) %>%
                    summarize(
                      N = length(unique(USUBJID)), ## get total unique number of participants
                      N_tot = n(), ## Get total number of entries
                      N_pct = N/big_n[big_n[[trt_var]] == first(!!trt_var) & big_n$param == "Big_N", "value"][[1]],
                      .groups = "drop"
                    ) %>%
                    pivot_longer(
                      cols = c(N, N_tot, N_pct),
                      names_to = "param",
                      values_to = "value"
                    )
                })
              )
          })
          
          qenv_ae_ard_all_r <- reactive({
            qenv_r() %>%
              eval_code(
                quote({
                  
                  big_n <- qenv_ae_ard_big_n_r()   
                  
                  ADAE %>%
                    filter(SAFFL == "Y") %>%
                    group_by(!!trt_var, !!ae_term, !!ae_body_sys) %>%
                    summarize(
                      N = length(unique(USUBJID)),
                      N_tot = n(),
                      N_pct = N/big_n[big_n[[trt_var]] == first(!!trt_var) & big_n$param == "Big_N", "value"][[1]],
                      .groups = "drop"
                    ) %>%
                    pivot_longer(
                      cols = c(N, N_tot, N_pct),
                      names_to = "param",
                      values_to = "value"
                    )
                })
              )
            
          })
          
          qenv_ae_ard_r <- reactive({
            qenv_r() %>%
              eval_code(quote(
                ae_ard <- bind_rows(
                  qenv_ae_ard_any_r(),
                  qenv_ae_ard_all_r()
                )
              ))
          })

          quenv_ae_ard_processed_r <- reactive({
            qenv_ae_ard_r() %>%
              eval_code(
                quote({
                  ae_ard_processed <- ae_ard %>%
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
              )
          })

          quenv_ae_ard_filtered_r <- reactive({
            quenv_ae_ard_processed_r() %>%
              eval_code(
                quote({
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
                })
              )
          })

          quenv_ae_tfrmt_r <- reactive({
            new_qenv() %>%
              eval_code(
                substitute(
                  expr = {
                    ae_tfrmt <- tfrmt(
                      group = AEBODSYS,
                      label = AETERM,
                      param = param,
                      column = c(TRT01A, sub_col_label),
                      value = value,
                      sorting_cols = c(AETERM_ORD, AEBODSYS_ORD),
                      title = title,
                      subtitle = subtitle
                    )
                  },
                  env = list(
                    title = input$title,
                    subtitle = input$subtitle
                  )
                )
              )
          })

          quenv_gt_table <- reactive({
            quenv_ae_ard_filtered_r() %>%
              join(quenv_ae_tfrmt_r()) %>%
              eval_code(
                quote({
                  x <- print_to_gt(ae_tfrmt, .data = ae_ard_filtered)
                })
              )
          })

          gt_table <- reactive({
            quenv_gt_table()[["x"]]
          })

          output$table <- render_gt({
            gt_table()
          })

          verbatim_popup_srv(
            id = "rcode",
            verbatim_content = reactive(get_code(quenv_gt_table())),
            title = "R Code"
          )

          # custom add card function
          card_fun <- function(card = ReportCard$new(), comment) {
            # please see ?teal.reporter::ReportCard for other append_ methods
            card$append_text("My plot", "header2")
            # card$append_html(as_raw_html(gt_table()))  ## <- not yet supported :(
            card$append_table(quenv_ae_ard_filtered_r()[["ae_ard_filtered"]])
            card$append_rcode(paste0(get_code(qenv_r()), collapse = "\n"))
            if (!comment == "") {
              card$append_text("Comment", "header3")
              card$append_text(comment)
            }
            card
          }
          # execute server part of the module with injected custom add card function
          simple_reporter_srv("reporter", reporter = reporter, card_fun = card_fun)
        })
      }
    )
  )
)

shinyApp(app$ui, app$server)
