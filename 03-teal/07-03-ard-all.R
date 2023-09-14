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
            )
          ),
          forms = verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      },
      server = function(id, data, reporter) {
        moduleServer(id, function(input, output, session) {
          qenv_r <- reactive({
            new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
              eval_code(
                substitute(
                  expr = {
                    trt_var <- as.name(x)
                  },
                  env = list(
                    x = input$trt_var
                  )
                )
              )
          })

          qenv_ae_ard_big_n_r <- reactive({
            qenv_r() %>%
              eval_code(
                quote({
                  big_n <- ADSL %>%
                    filter(SAFFL == "Y") %>%
                    group_by(!!trt_var) %>%
                    summarize(
                      Big_N = length(unique(USUBJID))
                    ) %>%
                    pivot_longer(
                      cols = c(Big_N),
                      names_to = "param",
                      values_to = "value"
                    )
                })
              )
          })

          qenv_ae_ard_any_r <- reactive({
            qenv_ae_ard_big_n_r() %>%
              eval_code(
                quote({
                  ae_ard_any <- ADAE %>%
                    filter(SAFFL == "Y") %>%
                    mutate(
                      AETERM = "ANY BODY SYSTEM",
                      AEBODSYS = "ANY BODY SYSTEM"
                    ) %>%
                    group_by(!!trt_var, AETERM, AEBODSYS) %>%
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
            qenv_ae_ard_any_r() %>%
              eval_code(
                quote({
                  ae_ard_all <- ADAE %>%
                    filter(SAFFL == "Y") %>%
                    group_by(!!trt_var, AETERM, AEBODSYS) %>%
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
            qenv_ae_ard_all_r() %>%
              eval_code(quote(
                ae_ard <- bind_rows(
                  ae_ard_any,
                  ae_ard_all
                )
              ))
          })

          qenv_ae_ard_processed_r <- reactive({
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

          qenv_ae_ard_filtered_r <- reactive({
            qenv_ae_ard_processed_r() %>%
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

          qenv_ae_tfrmt_r <- reactive({
            qenv_r() %>%
              eval_code(
                substitute(
                  expr = {
                    ae_tfrmt <- tfrmt(
                      group = AEBODSYS,
                      label = AETERM,
                      param = param,
                      column = vars(!!trt_var, sub_col_label),
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

          qenv_gt_table <- reactive({
            qenv_ae_ard_filtered_r() %>%
              join(qenv_ae_tfrmt_r()) %>%
              eval_code(
                quote({
                  x <- print_to_gt(ae_tfrmt, .data = ae_ard_filtered)
                })
              )
          })

          gt_table <- reactive({
            qenv_gt_table()[["x"]]
          })

          output$table <- render_gt({
            gt_table()
          })

          verbatim_popup_srv(
            id = "rcode",
            verbatim_content = reactive(get_code(qenv_gt_table())),
            title = "R Code"
          )

          # custom add card function
          card_fun <- function(card = ReportCard$new(), comment) {
            # please see ?teal.reporter::ReportCard for other append_ methods
            card$append_text("My plot", "header2")
            # card$append_html(as_raw_html(gt_table()))  ## <- not yet supported :(
            card$append_table(qenv_ae_ard_filtered_r()[["ae_ard_filtered"]])
            card$append_rcode(paste0(get_code(qenv_ae_ard_filtered_r()), collapse = "\n"))
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
