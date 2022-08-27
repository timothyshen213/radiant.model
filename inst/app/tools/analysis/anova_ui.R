################################################################
# ANOVA - UI
################################################################
avar_sum_check <-c("Tukeys Confidence Intervals"="tukey", "Post-Hoc"="posthoc")
avar_testtype<-c("One Way"="one", "Two Way" = "two")
avar_model<-c("Model I" = "one","Model II" = "two", "Model III"="three")
avar_args <- as.list(formals(avar))

## list of function inputs selected by user
avar_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  avar_args$data_filter <- if (input$show_filter) input$data_filter else ""
  avar_args$dataset <- input$dataset
  for (i in r_drop(names(avar_args))) {
    avar_args[[i]] <- input[[paste0("avar_", i)]]
  }
  avar_args
})

# output$ui_avar_vars <- renderUI({
#   vars <- varnames()
#   toSelect <- .get_class() %in% c("numeric", "integer", "date", "factor")
#   vars <- vars[toSelect]
#   selectInput(
#     inputId = "avar_vars", label = "Treatments:", choices = vars,
#     selected = state_multiple("avar_vars", vars),
#     multiple = TRUE, size = min(15, length(vars)), selectize = FALSE
#   )
# })

output$ui_avar_treatment <- renderUI({
  vars <- varnames()
  toSelect <- .get_class() %in% c("numeric", "integer", "date", "factor")
  vars <- vars[toSelect]
  pickerInput(
    inputId="avar_treat",label="Treatment I:", choices=vars,
    multiple=FALSE,options=pickerOptions(liveSearch=TRUE, maxOptions = 1))
})

output$ui_avar_treatments <- renderUI({
  vars <- varnames()
  toSelect <- .get_class() %in% c("numeric", "integer", "date", "factor")
  vars <- vars[toSelect]
  pickerInput(
    inputId="avar_treats",label="Treatment II:", choices=vars,
    multiple=FALSE,options=pickerOptions(liveSearch=TRUE, maxOptions = 1, title="**ONLY FOR TWO WAY ANOVA MODEL**"))
})

output$ui_avar_response <- renderUI({
  vars <- varnames()
  toSelect <- .get_class() %in% c("numeric", "integer", "date", "factor")
  vars <- vars[toSelect]
  pickerInput(
    inputId="avar_response",label="Response:", choices=vars,
    multiple=FALSE)
})


## add a spinning refresh icon if the tabel needs to be (re)calculated
run_refresh(avar_args, "avar", init = "vars", label = "Run ANOVA", relabel = "Re-run ANOVA")

output$ui_avar <- renderUI({
  req(input$dataset)
  tagList(
      wellPanel(
        actionButton("avar_run", "Estimate model", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
      )
    ,
    wellPanel(
        selectInput(
          "avar_way",
          label = HTML("Type of ANOVA:"), choices = avar_testtype,
          selected = state_single("avar_way", avar_testtype, "two"), multiple = FALSE
        ),
        uiOutput("ui_avar_treatment"),
        uiOutput("ui_avar_treatments"),
        uiOutput("ui_avar_response")),
    wellPanel(
        conditionalPanel(
          condition = "input.avar_way == 'one'",
          checkboxGroupInput(
            "avar_sum_check", label="Comparison of Means:", avar_sum_check,
            selected = state_group("avar_sum_check"), inline = TRUE
          )
        ),
        conditionalPanel(
          condition = "input.avar_way == 'two'",
          radioButtons("avar_model",label=HTML("Model Type: <em> read below <em>"), choices = avar_model),
          # em("Model I (Fixed Effects Model):"), p("Assumes all factors/treatments are treated as fixed. "), br(),
          # em("Model II (Random Effects Model):"), p("Assumes all factors/treatments are treated as random (ie. Random Sampling). "), br(),
          # em("Model I (Mixed Effects Model):"), p("Assumes some factors are treated as random and some fixed. "), br(),
          checkboxGroupInput(
            "avar_sum_check", label="Comparison of Means:", avar_sum_check,
            selected = state_group("avar_sum_check"), inline = TRUE
          ),
          checkboxInput("avar_interaction", label="With Interaction", value=FALSE)
        )
    ),
    help_and_report(
      modal_title = "Analysis of Variance (ANOVA)",
      fun_name = "avar",
      help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/anova.md"))
    ))
})

## output is called from the main radiant ui.R
# output$summary_avar <- renderPrint({
#   .summary_avar()})


output$avar <- renderUI({
  register_print_output("summary_avar", ".summary_avar")

  avar_output_panels <- tabPanel(
      "Summary",
      # download_link("dl_km_means"), br(),
      verbatimTextOutput("summary_avar")
    )

  stat_tab_panel(
    menu = "Model > Estimate",
    tool = "Analysis of Variance (ANOVA)",
    tool_ui = "ui_avar",
    output_panels = avar_output_panels
  )
})

.avar <- eventReactive(input$avar_run, {
  req(input$avar_response)
  withProgress(message = "Beep...beep...comparing means!", value = 1, {
    avari <- avar_inputs()
    avari$envir <- r_data
    do.call(avar, avari)
  })
})

.summary_avar <- reactive({
  if (not_pressed(input$avar_run)) {
    "** Select two or more treatments to run ANOVA. Note both treatments must have the same response variable. **"
  } else {
    summary(.avar())
  }
})
avar_report <- function() {
  if (radiant.data::is_empty(input$avar_vars)) {
    return(invisible())
  }
  outputs <- c("summary")
  inp_out <- list("", "")
  inp_out[[1]] <- clean_args(avar_inputs(), avar_args[-1])
  figs <- FALSE

  update_report(
    inp_main = clean_args(avar_inputs(), avar_args),
    fun_name = "avar",
    inp_out = inp_out,
    outputs = outputs,
  )
}
observeEvent(input$avar_report, {
  r_info[["latest_screenshot"]] <- NULL
  avar_report()
})

observeEvent(input$avar_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_avar_screenshot")
})

observeEvent(input$modal_avar_screenshot, {
  avar_report()
  removeModal() ## remove shiny modal after save
})

