################################################################
# ANOVA - UI
################################################################
avar_sum_check <-c("Tukeys Confidence Intervals"="tukey", "Post-Hoc"="posthoc", "Diagnostic" = "diag")
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
    inputId="avar_treattwo",label="Treatment II:", choices=vars,
    multiple=FALSE,options=pickerOptions(liveSearch=TRUE, maxOptions = 1))
})

output$ui_avar_response <- renderUI({
  vars <- varnames()
  toSelect <- .get_class() %in% c("numeric", "integer", "date", "factor")
  vars <- vars[toSelect]
  pickerInput(
    inputId="avar_response",label="Response:", choices=vars,
    multiple=FALSE)
})

output$ui_avar_m3 <- renderText({"<em> Treatment I is the fixed factor. Treatment II is the random factor"
})

avar_plot <- reactive({
  plots <- input$avar_plots
  req(plots)
  ph <- plots %>%
    {
      if (length(.) == 1 && . == "dendro") 800 else 400
    }
  pw <- 650
  list(plot_width = pw, plot_height = ph * length(plots))
})

avar_plot_width <- function() {
  avar_plot() %>%
    {
      if (is.list(.)) .$plot_width else 650
    }
}

avar_plot_height <- function() {
  avar_plot() %>%
    {
      if (is.list(.)) .$plot_height else 400
    }
}

## add a spinning refresh icon if the tabel needs to be (re)calculated
run_refresh(avar_args, "avar", init = "vars", label = "Run ANOVA", relabel = "Re-run ANOVA")
output$plot_pca<-renderPlot({
  .plot_pca()})
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
          selected = state_single("avar_way", avar_testtype, "one"), multiple = FALSE
        ),
        uiOutput("ui_avar_treatment")),
    wellPanel(
        conditionalPanel(
          condition = "input.avar_way == 'two'",
          uiOutput("ui_avar_treatments")
          ),
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
          radioButtons("avar_model",label=HTML("Model Type:"), choices = avar_model),
          conditionalPanel(
            condition = "input.avar_model == 'three'",
            uiOutput("ui_avar_m3")),
          conditionalPanel(
            condition = "input.avar_model == 'one'",
            checkboxInput("avar_interaction", label="With Interaction", value=TRUE)
        )),
        conditionalPanel(
          condition = "input.avar_way == 'two'",
          checkboxInput("avar_plots", label="Interaction Plot", value = FALSE)
        ),
        conditionalPanel(
          condition = "input.avar_way == 'two'",
          checkboxGroupInput(
            "avar_sum_check", label="Comparison of Means:", avar_sum_check,
            selected = state_group("avar_sum_check"), inline = TRUE
          ))),
    help_and_report(
      modal_title = "Analysis of Variance (ANOVA)",
      fun_name = "avar",
      help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/anova.md"))
    ))
})

## output is called from the main radiant ui.R
# output$summary_avar <- renderPrint({
#   .summary_avar()})

output$plot_avar<-renderPlot({
  .plot_avar()})

output$avar <- renderUI({
  register_print_output("summary_avar", ".summary_avar")
  register_plot_output(
    "plot_avar", ".plot_avar",
    width_fun = "avar_plot_width",
    height_fun = "avar_plot_height"
  )

  avar_output_panels <- tagList(
    tabPanel(
      "Summary",
      # download_link("dl_avar"), br(),
      verbatimTextOutput("summary_avar")
    ),
    tabPanel(
      "Plot",
      # download_link("dl_avar_plot"), br(),
      plotOutput("plot_avar", width = "100%", height = "100%")
    )
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
    "ANALYSIS OF VARIANCE (ANOVA) \n\nSelect 'One-Way' or 'Two-Way' ANOVA to begin.\nThe analysis requires the type of response variable to be numeric.\nNote: the treatment variables will be treated as factors. \nIf these variable types are not available please select another dataset.\n\n" %>%
      suggest_data("diamonds")
  } else {
    summary(.avar())
  }
})

.plot_avar <- eventReactive(
  {
    c(input$avar_run, input$avar_plots)
  },
  {withProgress(
    message = "Generating cluster plot", value = 1,
    capture_plot(plot(.avar(), plots = input$avar_plots))
  )
  }
)

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

