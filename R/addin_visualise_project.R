
library(shiny)
library(miniUI)

addin_visualise_project <- function() {

  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("You must have RStudio v0.99.878 or newer to use the visualise project add-in",
         call. = FALSE)
  }

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniUI::miniPage(
    shinyjs::useShinyjs(),
    miniUI::gadgetTitleBar(span(strong("Function Network"),
                        span(id="author", "by", a(href="https://darkpeakanalytics.com/","Dark Peaks Analytics")))),

    miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
        miniContentPanel(
            # uiOutput("time"),
            shiny::textInput("project_path", "Project Path", value = ""),
            shiny::textInput("foo_path", "Foo Path", value = "R"),
            shiny::textInput("test_path", "Test Path", value = NULL),
            shiny::textInput("exclude_files", "Exclude Files (comma-separated)", value = NULL),
            shiny::textInput("exclude_dirs", "Exclude Dirs (comma-separated)", value = NULL),
            shiny::checkboxInput("run_coverage", "Run Coverage", value = FALSE),
            shiny::textInput("color_no_test_bg", "No Test Background Color", value = "#fad1d0"),
            shiny::textInput("color_no_test_border", "No Test Border Color", value = "#9c0000"),
            shiny::textInput("color_no_test_highlight", "No Test Highlight Color", value = "#9c0000"),
            shiny::textInput("color_with_test_bg", "With Test Background Color", value = "#e6ffe6"),
            shiny::textInput("color_with_test_border", "With Test Border Color", value = "#65a765"),
            shiny::textInput("color_with_test_highlight", "With Test Highlight Color", value = "#65a765"),
            shiny::textInput("color_mod_coverage_bg", "Moderate Coverage Background Color", value = "#FFD580"),
            shiny::textInput("color_mod_coverage_border", "Moderate Coverage Border Color", value = "#E49B0F"),
            shiny::textInput("color_mod_coverage_highlight", "Moderate Coverage Highlight Color", value = "#E49B0F"),
            shiny::sliderInput("moderate_coverage_range", "Moderate Coverage Range", min = 0, max = 1, value = c(0.2, 0.8)),
            shiny::checkboxInput("print_isolated_foo", "Print Isolated Foo", value = TRUE),
            shiny::checkboxInput("show_in_shiny", "Show in Shiny", value = FALSE),
            shiny::textInput("network_title", "Network Title", value = "Function Network"),
            shiny::checkboxInput("scale_node_size_by_degree", "Scale Node Size by Degree", value = TRUE),
            shiny::actionButton("visualize", "Visualise the Network")
            )
          ),

      miniTabPanel("Visualize", icon = icon("area-chart"),
        miniContentPanel(
          shiny::plotOutput("network_plot")
        )
        )
      )
    )

  server <- function(input, output, session) {

    # # Set some CSS styles for our clock.
    # clockStyles <- paste(
    #   "border: 1px solid #DADADA",
    #   "background-color: #EFEFEF",
    #   "border-radius: 5px",
    #   "font-size: 6em",
    #   "margin-top: 60px",
    #   "text-align: center",
    #   sep = "; "
    # )

    # We'll use a 'reactiveTimer()' to force Shiny
    # to update and show the clock every second.
    # invalidatePeriodically <- reactiveTimer(intervalMs = 1000)
    # observe({

      # Call our reactive timer in an 'observe' function
      # to ensure it's repeatedly fired.
    #   invalidatePeriodically()
    #
    #   # Get the time, and render it as a large paragraph element.
    #   time <- Sys.time()
    #   output$time <- renderUI({
    #     p(style = clockStyles, time)
    #   })
    # })

      shiny::observeEvent(input$visualize, {

        exclude_files <- ifelse(is.null(input$exclude_files), NULL, unlist(strsplit(input$exclude_files, ",")))
        exclude_dirs <- ifelse(is.null(input$exclude_dirs), NULL, unlist(strsplit(input$exclude_dirs, ",")))

        color_no_test <- c("background" = input$color_no_test_bg, "border" = input$color_no_test_border, "highlight" = input$color_no_test_highlight)
        color_with_test <- c("background" = input$color_with_test_bg, "border" = input$color_with_test_border, "highlight" = input$color_with_test_highlight)
        color_mod_coverage <- c("background" = input$color_mod_coverage_bg, "border" = input$color_mod_coverage_border, "highlight" = input$color_mod_coverage_highlight)

        visualise_project(
          project_path = input$project_path,
          foo_path = input$foo_path,
          test_path = input$test_path,
          exclude_files = exclude_files,
          exclude_dirs = exclude_dirs,
          run_coverage = input$run_coverage,
          color_no_test = color_no_test,
          color_with_test = color_with_test,
          color_mod_coverage = color_mod_coverage,
          moderate_coverage_range = input$moderate_coverage_range,
          print_isolated_foo = input$print_isolated_foo,
          show_in_shiny = input$show_in_shiny,
          network_title = input$network_title,
          scale_node_size_by_degree = input$scale_node_size_by_degree
        )
      })

    # Listen for 'done' and 'cancel' events.

    observeEvent(input$done, {
      stopApp()
    })

    observeEvent(input$cancel,{
      stopApp()
    })

  }


  ## Viewer ----

  viewer <- shiny::dialogViewer("Visualise Project Addin", width = 900, height = 800)
  shiny::runGadget(shiny::shinyApp(ui, server), viewer = viewer,
                   stopOnCancel = TRUE)
}

addin_visualise_project()
