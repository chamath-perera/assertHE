
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
            uiOutput("time"),
            miniButtonBlock(
              actionButton("plot", "Visualise the Network")
            )
          ),

      miniTabPanel("Visualize", icon = icon("area-chart"),
        miniContentPanel()
        )
      )
    )
  )

  server <- function(input, output, session) {

    # Set some CSS styles for our clock.
    clockStyles <- paste(
      "border: 1px solid #DADADA",
      "background-color: #EFEFEF",
      "border-radius: 5px",
      "font-size: 6em",
      "margin-top: 60px",
      "text-align: center",
      sep = "; "
    )

    # We'll use a 'reactiveTimer()' to force Shiny
    # to update and show the clock every second.
    invalidatePeriodically <- reactiveTimer(intervalMs = 1000)
    observe({

      # Call our reactive timer in an 'observe' function
      # to ensure it's repeatedly fired.
      invalidatePeriodically()

      # Get the time, and render it as a large paragraph element.
      time <- Sys.time()
      output$time <- renderUI({
        p(style = clockStyles, time)
      })
    })

    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
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
                   stopOnCancel = FALSE)
}

# Try running the clock!
addin_visualise_project()
