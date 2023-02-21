paginateUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "shinyTimelines/paginate.css")
    ),
    shiny::div(
      id = id,
      class = "pagination-container",
      shiny::div(
        class = "pagination",
        shinyGizmo::conditionalJS(
          condition = "1 < input.current",
          ns = ns,
          jsCall = shinyGizmo::jsCalls$disable(when = FALSE),
          shiny::actionButton(ns("prev_page"), NULL, shiny::icon("angle-left"), class = "prev pager")
        ),
        shiny::tags$button(1, class = "min pager btn btn-default visibility-hidden"),
        shiny::selectInput(ns("current"), NULL, choices = "1") %>%
          htmltools::tagAppendAttributes(
            class = "current pager"
          ),
        shiny::numericInput(ns("max"), NULL, min = 1, value = 1) %>%
          htmltools::tagAppendAttributes(
            class = "max pager visibility-hidden"
          ),
        shinyGizmo::conditionalJS(
          condition = "input.max > input.current",
          ns = ns,
          jsCall = shinyGizmo::jsCalls$disable(when = FALSE),
          shiny::actionButton(ns("next_page"), NULL, shiny::icon("angle-right"), class = "next pager")
        )
      )
    )
  )
}

paginateServer <- function(id, max) {
  shiny::moduleServer(id, function(input, output, session) {
    value <- shiny::reactiveValues(current = 1, max = NULL)

    shiny::observeEvent(max$value, {
      value$current <- 1
      value$max <- max$value
      shiny::updateNumericInput(session = session, "max", value = value$max)
      shiny::updateSelectInput(
        session = session, "current", choices = as.character(seq_len(value$max))
      )
    })
    observeEvent(input$prev_page, {
      value$current <- value$current - 1
      shiny::updateSelectInput(session = session, "current", selected = as.character(value$current))
    })
    observeEvent(input$next_page, {
      value$current <- value$current + 1
      shiny::updateSelectInput(session = session, "current", selected = as.character(value$current))
    })
    observeEvent(input$current, {
      value$current <- as.numeric(input$current)
    })

    return(value)
  })
}
