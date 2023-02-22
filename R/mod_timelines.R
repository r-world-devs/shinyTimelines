#' Timeline component UI function
#'
#' @param id Internal parameter for {shiny}.
#' @param pagination Default number of rows per page. Set NULL to turn off feature.
#' @param selected_visits Vector of "Treatment", "Lab" and "Vitals" value defining what visits should be initially shown.
#'   Set NULL to turn off feature.
#' @param followup_order TRUE/FALSE value to turn on/off followup date filtering. Set NULL to turn off feature.
#' @param lot_only Show patients having line of therapy only. Set NULL to turn off feature.
#'
#' @importFrom shiny NS tagList div span HTML p
#' @export
shiny_timelines_ui <- function(id, pagination = 20, selected_visits = FALSE, followup_order = FALSE, lot_only = FALSE){
  ns <- NS(id)

  tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "shinyTimelines/styles.css")
      )
    ),
    div(
      class = "mod-navigation-container",
      div(
        class = "mod-navigation",
        if (!is.null(pagination)) {
            div(
              class = "mod-navigation-tool",
              span("Patients per page", class = "navigation-label"),
              shinyWidgets::pickerInput(
                inputId  = ns("per_page"),
                label    = NULL,
                choices  = c("10", "20", "30", "40", "50"),
                selected = as.character(pagination),
                multiple = FALSE,
                width    = "100%",
                options  = shinyWidgets::pickerOptions(
                  size = 10
                )
              )
          )
        },
        if (!is.null(selected_visits)) {
          div(
            class = "mod-navigation-tool",
            span("Show visits", class = "navigation-label"),
            shinyWidgets::checkboxGroupButtons(
              inputId = ns("selected_visits"),
              label = NULL,
              choices = c("Treatment", "Lab", "Vitals"),
              selected = selected_visits,
              direction = "horizontal",
              justified  = FALSE,
              individual = FALSE,
              size       = "normal",
              width      = "100%",
              checkIcon = list(
                yes = span(
                  style = "color: steelblue;",
                  htmltools::tagAppendAttributes(icon("square-check"), class = " fa-solid")
                ),
                no  = span(style = "color: steelblue;", icon("square"))
              )
            )
          )
        },
        if (!is.null(followup_order)) {
          div(
            class = "mod-navigation-tool",
            span("Order by follow up time", class = "navigation-label"),
            shinyWidgets::materialSwitch(ns("followup_order"), label = NULL, value = followup_order)
          )
        },
        if (!is.null(lot_only)) {
          div(
            class = "mod-navigation-tool",
            span("Show patients with LOT only", class = "navigation-label"),
            shinyWidgets::materialSwitch(ns("lot_only"), label = NULL, value = lot_only)
          )
        },
        if (!is.null(pagination)) {
          div(
            class = "mod-navigation-tool",
            span(HTML("Selected page"), class = "navigation-label"),
            paginateUI(ns("page_selector"))
          )
        }
      )
    ),
    timevis::timevisOutput(ns("timeline_plot"), width = "100%", height = "auto"),
    p("Years since date of diagnosis (00)", class = "timeline-footer"),
    p("*Showing patients with existing index date and line of therapy only", class = "timeline-subfooter")
  )
}

#' Default column mapping used for timelines module.
#' @export
default_columns_mapping <- list(
  "single_case" = list(
    "demographics" = list(
      "patientid" = "patientid",
      "dateofdeath" = "dateofdeath",
      "lastseen" = "lastseen",
      "indexdate" = "diagnosisdate"
    ),
    "lineoftherapy" = list(
      "patientid" = "patientid",
      "linename" = "linename",
      "linenumber" = "linenumber",
      "startdate" = "startdate",
      "enddate" = "enddate"
    ),
    "visits" = list(
      "patientid" = "patientid",
      "visitdate" = "visitdate",
      "visittype" = "visittype"
    )
  ),
  "multiple_case" = list(
    "demographics" = list(
      "patientid" = "patientid",
      "dateofdeath" = "dateofdeath",
      "lastseen" = "lastseen"
    ),
    "lineoftherapy" = list(
      "patientid" = "patientid",
      "linename" = "linename",
      "linenumber" = "linenumber",
      "startdate" = "startdate",
      "enddate" = "enddate",
      "indication" = "indication"
    ),
    "visits" = list(
      "patientid" = "patientid",
      "visitdate" = "visitdate",
      "visittype" = "visittype",
      "indication" = "indication"
    ),
    "indications" = list(
      "patientid" = "patientid",
      "indication" = "indication",
      "indexdate" = "diagnosisdate"
    )
  )
)

#' Timeline component Server function
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param demographics Unevaluated reactive (`shiny::reactive`, `shiny::reactiveVal`)
#'   value containing demographics data.frame.
#' @param lineoftherapy Unevaluated reactive (`shiny::reactive`, `shiny::reactiveVal`)
#'   value containing lineoftherapy data.frame.
#' @param visits Unevaluated reactive (`shiny::reactive`, `shiny::reactiveVal`)
#'   value containing visits data.frame.
#' @param indications Unevaluated reactive (`shiny::reactive`, `shiny::reactiveVal`)
#'   value containing indications data.frame.
#' @param columns_mapping Columns mapping used for the module.
#' @param extra_data Named character vector (label - value pairs) of extra columns stored in demographics
#'   to be presented in timeline.
#' @param .list (optional) List storing `demographics`, `lineoftherapy`, `visits`, `indications` and `column_mapping`.
#'   Makes calling module easier in programmatic use (e.g. storing data as `reactiveValues`).
#' @export
shiny_timelines_server <- function(
  input, output, session,
  demographics = NULL, lineoftherapy = NULL, indications = NULL, visits = NULL,
  columns_mapping = default_columns_mapping, extra_data = NULL,
  .list = list()
){
  ns <- session$ns
  max_pages <- reactiveValues(value = NULL)
  cols_mapping <- reactiveValues(value = NULL)

  demographics_transformed <- reactive({
    demographics_data <- demographics %||% .list$demographics
    indications_data <- indications %||% .list$indications
    lot_data <- lineoftherapy %||% .list$lineoftherapy
    columns_mapping_val <- columns_mapping %||% .list$columns_mapping
    req(demographics_data)
    req(lot_data)

    lot_only <- input$lot_only
    if (is.null(lot_only)) {
      lot_only <- FALSE
    }

    transform_demographics(
      demographics_data, indications_data, lot_data, lot_only, columns_mapping_val
    )
  })

  lot_transformed <- reactive({
    indications_data <- indications %||% .list$indications
    lot_data <- lineoftherapy %||% .list$lineoftherapy
    columns_mapping_val <- columns_mapping %||% .list$columns_mapping
    req(lot_data)

    transform_lineoftherapy(indications_data, lot_data, columns_mapping_val)
  })

  visits_transformed <- reactive({
    visits_data <- visits %||% .list$visits
    indications_data <- indications %||% .list$indications
    columns_mapping_val <- columns_mapping %||% .list$columns_mapping
    req(visits_data)
    selected_visits <- input$selected_visits

    transform_visits(visits_data, indications_data, selected_visits, columns_mapping_val)
  })

  demographics_grouped <- reactive({
    req(demographics_transformed())
    followup_order <- input$followup_order
    if (is.null(followup_order)) {
      followup_order <- FALSE
    }
    n_patients <- length(unique(demographics_transformed()$patientid))
    max_pages$value <- ceiling(n_patients / as.numeric(input$per_page))
    calc_groups_and_followup(demographics_transformed(), followup_order)
  })

  pagination <- paginateServer("page_selector", max_pages)

  demographics_paged <- reactive({
    req(demographics_grouped())
    n_patients <- length(unique(demographics_grouped()$patientid))
    req(pagination$max == ceiling(n_patients / as.numeric(input$per_page)))

    page_subset(demographics_grouped(), pagination$current, per_page = as.numeric(input$per_page))
  })

  timeline_data <- reactive({
    req(demographics_paged())
    columns_mapping_val <- columns_mapping %||% .list$columns_mapping
    process_timelines_data(demographics_paged(), lot_transformed(), extra_data, columns_mapping_val)
  })

  output$timeline_plot <- timevis::renderTimevis({
    req(demographics_paged())

    timeline_data_value <- timeline_data()
    timeline_with_visits <- attach_visits_timeline(
      timeline_data_value, demographics_paged(), visits_transformed(), input$selected_visits
    )
    plot_timelines(timeline_with_visits$data, timeline_with_visits$groups)
  })
}
