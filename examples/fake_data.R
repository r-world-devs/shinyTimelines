library(magrittr)

simul_line_base <- function(n, line_base, ...) {
  linebase <- line_base[unique(sample(1:length(line_base), length(line_base), TRUE))]
  res <- paste0(linebase, collapse = ",")
  rep(res, n)
}

calc_startend_periods <- function(n, lastseen, linenumber) {
  max_end_date <- max(as.Date(lastseen))
  min_start_date <- max_end_date - sample(365:(5*365), 1)
  dates_sample <- sort(sample(seq(min_start_date, max_end_date, by = "day"), 2 * n))

  end_dates <- dates_sample[2*(1:n)]
  start_dates <- dates_sample[-2*(1:n)]

  return(paste0(start_dates, ",", end_dates))
}

gen_visit <- function(n, startend) {
  date_ranges <- as.Date(unlist(strsplit(startend, ",")))
  min_date <- min(date_ranges)
  max_date <- max(date_ranges)
  sample(seq(min_date, max_date, by = "day"), n, TRUE)
}

DataFakeR::set_faker_opts(
  opt_default_table = DataFakeR::opt_default_table(nrows = DataFakeR::nrows_simul_ratio(0.25, 10000))
)

sch <- DataFakeR::schema_source("schema.yml")
set.seed(123)
sch <- DataFakeR::schema_simulate(sch)

datasets_fake <- list(
  demographics = DataFakeR::schema_get_table(sch, "demographics") %>%
    dplyr::select(-dplyr::starts_with("tmp")) %>%
    dplyr::mutate(random_date = lastseen - sample(365 * 1:5, 1)),
  visit = DataFakeR::schema_get_table(sch, "visit") %>%
    dplyr::select(-dplyr::starts_with("tmp")),
  indications = DataFakeR::schema_get_table(sch, "indications") %>%
    dplyr::select(-dplyr::starts_with("tmp")) %>% dplyr::distinct(),
  line_of_therapy = DataFakeR::schema_get_table(sch, "line_of_therapy") %>%
    dplyr::select(-dplyr::starts_with("tmp"))
)

library(shiny)

ui <- fluidPage(
  shinyTimelines::shiny_timelines_ui("timeln")
)

server <- function(input, output, session) {
  mod_data <- reactiveValues(
    demographics = datasets_fake$demographics,
    lineoftherapy = datasets_fake$line_of_therapy,
    visits = datasets_fake$visit,
    indications = datasets_fake$indications,
  )
  callModule(
    shinyTimelines::shiny_timelines_server,
    id = "timeln",
    session = session,
    extra_data = c("Extra col" = "random_date"),
    columns_mapping = list(
      "single_case" = list(
        "demographics" = list("patientid" = "patientid",
                              "dateofdeath" = "dateofdeath",
                              "lastseen" = "lastseen",
                              "indexdate" = "diagnosisdate"),
        "lineoftherapy" = list("patientid" = "patientid",
                               "linename" = "linename",
                               "linenumber" = "linenumber",
                               "startdate" = "startdate",
                               "enddate" = "enddate"),
        "visits" = list("patientid" = "patientid",
                        "visitdate" = "visitdate",
                        "visittype" = "visittype")
      ),
      "multiple_case" = list(
        "demographics" = list("patientid" = "patientid",
                              "dateofdeath" = "dateofdeath",
                              "lastseen" = "lastseen",
                              "random_date"= "random_date"),
        "lineoftherapy" = list("patientid" = "patientid",
                               "linename" = "linename",
                               "linenumber" = "linenumber",
                               "startdate" = "startdate",
                               "enddate" = "enddate",
                               "indication" = "indication"),
        "visits" = list("patientid" = "patientid",
                        "visitdate" = "visitdate",
                        "visittype" = "visittype",
                        "indication" = "indication"),
        "indications" = list("patientid" = "patientid",
                             "indication" = "indication",
                             "indexdate" = "diagnosisdate")
      )
    ),
    .list = mod_data
  )
}

shinyApp(ui, server)

