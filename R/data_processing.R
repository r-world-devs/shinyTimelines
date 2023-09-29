transform_demographics <- function(demographics, indications, lot_data, lot_only, columns_mapping) {
  mult_indications <- !is.null(indications)

  case_type <- "single_case"
  if (mult_indications) {
    case_type <- "multiple_case"
    indications <- indications %>%
      map_rename("indications", case_type, columns_mapping)
  }

  demographics <- demographics %>%
    map_rename("demographics", case_type, columns_mapping)

  if (lot_only) {
    patients_with_lot <- unique(lot_data[!is.na(lot_data$startdate), ]$patientid)
    demographics <- demographics %>%
      dplyr::filter(patientid %in% !!patients_with_lot)
  }

  if (mult_indications) {
    demographics <- demographics %>%
      dplyr::left_join(indications, by = "patientid") %>%
      dplyr::filter(!is.na(indication))
  } else {
    demographics$indication <- "noindication"
  }

  demographics <- demographics %>%
    dplyr::filter(!is.na(indexdate))

  return(demographics)
}

transform_lineoftherapy <- function(indications, lot_data, columns_mapping) {
  mult_indications <- !is.null(indications)

  case_type <- "single_case"
  if(mult_indications){
    case_type <- "multiple_case"
  }
  lot_data <- lot_data %>%
    map_rename("lineoftherapy", case_type, columns_mapping)

  if(!mult_indications){
    lot_data$indication <- "noindication"
  }

  return(lot_data)
}

transform_visits <- function(visits, indications, selected_visits, columns_mapping) {
  mult_indications <- !is.null(indications)

  if(!is.null(selected_visits)) {
    case_type <- "single_case"
    if(mult_indications){
      case_type <- "multiple_case"
    }
    visits <- visits %>%
      map_rename("visits", case_type, columns_mapping)

    if (!mult_indications){
      visits$indication <- "noindication"
    }
  }

  return(visits)
}


make_date_relative <- function(start, end) {
  date_interval <- lubridate::time_length(
    lubridate::interval(start = lubridate::as_datetime(start), end = end)
  )

  lubridate::as_datetime("2000-01-01 00:00:00") + date_interval
}

rearrange_group <- function(data) {
  grouping_vars <- dplyr::group_vars(data)
  attr(data, "groups") <- dplyr::left_join(
    dplyr::ungroup(dplyr::distinct(data, dplyr::across(dplyr::any_of(grouping_vars)))),
    attr(data, "groups"),
    by = grouping_vars
  )
  return(data)
}

calc_groups_and_followup <- function(demographics, order) {
  demographics <- demographics %>%
    dplyr::mutate(
      follow_up_time = as.integer(dplyr::case_when(
        is.na(dateofdeath) ~ difftime(lastseen, indexdate, units = "days"),
        TRUE ~ difftime(dateofdeath, indexdate, units = "days")
      ))
    )

  demographics <- demographics %>%
    dplyr::group_by(patientid) %>%
    rearrange_group() %>%
    dplyr::mutate(patient_orig_no = dplyr::cur_group_id())

  if (identical(order, TRUE)) {
    demographics <- demographics %>%
      dplyr::mutate(max_followup = max(follow_up_time)) %>%
      dplyr::arrange(desc(max_followup), desc(follow_up_time)) %>%
      rearrange_group()
  }

  demographics <- demographics %>%
    dplyr::mutate(patient_no = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(patient_ind_no = 1:dplyr::n())

  return(demographics)
}

page_subset <- function(dataset, selected_page, per_page = 20) {
  n_rows <- nrow(dataset)

  if (is.null(selected_page)) {
    per_page <- n_rows
    selected_page <- 1
  }

  page_idx <- seq(
    (selected_page - 1) * per_page + 1,
    min(selected_page * per_page, n_rows),
    by = 1
  )

  return(
    dataset %>% dplyr::filter(patient_no %in% !!page_idx)
  )
}

init_timeline_data <- tibble::tibble(
  group = numeric(),
  start = lubridate::as_datetime(as.Date("1970-01-01")),
  end = lubridate::as_datetime(as.Date("1970-01-01")),
  type = character(),
  content = character(),
  title = character()
)

process_timelines_data <- function(demographics, lineoftherapy, extra_cols, columns_mapping) {

  multi_ind <- FALSE
  if (demographics$indication[1] != "noindication") {
    multi_ind <- TRUE
  }
  indexdate_col <- columns_mapping$single_case$demographics$indexdate

  if (multi_ind) {
    indexdate_col <- columns_mapping$multiple_case$indications$indexdate
    init_timeline_data <- dplyr::bind_rows(
      init_timeline_data,
      demographics %>%
        dplyr::transmute(
          group = patient_no,
          type = "background",
          content = indication,
          title = "Indication",
          className = "indication",
          start = lubridate::as_datetime("1999-01-01 00:00:00"),
          end = lubridate::as_datetime("1999-12-01 00:00:00")
        )
    )
  }

  data_to_plot <- dplyr::bind_rows(
    init_timeline_data,
    demographics %>%
      dplyr::transmute(
        group = patient_no,
        type = "point",
        content = " ",
        title = paste("Index date:", indexdate_col),
        className = "index-date",
        start = lubridate::as_datetime("2000-01-01 00:00:00")
      )
  )

  data_to_plot <- dplyr::bind_rows(
    data_to_plot,
    demographics %>%
      dplyr::left_join(
        lineoftherapy,
        by = c("patientid", "indication")
      ) %>%
      dplyr::group_by(patient_no, patient_ind_no, linenumber) %>%
      dplyr::summarise(
        linename = paste0(linename, collapse = ", "),
        indexdate = dplyr::first(indexdate),
        startdate = dplyr::first(startdate),
        enddate = dplyr::first(enddate),
      ) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        group = patient_no,
        type = "range",
        content = "",
        className = paste0("line line-", linenumber),
        title = paste0("Line ", linenumber, ": ", linename),
        start = make_date_relative(indexdate, startdate),
        end = make_date_relative(indexdate, enddate)
      )
  )

  data_to_plot <- dplyr::bind_rows(
    data_to_plot,
    demographics %>%
      dplyr::filter(!is.na(dateofdeath)) %>%
      dplyr::transmute(
        group = patient_no,
        type = "point",
        title = "Date of death",
        content = " ",
        className = "death",
        start = make_date_relative(indexdate, dateofdeath)
      )
  )

  data_to_plot <- dplyr::bind_rows(
    data_to_plot,
    demographics %>%
      dplyr::filter(is.na(dateofdeath) & !is.na(lastseen)) %>%
      dplyr::transmute(
        group = patient_no,
        type = "point",
        title = "Max activity",
        content = " ",
        className = "max-activity",
        start = make_date_relative(indexdate, lastseen)
      )
  )

  for (extra_col in extra_cols) {
    if (!extra_col %in% names(demographics)) {
      next
    }
    extra_col_name <- names(extra_cols[extra_cols == extra_col])
    data_to_plot <- dplyr::bind_rows(
      data_to_plot,
      demographics %>%
        dplyr::filter(!is.na(!!sym(extra_col))) %>%
        dplyr::transmute(
          group = patient_no,
          type = "point",
          title = extra_col_name,
          content = " ",
          className = "extra",
          start = make_date_relative(indexdate, lastseen)
        )
    )
  }

  groups <- demographics %>%
    dplyr::group_by(patient_no) %>%
    dplyr::summarise(id = patient_no, orig_no = patient_orig_no) %>%
    dplyr::mutate(content = paste("Patient", orig_no), className = "patient") %>%
    dplyr::select(-orig_no)

  return(list(data = data_to_plot, groups = groups))
}

attach_visits_timeline <- function(timelines_data, demographics, visits, selected_visits) {
  for (selected_visit_type in selected_visits) { # if null no iteration
    timelines_data$data <- dplyr::bind_rows(
      timelines_data$data,
      demographics %>%
        dplyr::left_join(
          visits %>% dplyr::filter(visittype == selected_visit_type),
          by = c("patientid", "indication")
        ) %>%
        dplyr::filter(!is.na(visitdate)) %>%
        dplyr::transmute(
          patientid = patientid,
          group = patient_no,
          type = "point",
          content = "",
          title = paste0("Visit type: ", visittype),
          className = paste0("visit visit-type-", selected_visit_type),
          start = make_date_relative(indexdate, visitdate)
        )
    )
  }
  return(timelines_data)
}

plot_timelines <- function(plot_data, plot_groups) {
  timevis::timevis(
    data = plot_data,
    groups = plot_groups,
    options = list(
      width  = '100%',
      margin = list(axis = 2, item = list(horizontal = 5, vertical   = 5)),
      stack = FALSE,
      stackSubgroups = FALSE,
      zoomMin = 10000000000,
      zoomMax = 10000000000000,
      orientation = "both",
      verticalScroll = TRUE,
      format = list(
        minorLabels = list(month = 'YY', year = 'YY'),
        majorLabels = list(month = 'YY', year = 'YY')
      ),
      showMajorLabels = FALSE,
      timeAxis = list(scale = 'year', step = 1),
      showCurrentTime = FALSE,
      showTooltips = TRUE
    ))
}
