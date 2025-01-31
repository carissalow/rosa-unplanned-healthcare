require(tidyverse)
require(lubridate)
require(RMariaDB)

#' Read a CSV or Excel and clean the column names
#'
#' @param file A CSV, XLS, or XSLX file
#' @param excluded_column_names A vector of strings containing column names to be excluded from output
#' @param exlcuded_column_patterns A regular expression  
#' @return A data frame
read_file_with_clean_names <- function(file, excluded_column_names = c(), excluded_column_patterns = NULL) {
  file_extension <- gsub(".*[\\..*$]", "", file)
  
  if (file_extension == "csv") {
    read_file_function <- readr::read_csv
  } else if (file_extension %in% c("xls", "xlsx")) {
    read_file_function <- readxl::read_excel
  } else {
    stop("File must be one of the following: csv, xls, xlsx")
  }
  
  data <- read_file_function(here::here(file))
  
  columns_to_exclude <- excluded_column_names
  if (!is.null(excluded_column_patterns)) {
    for (pattern in excluded_column_patterns) {
      columns_matching_pattern <- grep(pattern, colnames(data), value = TRUE)
      columns_to_exclude <- c(columns_to_exclude, columns_matching_pattern)
    }
  } 
  
  item_numbering_regex <- "^[1-9|a-z]\\d*[\\.|\\)] "
  parentheticals_regex <- " \\(e.g.,.*\\)"
  
  data <- data %>%
    # drop specified columns (exact names and/or pattern matches)
    select(-any_of(columns_to_exclude)) %>%
    # remove item numbering and parentheticals from column names
    rename_all(function(x) gsub(item_numbering_regex, "", x)) %>%
    rename_all(function(x) gsub(parentheticals_regex, "", x)) %>%
    janitor::clean_names()
  
  return(data)
}

#' Convert a wide-format dataset with one row per participant ID and columns for study start and end dates to long-format data set with rows for each participant ID and date in the study
#'
#' @param data A data frame
#' @param start_date_column A string with the name of the column containing the study start date for each participant
#' @param end_date_column A string with the name of the column containing the study end date for each participant
#' @param account_for_excluded_periods A logical flag indicating whether or not you need to account for periods between start and end date that should be dropped (e.g., because the participant paused participation)
#' @param excluded_period_start_date_column A string with the name of the column containing the excluded period start date for each participant
#' @param excluded_period_end_date_column A string with the name of the column containing the excluded period end date for each participant
#' @param excluded_period_offset An integer indicating the number of days by which the excluded period dates should be offset; default is 0
#' @return A data frame in long format with additional columns `date` and `study_day`
convert_date_to_study_day <- function(data, start_date_column, end_date_column, account_for_excluded_periods = FALSE, excluded_period_start_date_column = NULL, excluded_period_end_date_column = NULL, excluded_period_offset = 0) {
  if (!account_for_excluded_periods) {
    data <- data %>%
      mutate(date = list(clock::date_seq(from = get(start_date_column), to = get(end_date_column), by = 1))) 
  } else {
    data <- data %>%
      mutate(
        date = ifelse(
          !is.na(excluded_start_date) & !is.na(excluded_end_date),
          list(
            c(
              clock::date_seq(from = get(start_date_column), to = get(excluded_period_start_date_column) - excluded_period_offset, by = 1), 
              clock::date_seq(from = get(excluded_period_end_date_column) + excluded_period_offset, to = get(end_date_column), by = 1))
          ),
          list(clock::date_seq(from = get(start_date_column), to = get(end_date_column), by = 1))
        )
      )
  }
  data <- data %>%
    unnest(date) %>%
    mutate(study_day = dense_rank(date) - 1) 
  return(data)
}


#' Restrict data frame to subset of participants of interest
#'
#' @param data A data frame
#' @param participant_id_column A string containing the column name corresponding to the participant identifier
#' @param participants A vector of participant IDs 
#' @param include A logical flag indicating whether or not the IDs in the `participants` vector should be included or excluded from the output
#' @return A data frame
subset_participants <- function(data, participant_id_column, participants, include = TRUE) {
  if (include) {
    data <- filter(data, get(participant_id_column) %in% participants)
  }
  else {
    data <- filter(data, !(get(participant_id_column) %in% participants))
  }
  return(data)
}

#' Connect to a MySQL database
#' 
#' @param credentials A (nested) list of credentials containing user, password, host, port, and database for one or more groups
#' @param group A string with the group of credentials to use
#' @return A database connection
connect_to_database <- function(credentials, group) {
  credentials <- credentials[[group]]
  con <- dbConnect(
    RMariaDB::MariaDB(),
    user = credentials$user,
    password = credentials$password,
    host = credentials$host,
    port = credentials$port,
    dbname = credentials$database
  )
  return(con)
}

#' Pull ROSA daily survey data from MySQL database
#' 
#' @param con Database connection e.g., created by `connect_to_database()`
#' @param pids Vector of participant IDs to pull data for
#' @return Data frame containing daily survey data for specified participants 
pull_daily_survey_data <- function(con, pids = c()) {
  query <- "
    select 
      *,
	    case 
		    when id = '0015' then '1015'
		    else id
	    end as record_id
    from daily_survey_responses
    where id not in ('unknown', '');
  "
  data <- dbGetQuery(con, query)
  if (length(pids) > 0) {
    data <- data %>% filter(record_id %in% pids)
  }
  return(data)
}

#' Assign daily survey responses to a date based on clock time
#' 
#' @param data 
#' @param datetime_start_threshold
#' @param datetime_end_threshold
#' @return Data frame containing daily survey data assigned to dates based on specified times
assign_daily_surveys_to_date <- function(data, datetime_start_threshold = "00:00:00", datetime_end_threshold = "00:00:00") {
  datetime_start_threshold <- paste0("1970-01-01 ", datetime_start_threshold)
  datetime_end_threshold <- paste0("1970-01-01 ", datetime_end_threshold)
  
  data <- data %>%
    select(
      record_id, 
      response_id = ResponseId, 
      start_date = StartDate, 
      end_date = EndDate, 
      recorded_date = RecordedDate,
      progress = Progress, 
      duration_seconds = `Duration (in seconds)`, 
      starts_with("d_")
    ) %>%
    mutate(
      across(ends_with("_date"), function(x) as_datetime(x)),
      progress = as.numeric(progress),
      duration_seconds = as.numeric(duration_seconds),
      across(starts_with("d_"), function(x) as.numeric(x)) # an "NAs introduced by coercion" warning is fine/expected here
    ) %>%
    # assign surveys to dates
    mutate(
      time = format(start_date, "%H:%M:%S"),
      date = case_when(
        # if the time component of the start date is after midnight and before the end time threshold, we reassign to the previous day
        time > format(as_datetime(datetime_start_threshold), "%H:%M:%S") & time < format(as_datetime(datetime_end_threshold), "%H:%M:%S") ~ as.Date(date(start_date) - 1),
        TRUE ~ as.Date(start_date)
      )
    )
  
  return(data)
}

#' Clean and deduplicated daily survey responses
#' 
#' @param data Data frame containing ROSA daily survey data 
#' @param progress_threshold Integer representing the percentage progress at or above which a survey should be considered sufficiently complete; responses with progress < this threshold will be dropped. Default is 50
#' @param use_descriptive_names Logical indicating if the columns should be renamed with more descriptive and human-readable labels. Default is `TRUE` 
#' @param descriptive_name_mappings If `use_descriptive_names` is `TRUE`, a data frame containing 1:1 mapping of current and new names, with two columns called `current_column_name` and `new_column_name`, respectively
#' @param exclude_other_symptoms Logical indicating if the up to 6 "Other" symptoms participants could endorse should be excluded; default is `TRUE`
#' @return Data frame containing cleaned and de-duplicated daily survey data
clean_and_deduplicate_daily_surveys <- function(data, progress_threshold = 50, use_descriptive_names = TRUE, descriptive_name_mappings = NULL, exclude_other_symptoms = TRUE) {
  data <- data %>%
    # (1) drop insufficiently complete surveys 
    filter(progress >= progress_threshold) %>%
    # (2) drop duplicate surveys by retaining the first sufficiently complete survey per participant per day
    group_by(record_id, date) %>%
    filter(start_date == min(start_date)) %>%
    ungroup() %>%
    # (3) drop duplicates with the *same* timestamp by retaining the survey with the most non-missing items
    mutate(
      num_non_missing_vals = rowSums(across(starts_with("d_"), function(x) !is.na(x)))
    ) %>%
    group_by(record_id, date) %>%
    filter(num_non_missing_vals == max(num_non_missing_vals)) %>%
    # (4) if there are still duplicates, take the first one
    arrange(start_date) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(record_id, response_id, date, start_date, progress, duration_seconds, starts_with("d_")) 
  
  if (use_descriptive_names) {
    if (is.null(descriptive_name_mappings)) {
      stop("You must provide a dataframe with at least one row and two columns: current_column_name and new_column_name")
    } else {
      data <- data %>%
        rename_at(
          vars(descriptive_name_mappings$current_column_name), ~ descriptive_name_mappings$new_column_name
        )
    }
  }
  
  if (exclude_other_symptoms) {
    data <- data %>% select(-contains("other")) 
  }
  
  return(data)
}
