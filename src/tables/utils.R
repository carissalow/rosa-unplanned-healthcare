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

#' format names of predictors for plotting or tabling
#' @param x Character string column to format
#' @return Formatted character string 
format_predictors <- function(x) {
  x <- x %>%
    gsub("_", " ", .) %>%
    gsub(" symptom day$", "", .) %>%
    str_to_sentence(.)
  return(x)
}

#' format p-values per JMIR guidelines
#' @param val Numeric p-value
#' @return P-value as formatted character string 
format_p_values <- function(val) {
  p <- case_when(
    val < 0.001 ~ "<.001",
    val > 0.001 & val < 0.01 ~ as.character(round(val, 3)),
    val > 0.001 & val < 0.05 & round(val, 2) == 0.05 ~ as.character(round(val, 3)),
    val > 0.001 & round(val, 2) == 1.0 ~ ">.99",
    TRUE ~ as.character(round(val, 2))
  )
  p <- gsub("NA", "", p) 
  p <- gsub("^0", "", p)
  p <- ifelse(!is.na(nchar(p)) & nchar(p) == 2, paste0(p, "0"), p)
  
  return(p)
}

#' Format odds ratios per JMIR guidelines
#' #' @param val Numeric odds ratio or confidence limit
#' @return odds ratio as formatted character string 
format_odds_ratios <- function(val) {
  or <- as.character(round(val, 2))
  or <- ifelse(!is.na(nchar(or)) & nchar(or) == 1, paste0(or, ".00"), or)
  or <- ifelse(!is.na(nchar(or)) & nchar(or) == 3, paste0(or, "0"), or)
  return(or)
}

#' Format variable names for presentation in plots or tables
#' @param names String of variable name 
#' @return Formatted name 
format_variable_names <- function(names) {
  names <- names %>%
    str_to_sentence() %>%
    gsub("_", " ", .) %>%
    gsub(" collapsed", "", .) %>%
    gsub(" years", " (years, centered at mean)", .) %>%
    gsub("promis", "PROMIS", .) %>%
    gsub("preference", ", ", .) %>%
    gsub("PROMIS , ", "PROMIS,", .) %>%
    gsub("propr$", "PROPr", .) %>%
    gsub("cognition$", "Cognition", .) %>%
    gsub("depression$", "Depression", .) %>%
    gsub("^Forhp r", "R", .) %>%
    gsub("zipcode", "zip code", .) %>%
    gsub(" four", " 4", .) %>%
    gsub("Weekend day", "Weekend", .) %>%
    gsub("Days since last known chemotherapy treatment", "Time since last chemotherapy (days)", .)
  return(names)
}

#' Format GEE results to table
#' @param data Results of one or more models 
#' @param excluded_predictors Character vector of predictors to exclude; if empty, only "(Intercept)" will be excluded
#' @param adjusted Logical; set to TRUE if estimates are adjusted for covariates
#' @return data formatted for presentation in a table (with some further assistance)
format_gee_results_to_table <- function(data, excluded_predictors = c(), adjusted = FALSE, include_q = TRUE, include_anova = FALSE) {
  excluded_predictors <- c("(Intercept)", excluded_predictors)
  
  data_to_table <- data %>%
    filter(!(predictor %in% excluded_predictors)) %>%
    mutate(
      predictor = format_predictors(predictor),
      across(c("or", "or_ci_low", "or_ci_high"), format_odds_ratios),
      or_and_ci = ifelse(!is.na(or), paste0(or, " (", or_ci_low, ", ", or_ci_high, ")"), "*Reference*"),
      p_value = format_p_values(wald_p_value),
      anova_p_value = format_p_values(anova_p_value),
      across(contains("p_value"), function(x) ifelse(is.na(x), "", x))
    ) %>%
    select(
      n, n_obs, predictor, level, level_number, n_per_level, or_and_ci, wald_p_value, p_value, anova_p_value
    )
  
  if (include_q) {
    data_to_table <- data_to_table %>%
      mutate(
        q_value = format_p_values(p.adjust(wald_p_value, method = "fdr")),
        across(contains("q_value"), function(x) ifelse(is.na(x), "", x))
      )
  }
  
  if (adjusted) {
    data_to_table <- data_to_table %>%
      rename_at(vars(ends_with("_value")), function(x) paste0("adjusted_", x)) %>%
      rename_at(c("or_and_ci"), function(x) paste0("adjusted_", x))
  }
  
  if (include_anova) {
    data_to_table <- select(data_to_table, -contains("wald_p_value"))
  } else {
    data_to_table <- select(data_to_table, -contains("wald_p_value", "anova_p_value"))
  }
  
  return(data_to_table)
}
