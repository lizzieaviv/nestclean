#' Summarize Continuous Variables
#'
#' This function calculates summary statistics (mean, minimum, maximum, and missing count) for specified continuous variables in a data frame. The results are returned in a long format with each statistic as a separate row.
#'
#' @param df A data frame containing the variables to be summarized.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to summarize (e.g., column names, column ranges, or selection helpers like contains()).
#' @return A data frame in long format with the summary statistics for each variable.
#' @importFrom dplyr ungroup summarise across select arrange
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @export
check_continuous <- function(df, ...) {
  df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(across(dplyr::select(df, ...), list(
      mean = ~round(mean(.x, na.rm = TRUE), 1),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE),
      missing = ~sum(is.na(.x))
    ), .names = "{.fn}-{.col}")) %>%
    tidyr::pivot_longer(everything(),
                        names_to = c("function", ".value"),
                        names_sep = "-")
}

#' Summarize Categorical Variables
#'
#' This function calculates the frequency count for each unique value of the specified categorical variables in a data frame. The results include counts of missing values and are returned in a wide format.
#'
#' @param df A data frame containing the categorical variables to be summarized.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to summarize (e.g., column names, column ranges, or selection helpers like contains()).
#' @return A data frame in wide format with the frequency counts for each value of the specified variables.
#' @importFrom dplyr select bind_rows arrange
#' @importFrom tidyr pivot_wider
#' @importFrom sjlabelled get_labels remove_all_labels
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @export
check_categorical <- function(df, ...) {
  selected_columns <- df %>%
    dplyr::select(...) %>%
    colnames()

  combined_results <- tibble::tibble()

  for (column in selected_columns) {
    x <- df[[column]]
    value <- sort(unique(sjlabelled::remove_all_labels(x)))
    label <- sjlabelled::get_labels(x, drop.unused = TRUE)

    if (length(label) < length(value)) {
      label <- c(label, rep(NA, length(value) - length(label)))
    }

    count <- summary(as.factor(na.omit(x)))
    na_count <- sum(is.na(x))

    result <- tibble::tibble(
      name = column,
      value = c(value, NA),
      label = c(label, "No response"),
      count = c(as.numeric(count), na_count)
    )

    # Sort the result by value
    result <- result %>%
      dplyr::arrange(value)

    combined_results <- dplyr::bind_rows(combined_results, result)
  }

  combined_results %>%
    tidyr::pivot_wider(names_from = name, values_from = count, values_fill = list(count = 0))
}

#' Retrieve Variable Labels
#'
#' This function retrieves the labels of specified variables in a data frame and displays them in a formatted table.
#'
#' @param df A data frame containing the variables.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to retrieve labels for (e.g., column names, column ranges, or selection helpers like contains()).
#' @return A formatted table displaying the variable labels.
#' @importFrom dplyr select
#' @importFrom sjlabelled get_label
#' @importFrom kableExtra kbl kable_styling column_spec row_spec scroll_box
#' @export
inspect_labels <- function(df, ...) {
  df_selected <- df %>%
    dplyr::select(...)

  original_column_numbers <- match(names(df_selected), names(df))

  labels_df <- data.frame(
    Column_Number = original_column_numbers,
    Variable = names(df_selected),
    Label = unname(sjlabelled::get_label(df_selected, def.value = "unlabelled")),
    stringsAsFactors = FALSE
  )

  formatted_output <- labels_df %>%
    kableExtra::kbl(centering = TRUE, align = c("c", "l", "l")) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    kableExtra::column_spec(1, width = "auto", border_left = TRUE, border_right = TRUE) %>%
    kableExtra::column_spec(2, width = "auto", border_left = TRUE, border_right = TRUE) %>%
    kableExtra::column_spec(3, width = "auto", border_left = TRUE, border_right = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE, align = "center", extra_css = "border-bottom: 2px solid;") %>%
    kableExtra::scroll_box(height = "400px", width = "100%")

  return(formatted_output)
}

#' Print a Random Slice of Data
#'
#' This function selects and prints a random sample of 6 rows from specified columns in a data frame, formatted as a table.
#'
#' @param df A data frame containing the data.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to print (e.g., column names, column ranges, or selection helpers like contains()).
#' @return A formatted table displaying a random slice of the selected data.
#' @importFrom dplyr select ungroup slice_sample
#' @importFrom kableExtra kbl kable_styling row_spec scroll_box
#' @export
print_slice <- function(df, ...) {
  df %>%
    dplyr::select(c(CoupleID, Parent, ...)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_sample(n = 6) %>%
    kableExtra::kbl(centering = TRUE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    kableExtra::row_spec(0:6, align = "center") %>%
    kableExtra::scroll_box(width = "100%")
}

#' View Specific Columns in Data Frame
#'
#' This function displays a specified subset of columns in a data frame, including `CoupleID` and `Parent`.
#'
#' @param df A data frame containing the data.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to view (e.g., column names, column ranges, or selection helpers like contains()).
#' @return Opens the specified columns in a new View window.
#' @importFrom dplyr select
#' @export
view_selected <- function(df, ...) {
  View(dplyr::select(df, CoupleID, Parent, ...))
}
