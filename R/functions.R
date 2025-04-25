# any time I make a change, run the following: devtools::document(); devtools::install()

#' Summarize Continuous Variables
#'
#' This function calculates summary statistics (mean, minimum, maximum, and missing count) for specified continuous variables in a data frame. The results are returned in a long format with each statistic as a separate row.
#'
#' @param df A data frame containing the variables to be summarized.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to summarize (e.g., column names, column ranges, or selection helpers like contains()).
#' @return A data frame in long format with the summary statistics for each variable.
#' @importFrom dplyr ungroup summarise across arrange
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @export
check_continuous <- function(df, ...) {
  df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(across(c(...), list(
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
#' Returns a long-format tibble with value counts, including zeros and missing values.
#' If not assigned, prints a scrollable summary in the Viewer pane.
#'
#' @param df A data frame.
#' @param ... Unquoted tidyselect expressions.
#' @return A tibble (long-format). Printed as scrollable table if not assigned.
#' @importFrom dplyr select bind_rows arrange mutate
#' @importFrom sjlabelled get_labels remove_all_labels
#' @importFrom tibble tibble
#' @importFrom purrr map_dbl
#' @importFrom kableExtra kbl kable_styling scroll_box
#' @export
check_categorical <- function(df, ...) {
  selected_columns <- df %>%
    dplyr::select(...) %>%
    colnames()

  combined_results <- tibble::tibble()

  for (column in selected_columns) {
    x <- df[[column]]
    raw_vals <- sort(unique(sjlabelled::remove_all_labels(x)))
    labels <- sjlabelled::get_labels(x, drop.unused = FALSE)

    # Match label/value lengths
    if (length(labels) < length(raw_vals)) {
      labels <- c(labels, rep(NA, length(raw_vals) - length(labels)))
    } else if (length(labels) > length(raw_vals)) {
      raw_vals <- c(raw_vals, rep(NA, length(labels) - length(raw_vals)))
    }

    count_df <- tibble::tibble(
      name = column,
      value = raw_vals,
      label = labels,
      count = purrr::map_dbl(raw_vals, ~ sum(x == .x, na.rm = TRUE))
    )

    na_row <- tibble::tibble(
      name = column,
      value = NA,
      label = "No response",
      count = sum(is.na(x))
    )

    combined_results <- dplyr::bind_rows(combined_results, count_df, na_row)
  }

  out <- combined_results %>% dplyr::arrange(value)

  # Only show a scrollable HTML table if the user didn't assign the output
  if (interactive() && is.null(sys.calls()[[sys.nframe() - 1]])) {
    out %>%
      kableExtra::kbl(align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
      kableExtra::scroll_box(width = "100%", height = "400px") %>%
      print()
  }

  return(out)
}

#' Inspect Variable Labels
#'
#' This function retrieves the labels of specified variables in a data frame and displays them in a formatted table.
#'
#' @param df A data frame containing the variables.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to retrieve labels for (e.g., column names, column ranges, or selection helpers like contains()).
#' @return A formatted table displaying the variable labels.
#' @importFrom dplyr select
#' @importFrom sjlabelled get_label
#' @importFrom kableExtra kbl kable_styling column_spec row_spec scroll_box
#' @importFrom magrittr %>%
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


#' Inspect Variable Names and Column Numbers
#'
#' This function selects specified columns from a data frame and returns a formatted table with the original column numbers and variable names.
#'
#' @param df A data frame containing the variables.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to select (e.g., column names, column ranges, or selection helpers like contains()).
#' @return A formatted table displaying the original column numbers and variable names.
#' @importFrom dplyr select
#' @importFrom kableExtra kbl kable_styling column_spec row_spec scroll_box
#' @importFrom magrittr %>%
#' @export
inspect_var_names <- function(df, ...) {
  # Select the specified columns
  df_selected <- df %>% dplyr::select(...)

  # Get the column numbers from the original data frame using match()
  original_column_numbers <- match(names(df_selected), names(df))

  # Combine the original column numbers and variable names into a data frame
  labels_df <- data.frame(
    Column_Number = original_column_numbers,  # Use original column numbers
    Variable = names(df_selected),
    stringsAsFactors = FALSE
  )

  # Format the output with kableExtra
  formatted_output <- labels_df %>%
    kableExtra::kbl(centering = TRUE, align = c("c", "l")) %>%  # c = center for 1st column, l = left for 2nd column
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    kableExtra::column_spec(1, width = "auto", border_left = TRUE, border_right = TRUE) %>%  # Adjust width and borders for column 1
    kableExtra::column_spec(2, width = "auto", border_left = TRUE, border_right = TRUE) %>%  # Adjust width and borders for column 2
    kableExtra::row_spec(0, bold = TRUE, align = "center", extra_css = "border-bottom: 2px solid;") %>%  # Add bottom border to header row
    kableExtra::scroll_box(height = "400px", width = "100%")  # Set height for vertical scroll

  return(formatted_output)
}

#' Print a Random Slice of Data: HATCH
#'
#' This function selects and prints a random sample of 6 rows from specified columns in a data frame, formatted as a table.
#'
#' @param df A data frame containing the data.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to print (e.g., column names, column ranges, or selection helpers like contains()).
#' @return A formatted table displaying a random slice of the selected data.
#' @importFrom dplyr select ungroup slice_sample
#' @importFrom kableExtra kbl kable_styling row_spec scroll_box
#' @importFrom magrittr %>%
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

#' Print a Random Slice of Data (CHIRP)
#'
#' This function selects and prints a random sample of 6 rows from specified columns in a data frame, formatted as a table.
#'
#' @param df A data frame containing the data.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to print (e.g., column names, column ranges, or selection helpers like contains()).
#' @return A formatted table displaying a random slice of the selected data.
#' @importFrom dplyr select ungroup slice_sample
#' @importFrom kableExtra kbl kable_styling row_spec scroll_box
#' @importFrom magrittr %>%
#' @export
print_slice_chirp <- function(df, ...) {
  df %>%
    dplyr::select(c(ID, ...)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_sample(n = 6) %>%
    kableExtra::kbl(centering = TRUE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    kableExtra::row_spec(0:6, align = "center") %>%
    kableExtra::scroll_box(width = "100%")
}

#' View Specific Columns in Data Frame: HATCH
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

#' View Specific Columns in Data Frame: CHIRP
#'
#' This function displays a specified subset of columns in a data frame, including `CoupleID` and `Parent`.
#'
#' @param df A data frame containing the data.
#' @param ... One or more unquoted expressions separated by commas, indicating variables to view (e.g., column names, column ranges, or selection helpers like contains()).
#' @return Opens the specified columns in a new View window.
#' @importFrom dplyr select
#' @export
view_selected_chirp <- function(df, ...) {
  View(dplyr::select(df, ID, ...))
}
