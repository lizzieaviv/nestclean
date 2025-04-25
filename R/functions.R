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
  # 1. Build the summary & pivot, assign to out
  out <- df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(
      across(
        c(...),
        list(
          mean    = ~ round(mean(.x, na.rm = TRUE), 1),
          min     = ~ min(.x, na.rm = TRUE),
          max     = ~ max(.x, na.rm = TRUE),
          missing = ~ sum(is.na(.x))
        ),
        .names = "{.fn}-{.col}"
      )
    ) %>%
    tidyr::pivot_longer(
      cols      = everything(),
      names_to  = c("function", ".value"),
      names_sep = "-"
    )

  # 2. Blank out the first column header
  names(out)[1] <- ""

  # 3. Return the result
  out
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

    # 1. grab full label defs (codes → text), keep unused levels
    label_vec  <- sjlabelled::get_labels(x, drop.unused = FALSE)
    label_defs <- tibble::tibble(
      value = unname(label_vec),      # the numeric codes
      label = names(label_vec)        # the corresponding text
    )

    # 2. observed non-NA codes
    observed_vals <- sort(unique(sjlabelled::remove_all_labels(x)))

    # 3. union of observed + defined codes
    all_vals <- sort(unique(c(observed_vals, label_defs$value)))

    # 4. count each code (0 if absent)
    count_df <- tibble::tibble(
      name  = column,
      value = all_vals,
      count = purrr::map_dbl(all_vals, ~ sum(x == .x, na.rm = TRUE))
    )

    # 5. join in text labels, then add the NA row
    result <- count_df %>%
      dplyr::left_join(label_defs, by = "value") %>%
      dplyr::bind_rows(
        tibble::tibble(
          name  = column,
          value = NA_real_,
          label = "No response",
          count = sum(is.na(x))
        )
      ) %>%
      dplyr::arrange(value)

    combined_results <- dplyr::bind_rows(combined_results, result)
  }

  # 6. pivot to wide, filling missing with 0
  combined_results %>%
    tidyr::pivot_wider(
      names_from  = name,
      values_from = count,
      values_fill = list(count = 0)
    )
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
