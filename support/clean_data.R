#| Function: clean_data
#| -------------------------------------------------------------------------
#| Description: 
#|   Cleans and transforms a given data frame by pivoting it longer based on
#|   columns starting with "Replicate," cleaning column names, and adding
#|   log-transformed columns for dose and response.
#|
#| Parameters:
#|   - src: Input data frame (default is the variable 'src').
#|
#| Returns:
#|   A cleaned and transformed data frame.
#|
#| Usage:
#|   clean_data(src = my_data_frame)
#|   # or
#|   clean_data()  # Assumes 'src' is defined or defaults to variable 'src'.
#| -------------------------------------------------------------------------
#| @param src: Input data frame (default is the variable 'src').
#| -------------------------------------------------------------------------
#| @return: A cleaned and transformed data frame.
#| -------------------------------------------------------------------------
#| @examples:
#|   clean_data(src = my_data_frame)
#|   # or
#|   clean_data()  # Assumes 'src' is defined or defaults to variable 'src'.
#| -------------------------------------------------------------------------
#| @importFrom: dplyr %>%
#| @importFrom: tidyr pivot_longer
#| @importFrom: janitor clean_names
#| @importFrom: base log2
#| -------------------------------------------------------------------------



#### Load Required Packages ####
# Check if the 'pacman' package is installed; if not, install it
if (!require("pacman")) install.packages("pacman")

# Load multiple R packages using 'pacman::p_load'
pacman::p_load(
  tidyverse,    # A collection of packages for data manipulation and visualization
  janitor       # Provides convenient functions for cleaning and tidying data
)

# Define a function 'clean_data' with an optional 'src' argument
clean_data <- function(src = src){
  
  # Pivot the data frame longer based on columns starting with "Replicate"
  df <- src %>% 
    pivot_longer(
      .,
      cols = starts_with("Replicate"),
      names_to = "replicate",
      names_prefix = "Replicate",
      values_to = "response",
      values_drop_na = TRUE
    ) %>% 
    
    # Clean column names
    clean_names() %>% 
    
    # Add two new columns with log-transformed values
    mutate(
      log_dose = log2(dose),
      log_response = log2(response),
      material_type = case_when(
        type == "Standard" ~ 0,
        TRUE ~ 1
      )
    ) %>% 
    arrange(plate, type, desc(dose))
  
  # Return the cleaned and transformed data frame
  return(df)
}
