#### Load Required Packages ####
# Check if the 'pacman' package is installed; if not, install it
if (!require("pacman")) install.packages("pacman")

# Load multiple R packages using 'pacman::p_load'
pacman::p_load(
  tidyverse,    # A collection of packages for data manipulation and visualization
  ggplot2,      # A popular package for creating static and dynamic graphics
  cowplot,      # Extension of ggplot2 for complex plot layout
  scales,       # Provides functions for scaling data
  here,         # Simplifies file paths and project organization
  purrr,        # Functional programming tools for working with lists and vectors
  viridis,      # Color maps for use with ggplot2
  drc,          # Analysis of dose-response curves
  minpack.lm    # Levenberg-Marquardt nonlinear least-squares optimization
)

# Source the 'clean_data.R' script to incorporate functions or variables
source("support/clean_data.R")
source("support/FourPL.R")


##### Define variables
# Combine the current project directory with a subdirectory and a specific file name
file_path <- paste0(
  here(),        # 'here()' returns the path to the current project directory
  "/data/",      # Subdirectory named "data/"
  "data.csv"     # File name "data.csv"
)

#### Read Data ####
# Read a CSV file into a data frame
src <- read_csv(
  file_path,
  show_col_types = FALSE
)

#### Clean Data ####
df <- clean_data(src)

#### Fit Model(s) ####
# Set plate_number and type_number variables to specify plate and material type
plate_number <- 1
type_number <- 1

# Nonlinear least squares fitting using nlsLM function
model <- nlsLM(
  formula = response ~ Amin + ((Amax - Amin) / (1 + 2^(-B * (log_dose - log_EC50 + E * material_type)))),
  data = df %>% filter(plate == plate_number),  # Filter data for the specified plate
  start = list(
    Amin = 0,
    Amax = max(df$response),  # Set upper limit of Amax to the maximum response in the dataset
    log_EC50 = 2,
    B = -1,
    E = 1
  )
)


# Generate a summary of the nonlinear least squares model ('model')
model_summary <- summary(model)

# Extract the coefficients from the model summary
model_coefficients <- model_summary$coefficients



#### Plot Model(s) ####
# Create an empty data frame to store the restricted model information
restricted_model_df <- data.frame()

# Extract the 'type' corresponding to the specified 'plate_number'
type <- df %>% 
  filter(plate == plate_number) %>% 
  distinct(type) %>% 
  pull(type)

# Extract coefficients from the previously fitted model
coefficients <- model_summary$coefficients

# Create a data frame 'dat' with information related to the restricted model
dat <- data.frame(
  plate = plate_number,
  numerator = types[type_number],
  denominator = "Standard",
  type = type[type_number],
  group = paste0(type[type_number], "_", "Standard"),
  Amin = coefficients["Amin", "Estimate"],
  Amax = coefficients["Amax", "Estimate"],
  log_EC50 = coefficients["log_EC50", "Estimate"],
  B = coefficients["B", "Estimate"],
  E = coefficients["E", "Estimate"],
  Amin_se = coefficients["Amin", "Std. Error"],
  Amax_se = coefficients["Amax", "Std. Error"],
  log_EC50_se = coefficients["log_EC50", "Std. Error"],
  B_se = coefficients["B", "Std. Error"],
  E_se = coefficients["E", "Std. Error"]
)

# Initialize 'restricted_model_df' and bind rows with two copies of 'dat'
restricted_model_df <- data.frame()
restricted_model_df <- bind_rows(
  restricted_model_df,
  dat %>% mutate(reference = 0),
  dat %>% mutate(reference = 1)
) %>% 
  # Generate a sequence of 'x' values and create all possible combinations
  crossing(x = seq(-7.5, 2.5, 0.1)) %>% 
  
  # Group by 'plate' and calculate the 'y' values based on the reference
  group_by(plate) %>% 
  mutate(
    y = case_when(
      reference == 1 ~ fourPL(x, Amin, Amax, log_EC50, B),
      TRUE ~ fourPL_sample(x, Amin, Amax, log_EC50, B, E)
    ),
    group = paste0(denominator, "_", numerator)
  ) %>% 
  
  # Bind rows with the original data, ordering by 'plate', 'type', and 'dose'
  bind_rows(
    .,
    df %>% 
      filter(plate == plate_number) %>% 
      arrange(plate, type, desc(dose))
  ) %>% 
  
  # Replace missing 'x' values with 'dose'
  mutate(
    x = case_when(
      is.na(x) ~ dose,
      TRUE ~ x
    )
  )

  
d <- df %>% 
  filter(plate == plate_number) %>% 
  arrange(plate, type, desc(dose))

ggplot(restricted_model_df, aes(colour = type)) +
  geom_line(aes(x,y)) + 
  geom_point(aes(x,response)) + 
  facet_wrap(~group, scales = "free", ncol=3) +
  theme(legend.position="bottom", legend.title = element_blank()) + 
  xlab("Log2 concentration") + 
  ylab("Response")
