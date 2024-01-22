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
plate_number <- 1
type_number <- 1
model <- nlsLM(
  formula = response ~ Amin + ((Amax - Amin) / (1 + 2^(-B * (log_dose - log_EC50 + E * material_type)))),
  data = df %>% filter(plate == plate_number),
  start = list(
    Amin = 0,
    Amax = max(df$response),
    log_EC50 = 2,
    B = -1,
    E = 1
  )
)

model_summary <- summary(model)
model_coefficients <- model_summary$coefficients


#### Plot Model(s) ####
restricted_model_df <- data.frame()

type <- df %>% 
  filter(plate == plate_number) %>% 
  distinct(type) %>% 
  pull(type)

coefficients <- model_summary$coefficients

dat <- data.frame(
  plate = plate_number,
  numerator = types[type_number],
  denominator = "Standard",
  type = type[type_number],
  group = paste0(type[type_number], "_", "Standard"),
  Amin = model_coefficients["Amin", "Estimate"],
  Amax = model_coefficients["Amax", "Estimate"],
  log_EC50 = model_coefficients["log_EC50", "Estimate"],
  B = model_coefficients["B", "Estimate"],
  E = model_coefficients["E", "Estimate"],
  Amin_se = model_coefficients["Amin", "Std. Error"],
  Amax_se = model_coefficients["Amax", "Std. Error"],
  log_EC50_se = model_coefficients["log_EC50", "Std. Error"],
  B_se = model_coefficients["B", "Std. Error"],
  E_se = model_coefficients["E", "Std. Error"]
)

restricted_model_df <- data.frame()
restricted_model_df <- bind_rows(
  restricted_model_df,
  dat %>% mutate(reference = 0),
  dat %>% mutate(reference = 1)
) %>% 
  crossing(x = seq(-7.5, 2.5, 0.1)) %>% 
  group_by(plate) %>% 
  mutate(
    y = case_when(
      reference == 1 ~ fourPL(x, Amin, Amax, log_EC50, B),
      TRUE ~ fourPL_sample(x, Amin, Amax, log_EC50, B, E)
    ),
    group = paste0(denominator, "_", numerator)
  ) %>% 
  bind_rows(
    .,
    df %>% 
      filter(plate == plate_number) %>% 
      arrange(plate, type, desc(dose))
  ) %>% 
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
