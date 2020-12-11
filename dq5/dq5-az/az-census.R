library(shiny)
library(tidyverse)
library(tidycensus)

# Import the helper functions in the main working file
source('../helper-functions.R') #copy and paste whatever is inside of the helper function file
envfile <- "../.env"
#Get the API key from helper code
api_key <- get_key(envfile, "census_api_key")

# Set census API key with census_api_key()
census_api_key(api_key, install = TRUE)

# Reload environment so I can use the key without restarting R (first time only)
readRenviron("~/.Renviron")

# Check environment
Sys.getenv("CENSUS_API_KEY")

# Examine variables in 2019 1-year ACS data set
v19 <- load_variables(2019, "acs1", cache = TRUE)

View(v19)

# Choose and rename variables of interest
pop_vars <- c(total_pop = "B01001_001")
# B030002 is our race/ethnicity variable of choice for all data sets
# B03002_012 represents all Hispanic or Latinx respondents, regardless of the
# Race that they selected
race_vars <- c(natv = "B03002_005",
               paci = "B03002_007",
               afam = "B03002_004",
               hisp = "B03002_012",
               asam = "B03002_006",
               angl = "B03002_003",
               mixd = "B03002_009",
               othr = "B03002_008"
)
# C160001 is our language variable of choice for all data sets
# Below are the top 5 non-English Languages spoken in TN based on the ACS5 2009-2013
# Update these variables for the most recent 5-year census available (if desired) and
# Update these variables for each state as needed
lang_vars <- c(spanish = "C16001_003",
               arabic = "C16001_033",
               chinese = "C16001_021",
               vietnamese = "C16001_024",
               korean = "C16001_018",
               english = "C16001_002"
)
# combine all variable categories into one list
my_vars <- c(pop_vars, race_vars, lang_vars, sex_vars, age_vars)
# Pull ACS 2019 1-year data for all counties in TN; specify Davidson+ counties after
data_example <- get_acs(
    geography = "county",
    year = 2019,
    survey = "acs1",
    state = "TN",
    variables = my_vars,
    output = "wide"
)

sex_vars <- c(male = 'B01001_002',
            female = 'B01001_026')


age_vars <- c(gen_z = c('B01001_007','B01001_008','B01001_009','B01001_010', 'B01001_031', 'B01001_032','B01001_033','B01001_034'),
              millen = c('B01001_011','B01001_012','B01001_013','B01001_035','B01001_036','B01001_037'),
              gen_x = c('B01001_014','B01001_015','B01001_016','B01001_038','B01001_039','B01001_040'),
              boomer = c('B01001_017','B01001_018','B01001_019','B01001_020','B01001_021','B01001_022','B01001_041','B01001_042','B01001_043','B01001_044','B01001_045','B01001_046'),
              silent = c('B01001_023','B01001_024','B01001_025','B01001_047','B01001_048','B01001_049')

)


