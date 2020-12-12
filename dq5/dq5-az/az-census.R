library(shiny)
library(tidycensus)
library(tidyverse)


# Import the helper functions in the main working file
source('../helper-functions.R') #copy and paste whatever is inside of the helper function file
envfile <- "../.env"
#Get the API key from helper code
api_key <- get_key(envfile, "census_api_key")

# Set census API key with census_api_key()
census_api_key(api_key,overwrite = TRUE, install = TRUE)

# Reload environment so I can use the key without restarting R (first time only)
readRenviron("~/.Renviron")

# Check environment
Sys.getenv("CENSUS_API_KEY")

# Examine variables in 2019 1-year ACS data set
v19 <- load_variables(2019, "acs1", cache = TRUE)

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


age_vars_gen <- c(gen_z = c('B01001_007','B01001_008','B01001_009','B01001_010', 'B01001_031', 'B01001_032','B01001_033','B01001_034'),
              millen = c('B01001_011','B01001_012','B01001_013','B01001_035','B01001_036','B01001_037'),
              gen_x = c('B01001_014','B01001_015','B01001_016','B01001_038','B01001_039','B01001_040'),
              boomer = c('B01001_017','B01001_018','B01001_019','B01001_020','B01001_021','B01001_022','B01001_041','B01001_042','B01001_043','B01001_044','B01001_045','B01001_046'),
              silent = c('B01001_023','B01001_024','B01001_025','B01001_047','B01001_048','B01001_049')

)

View(v19)
#I want three data sets that have just my two variables: gender and age (age is grouped by generation when using PEW and decades when using example data)

gender <- get_acs(
    geography = 'county',
    year = 2019,
    survey = 'acs1',
    state= 'TN',
    variables= sex_vars
)

age <- get_acs(
    geography = 'county',
    year = 2019,
    survey = 'acs1',
    state= 'TN',
    variables= age_vars_gen
)

#Let's drop the MOE (margin of error) column
gender <- gender %>%
    select(-moe)

age <- age %>%
    select(-moe)

View(age)

#I want to clean up age so that each generation/gender is grouped together

gen_zm <- c('gen_z1','gen_z2', 'gen_z3', 'gen_z4')
gen_zf <- c('gen_z5','gen_z6','gen_z7','gen_z8')

millenm <- c('millen1','millen2','millen3')
millenf <- c('millen4','millen5','millen6')

gen_xm <- c('gen_x1','gen_x2', 'gen_x3')
gen_xf <- c('gen_x4','gen_x5','gen_x6')

boomerm <- c('boomer1','boomer2','boomer3','boomer4','boomer5','boomer6')
boomerf <- c('boomer7','boomer8','boomer9','boomer10','boomer11','boomer12')

silentm <- c('silent1','silent2','silent3')
silentf <- c('silent4','silent5','silent6')

#create a new column before group_by

age <- age %>%
    mutate(generation = case_when(
        variable %in% gen_zm | variable %in% gen_zf ~ 'gen_z',
        variable %in% millenm | variable %in% millenf ~ 'millen',
        variable %in% gen_xm | variable %in% gen_xf ~ 'gen_x',
        variable %in% boomerm | variable %in% boomerf ~ 'boomer',
        variable %in% silentm | variable %in% silentf ~ 'silent'
    ))


#I learned i have to use %in% instead of == due to having multiple variables, am still working on this
#Alexa: create data dictionary of each generation and what it corresponds to
age %>%
    group_by(NAME) %>%
    filter(generation == 'gen_z') %>%
    mutate(gen_z = sum(estimate))

age %>%
    group_by(NAME) %>%
    filter(generation == 'millen') %>%
    mutate(millen = sum(estimate))

age %>%
    group_by(NAME) %>%
    filter(generation == 'gen_x') %>%
    mutate(gen_x = sum(estimate))

age %>%
    group_by(NAME) %>%
    filter(generation == 'boomer') %>%
    mutate(boomer = sum(estimate))

age %>%
    group_by(NAME) %>%
    filter(generation == 'silent') %>%
    mutate(silent = sum(estimate))

View(age)
#Now I want to just look at Davidson County

gender_Davidson <- gender %>%
    filter(NAME=='Davidson County, Tennessee')

age_Davidson <- age %>%
    filter(NAME=='Davidson County, Tennessee')

