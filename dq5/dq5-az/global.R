library(shiny)
library(tidycensus)
library(tidyverse)
library(plotly)
library(shiny)



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
#keeping genders separately
age_mf <- age %>%
    mutate(generation = case_when(
        variable %in% gen_zm ~ 'gen_zm',
        variable %in% gen_zf ~ 'gen_zf',
        variable %in% millenm ~ 'millenm',
        variable %in% millenf ~ 'millenf',
        variable %in% gen_xm ~ 'gen_xm',
        variable %in% gen_xf ~ 'gen_xf',
        variable %in% boomerm ~ 'boomer_m',
        variable %in% boomerf ~ 'boomerf',
        variable %in% silentm ~ 'silentm',
        variable %in% silentf ~ 'silentf'
    ))
#separating age by decade:

under_20m <- 'gen_z1'
under_20f<- 'gen_z5'

twentiesm <- c('gen_z2', 'gen_z3', 'gen_z4', 'millen1')
twentiesf <- c('gen_z6','gen_z7','gen_z8','millen4')

thirtiesm <- c('millen2','millen3')
thirtiesf <- c('millen5','millen6')

fortiesm <- c('gen_x1','gen_x2')
fortiesf <- c('gen_x4', 'gen_x5')

fiftiesm <- c('gen_x3','boomer1')
fiftiesf <- c('gen_x6','boomer7')

above_60m <- c('boomer2','boomer3','boomer4','boomer5','boomer6','silent1','silent2','silent3')
above_60f <- c('boomer8','boomer9','boomer10','boomer11','boomer12','silent4','silent5','silent6')

decade_mf <- age %>%
    mutate(decade = case_when(
        variable %in% under_20m ~ 'under20m',
        variable %in% under_20f ~ 'under_20f',
        variable %in% twentiesm ~ 'twentiesm',
        variable %in% twentiesf ~ 'twentiesf',
        variable %in% thirtiesm ~ 'thirtiesm',
        variable %in% thirtiesf ~ 'thirtiesf',
        variable %in% fortiesm ~ 'fortiesm',
        variable %in% fortiesf ~ 'fortiesf',
        variable %in% fiftiesm ~ 'fiftiesm',
        variable %in% fiftiesf ~ 'fiftiesf',
        variable %in% above_60m ~ 'above_60m',
        variable %in% above_60f ~ 'above_60f',

    ))

#Alexa: create data dictionary of each generation and what it corresponds to

#Now I want to just look at Davidson County

age_Davidson <- age %>%
    filter(NAME=='Davidson County, Tennessee')

age_Davidsonmf <- age_mf %>%
    filter(NAME=='Davidson County, Tennessee')

gender_Davidson <- gender %>%
    filter(NAME=='Davidson County, Tennessee')


#below is the table I'll want to graph for generations
pew_Davidson <-age_Davidson %>%
    group_by(generation) %>%
    summarise(pop=sum(estimate))

#male and female numbers separate:
gen_Davidsonmf <-age_Davidsonmf %>%
    group_by(generation) %>%
    summarise(pop=sum(estimate))

#below is the table I'll want to graph for decades
dec_Davidson <- decade_mf %>%
    group_by(decade) %>%
    summarise(pop=sum(estimate))

#graphing gender (df is gender_Davidson) as it's the simplest:
#
#
#
#
#
#
# gender_gg <- gender_Davidson %>%
#     plot_ly(labels= ~variable, values= ~estimate, marker = list(colors = c('#F6DDB6','#F0C37F')))
# gender_gg<- gender_gg %>%
#     add_pie(hole=0.5)
# gender_gg <- gender_gg %>%
#     layout(title='Male and Female Population',
#            xaxis=list(showgrid=FALSE, zeroline= FALSE, showticklabels= FALSE),
#            yaxis= list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))
# gender_gg

#graphing decade age with df dec_Davidson
#
#
#
#
#
#hard coding the data frame because I can't figure out how to transform it to look like this:
women <- c(60947,345211,329122,305092,308717,559222)
men <- c(66397,334193,313213,292953,285526, 442691)
labels <- c('<20', '20-29','30-39','40-49','50-59','60+')

dec_Davidson <- data.frame(labels,women,men)

#don't like this graph, trying it with stacked bar chart
decade_gg <- plot_ly(dec_Davidson, x=~labels, y=~women, type='bar', name='Female')
decade_gg <- decade_gg %>% add_trace(y=~men, name='Male')
decade_gg <- decade_gg %>%  layout (yaxis=list(title='Population'),
                                    xaxis=list(title='Age grouping'),
                                               barmode='stack')
decade_gg
#need to adjust the colors and getting under20 to be at the beginning of the list


#graphing for age by generation
#
#
#
#
#
#
#
#hard coding the data frame because I can't figure out how to transform it to look like this:
women_gen <- c(35048, 102428,59155,71551,20942)
men_gen <- c(32147,93325,62732,61285,12477)
generation <- c('Gen Z', 'Millennials', 'Gen X', 'Boomers', 'Silent')

gen_Davidsonmf <- data.frame(generation, women_gen, men_gen)

gen_gg <- plot_ly(gen_Davidsonmf, x=~generation, y=~women_gen, type='bar', name='Female', color = I('#9C877B'))
gen_gg <- gen_gg %>% add_trace(y=~men_gen, name='Male', color= I('#DCC5A8'))
gen_gg <- gen_gg %>%  layout (yaxis=list(title='Population'),
                                    xaxis=list(title='Age grouping'),
                                    barmode='stack')
gen_gg

#need to adjust the colors and getting generation orders correct
