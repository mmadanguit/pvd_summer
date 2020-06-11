# to do
# * load all vars (citizenship, language spoken, income,
# education, )
# give income the option of individual income, and average income
# * validate results
# * loop through and than concat at end?

library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(stringr)
# view all variables: load_variables(year, dataset, cache=BOOL)
# eg: load_variables(2018, "acs5", cache=TRUE)
sex <- c(Male = "B01001_002", Female = "B01001_026")
race <- c(White = "B03002_003", Black = "B03002_004", 
               Native = "B03002_005", Asian = "B03002_006", 
               HIPI = "B03002_007", Hispanic = "B03002_012")
inc <- c(inc0 = "B07010_002", inc1 = "B07010_004", inc2 = "B07010_005",
         inc3 = "B07010_006", inc4 = "B07010_007", inc5 = "B07010_008",
         inc6 = "B07010_009", inc7 = "B07010_010", inc8 = "B07010_011")
educ <- c(uHigh = "B07009_002", High = "B07009_003",
          sCollege = "B07009_004", Bachelor = "B07009_005", 
          Graduate = "B07009_006")
hous <- c(own = "B07013_002", rent = "B07013_003")

# population

vars = c(sex, race, inc, educ)
print(vars)

# get data
riPop <- get_acs(geography = "tract", 
                  state = "RI", 
                  county = 'Providence County',
                  variables = vars,
                  geometry = FALSE, # shape data
                  cache_table = TRUE) 

# trim, reshape to tidy, constrain to providence reshape to be tidy
riPop = riPop %>% select(c('GEOID','NAME','variable','estimate')) %>%
  spread(key='variable',value='estimate')

# constrain to providence
riPop = riPop[riPop$GEOID < 44007010000,]

# sort columns
riPop = riPop[c('GEOID', 'NAME', names(vars))]