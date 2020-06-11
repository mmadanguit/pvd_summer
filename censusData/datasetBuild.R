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
# naming scheme: https://www.census.gov/programs-surveys/acs/guidance/which-data-tool/table-ids-explained.html
# validation: https://www.census.gov/programs-surveys/acs/guidance/subjects.html
pop <- c(Pop = "B01001_001") # verified
sex <- c(Male = "B01001_002", Female = "B01001_026") # verified
race <- c(White = "B03002_003", Black = "B03002_004", 
          Native = "B03002_005", Asian = "B03002_006", 
          HIPI = "B03002_007", oAlone = "B03002_008",
          oTwo = "B03002_009", Hispanic = "B03002_012") # verified
inc <- c(inc0 = "B07010_002", inc1 = "B07010_004", inc2 = "B07010_005",
         inc3 = "B07010_006", inc4 = "B07010_007", inc5 = "B07010_008",
         inc6 = "B07010_009", inc7 = "B07010_010", inc8 = "B07010_011")
         # only half of pop
incM <- c(incTot = "B20001_001", inc0m = "B20001_003", inc1m = "B20001_004",
          inc2m = "B20001_005", inc3m = "B20001_006", inc4m = "B20001_007",
          inc5m = "B20001_008", inc6m = "B20001_009", inc7m = "B20001_010",
          inc8m = "B20001_011", inc9m = "B20001_012", inc10m = "B20001_013",
          inc11m = "B20001_014", inc12m = "B20001_015", inc13m = "B20001_016",
          inc14m = "B20001_017", inc15m = "B20001_018", inc16m = "B20001_019",
          inc17m = "B20001_020", inc18m = "B20001_021", inc19m = "B20001_022")
incF <- c(inc0f = "B20001_003", inc1f = "B20001_004",
          inc2f = "B20001_005", inc3f = "B20001_006", inc4f = "B20001_007",
          inc5f = "B20001_008", inc6f = "B20001_009", inc7f = "B20001_010",
          inc8f = "B20001_011", inc9f = "B20001_012", inc10f = "B20001_013",
          inc11f = "B20001_014", inc12f = "B20001_015", inc13f = "B20001_016",
          inc14f = "B20001_017", inc15f = "B20001_018", inc16f = "B20001_019",
          inc17f = "B20001_020", inc18f = "B20001_021", inc19f = "B20001_022")
cit <- c(NotCitizen = "B05001_006") # verified
dis <- c(mDisability = "B18101_002", fDisability = "B18101_021") # verified
educ <- c(educTot = "B15003_001", noSch = "B15003_002", nursery = "B15003_003", 
          kinder = "B15003_004", first = "B15003_005", sec = "B15003_006", 
          third = "B15003_007", fourth = "B15003_008", fifth = "B15003_009", 
          sixth = "B15003_010", seventh = "B15003_011", eigth = "B15003_012", 
          ninth = "B15003_013", tenth = "B15003_014", eleventh = "B15003_015",
          twelfth = "B15003_016", high = "B15003_017", GED = "B15003_018",
          sCollege1 = "B15003_019", sCollege2 = "B15003_020", assoc = "B15003_021",
          bachelor = "B15003_022", master = "B15003_023", profSchool = "B15003_024",
          phd = "B15003_025") # only for 25+

# B06007_0## vars used as B16001_0## vars were NA
lang <- c(langTot = "B06007_001", engOnly = "B06007_002", spanish = "B06007_003",
          spanishStrE = "B06007_004", spanishWeakE = "B06007_005")
vars = c(pop, incM)

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