# see censusData.md for notes
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(stringr)
pop <- c(Pop = "B01001_001") # verified
sex <- c(Male = "B01001_002", Female = "B01001_026") # verified
race <- c(White = "B03002_003", Black = "B03002_004", 
          Native = "B03002_005", Asian = "B03002_006", 
          HIPI = "B03002_007", oAlone = "B03002_008",
          oTwo = "B03002_009", Hispanic = "B03002_012") # verified
inc <- c(medFamInc = "B19113_001", perCapitaInc = "B19301_001", 
         sampInc = "B19001_001", inc0 = "B19001_002", inc1 = "B19001_003", 
         inc2 = "B19001_004", inc3 = "B19001_005", inc4 = "B19001_006", 
         inc5 = "B19001_007", inc6 = "B19001_008", inc7 = "B19001_009", 
         inc8 = "B19001_010", inc9 = "B19001_011", inc10 = "B19001_012", 
         inc11 = "B19001_013", inc12 = "B19001_014", inc13 = "B19001_015", 
         inc14 = "B19001_016", inc15 = "B19001_017") # verified
pov <- c(sampPov = "B17020_001", 
         Poverty = "B17020_002", abovePoverty ="B17020_010") # verified
educ <- c(sampEduc = "B06009_001", uHigh = "B06009_002", 
          High = "B06009_003", sCollOrAssoc = "B06009_004", 
          Bachelor = "B06009_005", Graduate = "B06009_006") # verified
net <- c(sampInt = "B28011_001", intSubs = "B28011_002",
         internetAccess = "B28011_007", noInternetAccess = "B28011_008") # verified
cit <- c(usBornCit = "B05001_002", pBornCit = "B05001_003", 
         aBornCit = "B05001_004", natCit = "B05001_005", 
         NotCitizen = "B05001_006") # verified
dis <- c(sampDis = "B18135_001", 
         Dis19 = "B18135_003", nDis19 = "B18135_008",
         Dis64 = "B18135_014", nDis64 = "B18135_019", 
         Dis65 = "B18135_025", nDis65 = "B18135_030") # verified
lang <- c(sampLang = "B06007_001", engOnly = "B06007_002", 
          spanish = "B06007_003", spanishStrE = "B06007_004", 
          spanishWeakE = "B06007_005") # verified
comm <- c(sampComm = "B08134_001", comm0 = "B08134_002", comm1 = "B08134_003",
          comm2 = "B08134_004", comm3 = "B08134_005", comm4 = "B08134_006", 
          comm5 = "B08134_007", comm6 = "B08134_008", comm7 = "B08134_009",
          comm8 = "B08134_010", auto = "B08134_011", public = "B08134_061", 
          walk = "B08134_101", other = "B08134_111") # verified
enrol <- c(sampEnrol = "B14007_001",
           college = "B14007_017", grad = "B14007_018")
vars = c(pop, sex, race, cit, lang, inc, pov, net, dis, comm, enrol)

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
riPop = riPop[riPop$GEOID < 44007010000,] # constrain to providence
riPop = riPop[c('GEOID', 'NAME', names(vars))] # sort columns

# group and select functions
group <- function(df, toGroup, colName){
  for (i in 2:length(toGroup)){
    df[toGroup[1]] <- df[toGroup[1]] + df[toGroup[i]] # add columns
    df[toGroup[i]] <- NULL # remove col
  }
  names(df)[names(df)==toGroup[1]] <- colName # rename col
  return(df)
}
percent <- function(df, items, pop, delete=FALSE){
  for (item in items){
    df[item] = df[item]/df[pop]
  }
  if (delete) {
    df[pop] <- NULL # remove col
  }
  return(df)
}

# GROUP DATA
riPop = riPop %>% group(c('intSubs', 'internetAccess'), 'InternetAccess') %>%
  group(c('Dis19', 'Dis64', 'Dis65'), 'Disability') %>%
  group(c('nDis19', 'nDis64', 'nDis65'), 'NoDisability') %>%
  group(c('usBornCit', 'pBornCit', 'aBornCit', 'natCit'), 'Citizen') %>%
  group(c('Native', 'Asian', 'HIPI', 'oAlone', 'oTwo'), 'Other') %>%
  # simplify income brackets
  group(c('inc0', 'inc1'), 'inc0') %>% # < 15k
  group(c('inc2', 'inc3'), 'inc1') %>% # 15-25k
  group(c('inc4', 'inc5'), 'inc2') %>% # 25-35k
  group(c('inc6', 'inc7'), 'inc3') %>% # 35-45k
  group(c('inc8', 'inc9'), 'inc4') %>% # 45-60k
  group(c('inc10', 'inc11'), 'inc5') %>% # 60-100k
  group(c('inc12', 'inc13'), 'inc6') %>% # 100-150k
  group(c('inc14', 'inc15'), 'inc7') %>%# > 150k
  group(c('college', 'grad'), 'college')
  
# FIND PERCENTS
totPop = c('Male', 'Female', 'White', 'Black', 'Other', 'Hispanic',
           'Citizen', 'NotCitizen')
riPop = riPop %>% percent(totPop, 'Pop') %>%
  percent(c('engOnly', 'spanish', 'spanishStrE', 'spanishWeakE'), 
          'sampLang', TRUE) %>%
  percent(c('inc0', 'inc1', 'inc2', 'inc3', 'inc4', 'inc5', 'inc6', 'inc7'),
          'sampInc', TRUE) %>%
  percent(c('Poverty', 'abovePoverty'), 'sampPov', TRUE) %>%
  percent(c('InternetAccess', 'noInternetAccess'), 'sampInt', TRUE) %>%
  percent(c('Disability', 'NoDisability'), 'sampDis', TRUE) %>%
  percent(c('comm0', 'comm1', 'comm2', 'comm3', 'comm4', 'comm5', 'comm6', 
            'comm7', 'comm8', 'auto', 'public', 'walk', 'other'), 'sampComm', TRUE) %>%
  percent('college', 'sampEnrol', TRUE)

write.csv(riPop,file='~/riData.csv')