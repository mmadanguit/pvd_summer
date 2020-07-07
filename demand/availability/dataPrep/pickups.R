library(tidyverse)

setwd("~/Documents/github/pvd_summer/demand/availability") # change to your WD

cleanEvents <- function(df){
  "Remove unnecessary events items"
}

# Process
# 1. Import
# 2. Map lat/long to tract
# 3. Clean
# 4. Group by tract
# 5. Find # of items for each  date
# 6. Save