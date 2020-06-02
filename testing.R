
#####Code to test how to best match the county codes between the two datasources#####
library("readxl")
library("tidyverse")
df <- read.csv("alc_drugs.csv")
dfmls <- read_excel("cntyic.xlsx", skip = 1)
ncol(dfmls)
for (col in c(6:ncol(dfmls))) {
  colnames(dfmls)[col] <- paste(dfmls[1,col],"layoff",sep = "_")
}
dfmls <- dfmls[3:nrow(dfmls),]

MLS_codes_counties <- dfmls[, c(3,4,5)]
CDC_codes_counties <- df[,c(2,3)]
colnames(MLS_codes_counties) <- c("County_FIPS", "State_County_FIPS", 
                                  "County_Name")
MLS_codes_counties <- MLS_codes_counties[order(MLS_codes_counties$County_Name),]
CDC_codes_counties <- CDC_codes_counties[order(CDC_codes_counties$County),]

MLS_codes_counties <- MLS_codes_counties %>% unique()
CDC_codes_counties <- CDC_codes_counties %>% unique()

typeof(CDC_codes_counties$County.Code)
typeof(MLS_codes_counties$State_County_FIPS)

MLS_codes_counties$int_State_County_FIPS <- as.integer(MLS_codes_counties$State_County_FIPS)
county_codes_comparison <- cbind(MLS_codes_counties, CDC_codes_counties)

##### Double-checking to make sure that no rows from mls_df were dropped #####
mls_ids <- unique(mls_df$ID)
main_ids <- unique(main_df$ID)
test <- mls_ids[! (mls_ids %in% main_ids)]
match(test[1], main_ids)
match(test[1], mls_ids)
mls_ids[1]


##### Path to read IPUMS Data to check vars #####
library(readr)
ipums_df <- read_csv("C:/Users/ahkan/Desktop/usa_00001.csv")
#seems like the file is still too large; worry about later