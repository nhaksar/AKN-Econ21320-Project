##I don't understand why we have so many different files, it would be better to 
##have one file that we divide into sections -Alena

library("readxl")
library("tidyverse")
library("plm")

##### produces IPUMS_df for merge #####
#loading the data
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

# ddi <- read_ipums_ddi("usa_00004.xml")
# ipums_data <- read_ipums_micro(ddi)

#cleaning the data

#preparing the data for a merge

##############





#####State Abbreviations#####
states <- c("Alabama", "Alaska", "American Samoa", 
            "Arizona", "Arkansas", "California",
            "Colorado", "Connecticut", "Delaware",
            "District of Columbia", "Federated States of Micronesia",
            "Florida", "Georgia", "Guam", "Hawaii", "Idaho",
            "Illinois", "Indiana", "Iowa", "Kansas",
            "Kentucky", "Louisiana", "Maine", "Marshall Islands",
            "Maryland", "Massachusetts", "Michigan", "Minnesota",
            "Mississippi", "Missouri", "Montana", "Nebraska",
            "Nevada", "New Hampshire", "New Jersey", "New Mexico",
            "New York", "North Carolina", "North Dakota",
            "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Palau",
            "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina",
            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
            "Virgin Islands", "Virginia", "Washington", "West Virginia",
            "Wisconsin", "Wyoming")
abbreviations <- c("AL", "AK", "AS", 
                   "AZ", "AR", "CA",
                   "CO", "CT", "DE",
                   "DC", "FM",
                   "FL", "GA", "GU", "HI", "ID",
                   "IL", "IN", "IA", "KS",
                   "KY", "LA", "ME", "MH",
                   "MD", "MA", "MI", "MN",
                   "MS", "MO", "MT", "NE",
                   "NV", "NH", "NJ", "NM",
                   "NY", "NC", "ND",
                   "MP", "OH", "OK", "OR", "PW",
                   "PA", "PR", "RI", "SC",
                   "SD", "TN", "TX", "UT", "VT",
                   "VI", "VA", "WA", "WV",
                   "WI", "WY")
state_abbs <- tibble("State" = states,
                     "Abbreviation" = abbreviations)
state_abbs <- rbind(state_abbs, c("Washington D.C.", "DC")) %>% 
  rbind(c("Canada", "Canada")) %>%
  rbind(c("Unknown", "Unknown"))
##############





##### Produces mls_df for merge #####
#loading data
mls_df <- read_excel("cntyic.xlsx", skip = 1)
#cleaning data
ncol(mls_df)
for (col in c(6:ncol(mls_df))) {
  colnames(mls_df)[col] <- paste(mls_df[1,col],"layoff",sep = "_")
}
mls_df <- mls_df[3:nrow(mls_df),]
#preparing for merge based on a county-year id
colnames(mls_df) <- gsub(" ", "_", colnames(mls_df))
mls_df$State_county_FIPS <- as.integer(mls_df$State_county_FIPS)
mls_df$State <- sapply(strsplit(mls_df$County_name, ", "), "[", 2)
mls_df$State_Abb <- state_abbs$Abbreviation[match(mls_df$State, 
                                                  state_abbs$State)]
mls_df$ID <- paste(mls_df$Year, mls_df$State_county_FIPS, sep = "_")

##############








##### Preparing health_df(S)? for merge #####
#it would probably be good to have a function for creating our death columns
#given that we can only download so much health data at once

#Takes in the alcdrugs csv and 
# 1. Adds a county_year ID Column
# 2. Converts from a long dataframe to a wide dataframe 
get_cleaned_alcdrugs = function(raw_alcdrug_df){
  #raw_alcdrug_df <- subset(raw_alcdrug_df, select = -ï..Notes)
  #raw_alcdrug_df$Crude.Rate <- as.numeric(levels(raw_alcdrug_df$Crude.Rate))[raw_alcdrug_df$Crude.Rate]
  relevant = raw_alcdrug_df[raw_alcdrug_df$Drug.Alcohol.Induced.Code != "O", ]
  relevant$ID = paste(relevant$County.Code, relevant$Year, sep="_")
  wide_df = dcast(relevant, ID + Population ~Drug.Alcohol.Induced, value.var="Deaths")
  return(wide_df)
}

raw_drug_df <- read.csv("alc_drugs.csv")
cleaned_drug_df = get_cleaned_alcdrugs(raw_drug_df)


##############





##### Making a column of IDs to merge stuff onto #####
county_ids <- unique(mls_df$State_county_FIPS)
years <- c(1999:2013)
all_county_years <- expand.grid(county_ids, years)
colnames(all_county_years) <- c("County_code", "Year")
all_county_years$ID <- paste(all_county_years$Year, 
                             all_county_years$County_code, sep = "_")

##############





##### [section] #####


>>>>>>> 4f9013e7c399245770a237852b32ebaf6037b6f2
