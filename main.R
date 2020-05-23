##I don't understand why we have so many different files, it would be better to 
##have one file that we divide into sections -Alena

library("readxl")
library("tidyverse")

##### produces IPUMS_df for merge #####
#loading the data

#cleaning the data

#preparing the data for a merge


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
mls_df$ID <- paste(mls_df$Year, mls_df$State_county_FIPS, 
                   mls_df$State_Abb, sep = "_")
#^I'm not entirely sure that we need the state abbreviation there, but it might 
#be useful later, even if it actually ends up being irrelevant for the merge ID






##### Preparing health_df(S)? for merge #####
#it would probably be good to have a function for creating our death columns
#given that we can only download so much health data at once

#misc
alc_drug_df <- read.csv("alc_drugs.csv")
alc_drug_df <- subset(alc_drug_df, select = -ï..Notes)
alc_drug_df$Crude.Rate <- as.numeric(levels(alc_drug_df$Crude.Rate))[alc_drug_df$Crude.Rate]

##### [section] #####


##### [section] #####
