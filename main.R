##I don't understand why we have so many different files, it would be better to 
##have one file that we divide into sections -Alena

library("readxl")
library("tidyverse")
library("reshape2")
library("stargazer")
if (!require("plm")) install.packages("plm")
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

## add "_layoff" to name of layoff variables
for (col in c(6:ncol(mls_df))) {
  colnames(mls_df)[col] <- paste(mls_df[1,col],"layoff",sep = "_")
}

## drop first two rows
mls_df <- mls_df[3:nrow(mls_df),] 

#preparing for merge based on a county-year id
colnames(mls_df) <- gsub(" ", "_", colnames(mls_df)) # replaces name " " with "_"
mls_df$State_county_FIPS <- as.integer(mls_df$State_county_FIPS) # conver chr to num
mls_df$State_FIPS <- as.integer(mls_df$State_FIPS)
mls_df$County_FIPS <- as.integer(mls_df$County_FIPS)
mls_df$State <- sapply(strsplit(mls_df$County_name, ", "), "[", 2) # adds state to row
mls_df$State_Abb <- state_abbs$Abbreviation[match(mls_df$State, 
                                                  state_abbs$State)]
mls_df$ID <- paste(mls_df$Year, mls_df$State_county_FIPS, sep = "_") # assigns ID

# convert layoff numbers to numeric
mls_df[,6:20] <- sapply(mls_df[,6:20],as.integer)

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
  relevant$ID = paste(relevant$Year, relevant$County.Code, sep="_")
  wide_df = dcast(relevant, ID + Population ~Drug.Alcohol.Induced.Code, value.var="Deaths")
  return(wide_df)
}

raw_drug_df <- read.csv("alc_drugs.csv")
cleaned_drug_df = get_cleaned_alcdrugs(raw_drug_df)


##############





##### Making a column of IDs to merge stuff onto #####
df_for_ids <- read.delim("For_County_IDs.txt", sep = "\t")
df_for_ids <- df_for_ids[-which(is.na(df_for_ids$County.Code)),]
county_ids <- df_for_ids$County.Code
years <- c(1999:2013)
#I realized I needed to get a complete set of county_ids
all_county_years <- expand.grid(county_ids, years)

## for some reason, expand.grid returns list not data.frame. 
## causes errors. below line converts to data.frame, fixes
all_county_years <- data.frame(all_county_years)
colnames(all_county_years) <- c("County_code", "Year")
all_county_years$ID <- paste(all_county_years$Year, 
                             all_county_years$County_code, sep = "_")
#Setting up a column to do state-year fixed effects on (as an alternative for 
#year fixed effects)
all_county_years$states <- as.character(all_county_years$County_code) %>%
                substr(1,(nchar(as.character(all_county_years$County_code))-3))
all_county_years$state_years <- paste(all_county_years$states, 
                                      all_county_years$Year, sep = "_")
##############





##### Merge all DataFrames #####
main_df <- merge(all_county_years, mls_df, all.x = TRUE, by="ID") ## merges on ID
#note there are some rows from mls_df that didn't come over b/c they were from
#before 1999

main_df <- merge(main_df, cleaned_drug_df, all.x = TRUE, by="ID")
main_df$Year <- main_df$Year.x
main_df <- subset(main_df, select = - c(Year.x,Year.y))
main_df$total_deaths <- main_df$A + main_df$D

#main_df[is.na(main_df$Total_layoff),"Total_layoff"] = 0 ## NA => no layoffs

## NA => no layoffs, applies rule for all layoff columns
for (i in 9:23){
  main_df[is.na(main_df[i]),i] = 0
}

panel_df <- pdata.frame(main_df, index=c("County_code", "Year"))

##### SUMMARY STATISTICS #####
## min/max year in cleaned data
min(main_df[,"Year"])
max(main_df[,"Year"])

## total layoffs 1999-2013
tot.sum <- sum(main_df[!is.na(main_df$Total_layoff), "Total_layoff"])
wh.sum <- sum(main_df[!is.na(main_df$White_layoff), "White_layoff"])
bl.sum <- sum(main_df[!is.na(main_df$Black_layoff), "Black_layoff"])
hisp.sum <- sum(main_df[!is.na(main_df$Hispanic_orgin_layoff), "Hispanic_orgin_layoff"])
asia.sum <-sum(main_df[!is.na(main_df$Asian_or_Pacific_islander_layoff),
            "Asian_or_Pacific_islander_layoff"])
amin.sum <- sum(main_df[!is.na(main_df$American_indian_or_Alaskan_native_layoff),
            "American_indian_or_Alaskan_native_layoff"])
labeled.sums <- cbind(c("Total", "White", "Black","Hispanic",
                        "Asian/Pacific Islander",
                        "American Indian/Alaskan Native"),
                      c(tot.sum,wh.sum,bl.sum,hisp.sum,asia.sum,amin.sum))

## per-year average
sum(main_df[!is.na(main_df$Total_layoff), "Total_layoff"]) / 14

## make table for sums
stargazer(labeled.sums,
          title="Total layoffs 1999-2013 by ethnicity",
          align=TRUE, label="tb:layoff-sum")

##### REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = total_deaths ~ Total_layoff, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = total_deaths ~ Total_layoff + lag(Total_layoff, 1),
            model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1) 
            + lag(Total_layoff, 2), model="within", effect="twoways",
            data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
            + lag(Total_layoff, 2) + lag(Total_layoff, 3),
            model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total alcohol and drug deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total alcohol and drug deaths regressed on total layoffs with year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")

##### REGRESSIONS ON TOTAL LAYOFFS WITH YEAR-STATE FE #####
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag_ysfe <- plm(formula = total_deaths ~ Total_layoff + factor(state_years), 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag_ysfe <- plm(formula = total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + factor(state_years),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag_ysfe <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2) + factor(state_years), 
                model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag_ysfe <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3) + factor(state_years),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag_ysfe <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + factor(state_years),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag_ysfe <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5) + factor(state_years),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag_ysfe, mod_1lag_ysfe, mod_2lag_ysfe, mod_3lag_ysfe, mod_4lag_ysfe, 
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, # omit.stat="f",
          omit="([0-9]{1,2}_[0-9]{4})+",
          dep.var.labels = c("Total alcohol and drug deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total alcohol and drug deaths regressed on total layoffs with year, county, and state-year fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-ysfe")

##### REGRESSIONS ON WHITE LAYOFFS #####
mod_white_nolag <- plm(formula = total_deaths ~ White_layoff, 
                 model="within", data = panel_df)

## total deaths on white layoffs + 1 year lag. FE model
mod_white_1lag <- plm(formula = total_deaths ~ White_layoff + lag(White_layoff, 1),
                      model="within", effect="twoways", data = panel_df)

## total deaths on white layoff + 1,2 year lag. FE model
mod_white_2lag <- plm(formula=total_deaths ~ White_layoff + lag(White_layoff, 1) 
                      + lag(White_layoff, 2), model="within", effect="twoways",
                      data=panel_df)

## total deaths on white layoff + 1,2,3 year lag. FE model
mod_white_3lag <- plm(formula=total_deaths ~ White_layoff + lag(White_layoff, 1)
                      + lag(White_layoff, 2) + lag(White_layoff, 3),
                      model="within", effect="twoways",data=panel_df)

## total deaths on white layoff + 1,2,3,4 year lag. FE model
mod_white_4lag <- plm(formula=total_deaths ~ White_layoff + lag(White_layoff, 1)
                      + lag(White_layoff, 2) + lag(White_layoff, 3)
                      + lag(White_layoff, 4),
                      model="within", effect="twoways", data=panel_df)
## total deaths on white layoff + 1,2,3,4,5 year lag. FE model
mod_white_5lag <- plm(formula=total_deaths ~ White_layoff + lag(White_layoff, 1)
                      + lag(White_layoff, 2) + lag(White_layoff, 3)
                      + lag(White_layoff, 4) + lag(White_layoff, 5),
                      model="within", effect="twoways", data=panel_df)

## stargazer table
stargazer(mod_white_nolag, mod_white_1lag, mod_white_2lag, mod_white_3lag, 
          mod_white_4lag, mod_white_5lag, 
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total alcohol and drug deaths"),
          covariate.labels = c("White layoffs this year",
                               "White layoffs 1 year ago",
                               "White layoffs 2 years ago",
                               "White layoffs 3 years ago",
                               "White layoffs 4 years ago",
                               "White layoffs 5 years ago"),
          title="Total alcohol and drug deaths regressed on white layoffs with year and county fixed effects",
          digits=6,
          column.sep.width = "-6pt",
          label="tb:white-fe")