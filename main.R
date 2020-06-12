library("readxl")
library("tidyverse")
library("reshape2")
library("stargazer")
#if (!require("plm")) install.packages("plm")
library("plm")
library("ggplot2")

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
#to fix the na's in the total_deaths
cleaned_drug_df$A[is.na(cleaned_drug_df$A)] <- 0
cleaned_drug_df$D[is.na(cleaned_drug_df$D)] <- 0
##############





##### Making a column of IDs to merge stuff onto #####
df_for_ids <- read.delim("For_County_IDs.txt", sep = "\t")
df_for_ids <- df_for_ids[-which(is.na(df_for_ids$County.Code)),]
county_ids <- df_for_ids$County.Code
years <- c(1997:2013)
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

## make some things into factors
## call names(main_df) to see
for (i in 1:8){
  main_df[,i] <- as.factor(main_df[,i])
}
main_df$Year <- as.factor(main_df$Year)


##### set up panel DF ######
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

## split dataframe by year
#annual_df <- split(main_df, unique(main_df$Year))
# ann_stats <- c()
# race_breakdown <- c("White_layoff","Black_layoff","Hispanic_orgin_layoff",
#                     "American_indian_or_Alaskan_native_layoff",
#                     "Asian_or_Pacific_islander_layoff", "Year")
# options(scipen = 999)
# for (year in unique(main_df$Year)){
#   this_year <- c()
#   for (i in 1:5){
#     this_year <- cbind(this_year, sum(main_df[main_df$Year == year, names(main_df)[9+i]]))
#   }
#   this_year <- cbind(this_year, year)
#   ann_stats <- rbind(ann_stats, this_year)
# }
ann_stats <- c()
race_breakdown <- c("White","Black","Hispanic origin",
                    "American Indian or Alaskan Native",
                    "Asian or Pacific Islander")
options(scipen = 999)
for (year in unique(main_df$Year)){
  for (i in 1:5){
    ann_stats <- rbind(ann_stats, cbind(year, race_breakdown[i], sum(main_df[main_df$Year == year, names(main_df)[9+i]])))
  }
}
ann_stats <- data.frame(ann_stats)
names(ann_stats) <- c("Year", "Race", "Layoffs")
ann_stats <- ann_stats[order(Year, Race),]
ann_stats$Layoffs <- as.numeric(as.character(ann_stats$Layoffs))
ann_stats <- ann_stats[1:80,]

p <- ggplot(data=ann_stats, aes(x=Year)) + geom_bar(aes(y=Layoffs,fill=Race), stat="identity")
p <- p + ggtitle("Mass Layoffs 1997-2012") + ylab("Mass Layoffs") + theme(axis.text.x = element_text(angle = 90))

##### OLS REGRESSIONS ON TOTAL LAYOFFS #####
mod_nofe_nolag <- plm(formula = total_deaths ~ Total_layoff, data=panel_df)
mod_nofe_1lag <- plm(formula = total_deaths ~ Total_layoff + lag(Total_layoff, 1),
                    data=panel_df)
mod_nofe_2lag <- plm(formula = total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                    + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                    + lag(Total_layoff, 2) + lag(Total_layoff, 3), data=panel_df)
mod_nofe_4lag <- plm(formula = total_deaths ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4), data=panel_df)
mod_nofe_5lag <- plm(formula = total_deaths ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4) +
                       lag(Total_layoff, 5), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, mod_nofe_4lag, mod_nofe_5lag,
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total alcohol and drug deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total alcohol and drug deaths regressed on total layoffs",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")

##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = total_deaths ~ Total_layoff, 
                 model="within", effect = "twoways", data = panel_df)

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
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
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

##### REGRESSIONS ON TOTAL LAYOFFS WITH FE, NO CURRENT #####
mod_nocurr_5lag <- plm(formula=total_deaths ~ lag(Total_layoff, 1)
                       + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                       + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                       model="within", effect="twoways", data=panel_df)

stargazer(mod_5lag, mod_nocurr_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
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
          label="tb:update-fe")

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
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
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

########Computing #s for Data Description Table########
cleaned_drug_df$Total <- cleaned_drug_df$A + cleaned_drug_df$D
mean(cleaned_drug_df$Total)
mean(cleaned_drug_df$A[cleaned_drug_df$A != 0])
mean(cleaned_drug_df$D[cleaned_drug_df$D != 0])
quantile(cleaned_drug_df$Total)
quantile(cleaned_drug_df$A[cleaned_drug_df$A != 0])
quantile(cleaned_drug_df$D[cleaned_drug_df$D != 0])
length(cleaned_drug_df$A[cleaned_drug_df$A != 0])
length(cleaned_drug_df$D[cleaned_drug_df$D != 0])

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


