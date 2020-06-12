###using the one column alcohol & drug data ###
ad1_df <- read.delim("alc_drug_onecol_1999-2002.txt", sep = "\t")
ad2_df <- read.delim("alc_drug_onecol_2003-2007.txt", sep = "\t")
ad3_df <- read.delim("alc_drug_onecol_2008-2013.txt", sep = "\t")
ad_df <- rbind(ad1_df, ad2_df, ad3_df)
ad_df <- ad_df[-(which(is.na(ad_df$County.Code))), -c(1,5,7,8)]
colnames(ad_df)[4] <- "new_alc_and_drugs"
ad_df$ID <- paste(ad_df$Year, ad_df$County.Code, sep = "_")


###### Merging onto main_df ######
#Run main.R through line 152
main_df <- merge(main_df, ad_df, all.x = TRUE, by="ID")
main_df$Year <- main_df$Year.x
main_df <- subset(main_df, select = - c(Year.x,Year.y))
for (i in 9:23){
  main_df[is.na(main_df[i]),i] = 0
}

## make some things into factors
## call names(main_df) to see
for (i in 1:8){
  main_df[,i] <- as.factor(main_df[,i])
}
main_df$Year <- as.factor(main_df$Year)

#### just to have the code for the income stuff here as well
income_df <- read.csv("CAINC1__ALL_AREAS_1969_2018.csv")
income_df$Description_Code <- as.integer(income_df$Description)
pcincs_only_df <- income_df[(income_df$Description_Code==2),]
pcincs_only_df$GeoFIPS <- as.integer(as.character(pcincs_only_df$GeoFIPS))
main_df$income_pc <- NA
for (row in c(1:nrow(main_df))) {
  rel_code <- main_df$County_code[row]
  rel_yr <- paste("X", main_df$Year[row], sep = "")
  main_df$income_pc[row] <- pcincs_only_df[match(rel_code, 
                                                 pcincs_only_df$GeoFIPS), 
                                           match(rel_yr, 
                                                 colnames(pcincs_only_df))] %>% as.character() %>% as.integer()
}



panel_df <- pdata.frame(main_df, index=c("County_code", "Year"))

nofe_df <- panel_df
nofe_df$lag1 <- lag(panel_df$Total_layoff, 1)
nofe_df$lag2 <- lag(panel_df$Total_layoff, 2)
nofe_df$lag3 <- lag(panel_df$Total_layoff, 3)
nofe_df$lag4 <- lag(panel_df$Total_layoff, 4)
nofe_df$lag5 <- lag(panel_df$Total_layoff, 5)


##### OLS REGRESSIONS ON TOTAL LAYOFFS #####
mod_nofe_nolag <- lm(formula = new_alc_and_drugs ~ Total_layoff, data=nofe_df)
mod_nofe_1lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1),
                     data=panel_df)
mod_nofe_2lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3), data=panel_df)
mod_nofe_4lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4), data=panel_df)
mod_nofe_5lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4) +
                       lag(Total_layoff, 5), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, mod_nofe_4lag, mod_nofe_5lag,
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")



##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = new_alc_and_drugs ~ Total_layoff, 
                 model="within", effect = "twoways", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs with year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")

##### REGRESSIONS ON TOTAL LAYOFFS WITH YEAR-STATE FE #####
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag_ysfe <- plm(formula = new_alc_and_drugs ~ Total_layoff + factor(state_years), 
                      model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag_ysfe <- plm(formula = new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                     + factor(state_years),
                     model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag_ysfe <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1) 
                     + lag(Total_layoff, 2) + factor(state_years), 
                     model="within", effect="twoways",
                     data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag_ysfe <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3) + factor(state_years),
                     model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag_ysfe <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag_ysfe <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + lag(Total_layoff, 5) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag_ysfe, mod_1lag_ysfe, mod_2lag_ysfe, mod_3lag_ysfe, mod_4lag_ysfe, 
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="([0-9]{1,2}_[0-9]{4})+",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs with year, county, and state-year fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-ysfe")

############## Alcohol & Drugs regressions w/ income ####################
##### OLS REGRESSIONS ON TOTAL LAYOFFS #####
mod_nofe_nolag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc, data=panel_df)
mod_nofe_1lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                     data=panel_df)
mod_nofe_2lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3), data=panel_df)
mod_nofe_4lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4), data=panel_df)
mod_nofe_5lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4) +
                       lag(Total_layoff, 5), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, mod_nofe_4lag, mod_nofe_5lag,
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs with Per Capita Income",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")



##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc, 
                 model="within", effect = "twoways", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs with Per Capita Income and year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")

##### REGRESSIONS ON TOTAL LAYOFFS WITH YEAR-STATE FE #####
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag_ysfe <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + factor(state_years), 
                      model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag_ysfe <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + factor(state_years),
                     model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag_ysfe <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                     + lag(Total_layoff, 2) + factor(state_years), 
                     model="within", effect="twoways",
                     data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag_ysfe <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3) + factor(state_years),
                     model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag_ysfe <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag_ysfe <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + lag(Total_layoff, 5) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag_ysfe, mod_1lag_ysfe, mod_2lag_ysfe, mod_3lag_ysfe, mod_4lag_ysfe, 
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="([0-9]{1,2}_[0-9]{4})+",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income", 
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs with Per Capita Income and year, county, and state-year fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-ysfe")


############## Alcohol & Drugs Regressions w/ single factor FEs###############
##### with individual effects only ########
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = new_alc_and_drugs ~ Total_layoff, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1),
                model="within", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs with county fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")



######## with time effects only ############
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = new_alc_and_drugs ~ Total_layoff, 
                 model="within", effect="time", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1),
                model="within", effect="time", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="time",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="time",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="time", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="time", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs with year fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")


##### with income & individual effects only ########
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs with Per Capita Incomecounty fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")



######## with income & time effects only ############
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc, 
                 model="within", effect="time", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", effect="time", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="time",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="time",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="time", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=new_alc_and_drugs ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="time", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol and Drugs regressed on total layoffs with Per Capita Income and year fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")


##### LASSO SUGGESTED REGRESSIONS #####
mod_lasso_tim <- plm(formula = new_alc_and_drugs ~ lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 4)
                     + lag(Total_layoff, 5), data=panel_df,
                     model="within", effect="time")
mod_lasso_ind <- plm(formula = new_alc_and_drugs ~ lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 4)
                     + lag(Total_layoff, 5), data=panel_df,
                     model="within", effect="individual")
mod_lasso_2fe <- plm(formula = new_alc_and_drugs ~ lag(Total_layoff, 1)
                 + lag(Total_layoff, 2) + lag(Total_layoff, 4)
                 + lag(Total_layoff, 5), data=panel_df,
                 model="within", effect="twoways")
stargazer(mod_lasso_ind, mod_lasso_tim, mod_lasso_2fe, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Alcohol and Drugs"),
          covariate.labels = c("Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="LASSO-suggested regression with FE",
          digits=6,
          add.lines = list(c("Time FE", "No", "Yes", "Yes"),
                           c("County FE", "Yes", "No", "Yes")),
          column.sep.width = "-4pt",
          label="tb:lasso-alcdrug-fe")
