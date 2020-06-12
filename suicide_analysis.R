#Because main.R was getting long, I decided to add another R file
#before running the merge function in this file, it is important to run main.R 
#to the point where we have main_df



##### Cleaning Suicide Data #####
s1_df <- read.delim("selfharm_assault_indeterminateintent_1999-2006.txt", sep = "\t")
s2_df <- read.delim("selfharm_assault_indeterminateintent_2006-2013.txt", sep = "\t")
s_df <- rbind(s1_df, s2_df)
s_df <- unique(s_df[,-c(1,4,7,9,10)])
suicide_df <- dcast(s_df, County + County.Code + Year.Code ~ ICD.Sub.Chapter,
                    value.var = "Deaths")
suicide_df <- suicide_df[,-c(4)]
colnames(suicide_df)[5] <- "Undetermined_intent"
colnames(suicide_df)[6] <- "Intentional_self_harm"
suicide_df$intentional_and_undetermined <- suicide_df$Intentional_self_harm
suicide_df$intentional_and_undetermined[is.na(suicide_df$intentional_and_undetermined)] <- 0
suicide_df$Undetermined_intent[is.na(suicide_df$Undetermined_intent)] <- 0
suicide_df$intentional_and_undetermined <- suicide_df$intentional_and_undetermined + 
                                            suicide_df$Undetermined_intent
suicide_df <- suicide_df[-c(1),]
suicide_df$Undetermined_intent[suicide_df$Undetermined_intent == 0] <- NA
suicide_df$intentional_and_undetermined[suicide_df$intentional_and_undetermined == 0] <- NA

suicide_df$ID <- paste(suicide_df$Year.Code, suicide_df$County.Code, sep = "_")

###### Merging onto main_df ######
#Run main.R through line 152
main_df <- merge(main_df, suicide_df, all.x = TRUE, by="ID")
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


##### set up panel DF ######
panel_df <- pdata.frame(main_df, index=c("County_code", "Year"))



##### OLS REGRESSIONS ON TOTAL LAYOFFS #####
mod_nofe_nolag <- plm(formula = Intentional_self_harm ~ Total_layoff, data=panel_df)
mod_nofe_1lag <- plm(formula = Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1),
                     data=panel_df)
mod_nofe_2lag <- plm(formula = Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3), data=panel_df)
mod_nofe_4lag <- plm(formula = Intentional_self_harm ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4), data=panel_df)
mod_nofe_5lag <- plm(formula = Intentional_self_harm ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4) +
                       lag(Total_layoff, 5), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, mod_nofe_4lag, mod_nofe_5lag,
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total Deaths from Intentional Self Harm (Suicide) regressed on total layoffs",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")



##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = Intentional_self_harm ~ Total_layoff, 
                 model="within", effect = "twoways", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total deaths from intentional self harm (suicide) regressed on total layoffs with year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")

##### REGRESSIONS ON TOTAL LAYOFFS WITH YEAR-STATE FE #####
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag_ysfe <- plm(formula = Intentional_self_harm ~ Total_layoff + factor(state_years), 
                      model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag_ysfe <- plm(formula = Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                     + factor(state_years),
                     model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag_ysfe <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1) 
                     + lag(Total_layoff, 2) + factor(state_years), 
                     model="within", effect="twoways",
                     data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag_ysfe <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3) + factor(state_years),
                     model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag_ysfe <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag_ysfe <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + lag(Total_layoff, 5) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag_ysfe, mod_1lag_ysfe, mod_2lag_ysfe, mod_3lag_ysfe, mod_4lag_ysfe, 
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="([0-9]{1,2}_[0-9]{4})+",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from intentional self harm (suicide) regressed on total layoffs with year, county, and state-year fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-ysfe")

############## Suicide regressions w/ income ####################
##### OLS REGRESSIONS ON TOTAL LAYOFFS #####
mod_nofe_nolag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc, data=panel_df)
mod_nofe_1lag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                     data=panel_df)
mod_nofe_2lag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3), data=panel_df)
mod_nofe_4lag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4), data=panel_df)
mod_nofe_5lag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4) +
                       lag(Total_layoff, 5), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, mod_nofe_4lag, mod_nofe_5lag,
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total Deaths from Intentional Self Harm (Suicide) regressed on total layoffs with Per Capita Income",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")



##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc, 
                 model="within", effect = "twoways", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total deaths from intentional self harm (suicide) regressed on total layoffs with Per Capita Income and year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")

##### REGRESSIONS ON TOTAL LAYOFFS WITH YEAR-STATE FE #####
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag_ysfe <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + factor(state_years), 
                      model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag_ysfe <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + factor(state_years),
                     model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag_ysfe <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                     + lag(Total_layoff, 2) + factor(state_years), 
                     model="within", effect="twoways",
                     data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag_ysfe <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3) + factor(state_years),
                     model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag_ysfe <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag_ysfe <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + lag(Total_layoff, 5) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag_ysfe, mod_1lag_ysfe, mod_2lag_ysfe, mod_3lag_ysfe, mod_4lag_ysfe, 
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="([0-9]{1,2}_[0-9]{4})+",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income", 
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from intentional self harm (suicide) regressed on total layoffs with Per Capita Income and year, county, and state-year fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-ysfe")


############## Suicide Regressions w/ single factor FEs###############
##### with individual effects only ########
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = Intentional_self_harm ~ Total_layoff, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1),
                model="within", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from intentional self harm (suicide) regressed on total layoffs with county fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")



######## with time effects only ############
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = Intentional_self_harm ~ Total_layoff, 
                 model="within", effect="time", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1),
                model="within", effect="time", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="time",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="time",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="time", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=Intentional_self_harm ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="time", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from intentional self harm (suicide) regressed on total layoffs with year fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")


##### with income & individual effects only ########
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from intentional self harm (suicide) regressed on total layoffs with Per Capita Incomecounty fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")



######## with income & time effects only ############
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc, 
                 model="within", effect="time", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", effect="time", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="time",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="time",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="time", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=Intentional_self_harm ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="time", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Suicide"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from intentional self harm (suicide) regressed on total layoffs with Per Capita Income and year fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")


###################


