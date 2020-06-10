#Run main.r through 173 first
main_df$A[main_df$A == 0] <- NA
main_df$D[main_df$D == 0] <- NA
panel_df <- pdata.frame(main_df, index=c("County_code", "Year"))
#########Running stuff on just alcohol##########
##### OLS REGRESSIONS ON TOTAL LAYOFFS #####
mod_nofe_nolag <- plm(formula = A ~ Total_layoff, data=panel_df)
mod_nofe_1lag <- plm(formula = A ~ Total_layoff + lag(Total_layoff, 1),
                     data=panel_df)
mod_nofe_2lag <- plm(formula = A ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = A ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3), data=panel_df)
mod_nofe_4lag <- plm(formula = A ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4), data=panel_df)
mod_nofe_5lag <- plm(formula = A ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4) +
                       lag(Total_layoff, 5), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, mod_nofe_4lag, mod_nofe_5lag,
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total Deaths from Alcohol"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total Deaths from Alcohol regressed on total layoffs",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")



##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = A ~ Total_layoff, 
                 model="within", effect = "twoways", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = A ~ Total_layoff + lag(Total_layoff, 1),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Alcohol"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol regressed on total layoffs with year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")

##### REGRESSIONS ON TOTAL LAYOFFS WITH YEAR-STATE FE #####
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag_ysfe <- plm(formula = A ~ Total_layoff + factor(state_years), 
                      model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag_ysfe <- plm(formula = A ~ Total_layoff + lag(Total_layoff, 1)
                     + factor(state_years),
                     model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag_ysfe <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1) 
                     + lag(Total_layoff, 2) + factor(state_years), 
                     model="within", effect="twoways",
                     data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag_ysfe <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3) + factor(state_years),
                     model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag_ysfe <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag_ysfe <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + lag(Total_layoff, 5) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag_ysfe, mod_1lag_ysfe, mod_2lag_ysfe, mod_3lag_ysfe, mod_4lag_ysfe, 
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="([0-9]{1,2}_[0-9]{4})+",
          dep.var.labels = c("Total Deaths from Alcohol"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol regressed on total layoffs with year, county, and state-year fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-ysfe")

############## Alcohol regressions w/ income ####################
##### OLS REGRESSIONS ON TOTAL LAYOFFS #####
mod_nofe_nolag <- plm(formula = A ~ Total_layoff + income_pc, data=panel_df)
mod_nofe_1lag <- plm(formula = A ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                     data=panel_df)
mod_nofe_2lag <- plm(formula = A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3), data=panel_df)
mod_nofe_4lag <- plm(formula = A ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4), data=panel_df)
mod_nofe_5lag <- plm(formula = A ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4) +
                       lag(Total_layoff, 5), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, mod_nofe_4lag, mod_nofe_5lag,
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total Deaths from Alcohol"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total Deaths from Alcohol regressed on total layoffs with Per Capita Income",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")



##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = A ~ Total_layoff + income_pc, 
                 model="within", effect = "twoways", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = A ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Deaths from Alcohol"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol regressed on total layoffs with Per Capita Income and year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")

##### REGRESSIONS ON TOTAL LAYOFFS WITH YEAR-STATE FE #####
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag_ysfe <- plm(formula = A ~ Total_layoff + income_pc + factor(state_years), 
                      model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag_ysfe <- plm(formula = A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + factor(state_years),
                     model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag_ysfe <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                     + lag(Total_layoff, 2) + factor(state_years), 
                     model="within", effect="twoways",
                     data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag_ysfe <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3) + factor(state_years),
                     model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag_ysfe <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag_ysfe <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + lag(Total_layoff, 5) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag_ysfe, mod_1lag_ysfe, mod_2lag_ysfe, mod_3lag_ysfe, mod_4lag_ysfe, 
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="([0-9]{1,2}_[0-9]{4})+",
          dep.var.labels = c("Total Deaths from Alcohol"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income", 
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Deaths from Alcohol regressed on total layoffs with Per Capita Income and year, county, and state-year fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-ysfe")






############## Alcohol Regressions w/ single factor FEs###############
##### with individual effects only ########
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = A ~ Total_layoff, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = A ~ Total_layoff + lag(Total_layoff, 1),
                model="within", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total alcohol deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total alcohol deaths regressed on total layoffs with county fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")



######## with time effects only ############
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = A ~ Total_layoff, 
                 model="within", effect="time", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = A ~ Total_layoff + lag(Total_layoff, 1),
                model="within", effect="time", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="time",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="time",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="time", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=A ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="time", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total alcohol deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total alcohol deaths regressed on total layoffs with year fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")


##### with income & individual effects only ########
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = A ~ Total_layoff + income_pc, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = A ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total alcohol deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total alcohol deaths regressed on total layoffs with Per Capita Incomecounty fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")



######## with income & time effects only ############
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = A ~ Total_layoff + income_pc, 
                 model="within", effect="time", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = A ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", effect="time", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="time",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="time",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="time", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=A ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="time", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total alcohol deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total alcohol deaths regressed on total layoffs with Per Capita Income and year fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")






#########Running stuff on just drugs##########
##### OLS REGRESSIONS ON TOTAL LAYOFFS #####
mod_nofe_nolag <- plm(formula = D ~ Total_layoff, data=panel_df)
mod_nofe_1lag <- plm(formula = D ~ Total_layoff + lag(Total_layoff, 1),
                     data=panel_df)
mod_nofe_2lag <- plm(formula = D ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = D ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3), data=panel_df)
mod_nofe_4lag <- plm(formula = D ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4), data=panel_df)
mod_nofe_5lag <- plm(formula = D ~ Total_layoff + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4) +
                       lag(Total_layoff, 5), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, mod_nofe_4lag, mod_nofe_5lag,
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total Drug Deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total Drug Deaths regressed on total layoffs",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")



##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = D ~ Total_layoff, 
                 model="within", effect = "twoways", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = D ~ Total_layoff + lag(Total_layoff, 1),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Drug Deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Drug Deaths regressed on total layoffs with year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")

##### REGRESSIONS ON TOTAL LAYOFFS WITH YEAR-STATE FE #####
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag_ysfe <- plm(formula = D ~ Total_layoff + factor(state_years), 
                      model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag_ysfe <- plm(formula = D ~ Total_layoff + lag(Total_layoff, 1)
                     + factor(state_years),
                     model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag_ysfe <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1) 
                     + lag(Total_layoff, 2) + factor(state_years), 
                     model="within", effect="twoways",
                     data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag_ysfe <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3) + factor(state_years),
                     model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag_ysfe <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag_ysfe <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + lag(Total_layoff, 5) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag_ysfe, mod_1lag_ysfe, mod_2lag_ysfe, mod_3lag_ysfe, mod_4lag_ysfe, 
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="([0-9]{1,2}_[0-9]{4})+",
          dep.var.labels = c("Total Drug Deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Drug Deaths regressed on total layoffs with year, county, and state-year fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-ysfe")

############## Drugs regressions w/ income ####################
##### OLS REGRESSIONS ON TOTAL LAYOFFS #####
mod_nofe_nolag <- plm(formula = D ~ Total_layoff + income_pc, data=panel_df)
mod_nofe_1lag <- plm(formula = D ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                     data=panel_df)
mod_nofe_2lag <- plm(formula = D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3), data=panel_df)
mod_nofe_4lag <- plm(formula = D ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4), data=panel_df)
mod_nofe_5lag <- plm(formula = D ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3) + lag(Total_layoff, 4) +
                       lag(Total_layoff, 5), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, mod_nofe_4lag, mod_nofe_5lag,
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total Drug Deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total Drug Deaths regressed on total layoffs with Per Capita Income",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")



##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = D ~ Total_layoff + income_pc, 
                 model="within", effect = "twoways", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = D ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Drug Deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Drug Deaths regressed on total layoffs with Per Capita Income and year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")

##### REGRESSIONS ON TOTAL LAYOFFS WITH YEAR-STATE FE #####
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag_ysfe <- plm(formula = D ~ Total_layoff + income_pc + factor(state_years), 
                      model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag_ysfe <- plm(formula = D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + factor(state_years),
                     model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag_ysfe <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                     + lag(Total_layoff, 2) + factor(state_years), 
                     model="within", effect="twoways",
                     data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag_ysfe <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3) + factor(state_years),
                     model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag_ysfe <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag_ysfe <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                     + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                     + lag(Total_layoff, 4) + lag(Total_layoff, 5) + factor(state_years),
                     model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag_ysfe, mod_1lag_ysfe, mod_2lag_ysfe, mod_3lag_ysfe, mod_4lag_ysfe, 
          mod_5lag_ysfe, align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="([0-9]{1,2}_[0-9]{4})+",
          dep.var.labels = c("Total Drug Deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income", 
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Drug Deaths regressed on total layoffs with Per Capita Income and year, county, and state-year fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-ysfe")






############## Drugs Regressions w/ single factor FEs###############
##### with individual effects only ########
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = D ~ Total_layoff, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = D ~ Total_layoff + lag(Total_layoff, 1),
                model="within", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Drug deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Drug deaths regressed on total layoffs with county fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")



######## with time effects only ############
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = D ~ Total_layoff, 
                 model="within", effect="time", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = D ~ Total_layoff + lag(Total_layoff, 1),
                model="within", effect="time", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="time",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="time",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="time", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=D ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="time", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Drug deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Drug deaths regressed on total layoffs with year fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")


##### with income & individual effects only ########
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = D ~ Total_layoff + income_pc, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = D ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Drug deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Drug deaths regressed on total layoffs with Per Capita Incomecounty fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")



######## with income & time effects only ############
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = D ~ Total_layoff + income_pc, 
                 model="within", effect="time", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = D ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", effect="time", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="time",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="time",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="time", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=D ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="time", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total Drug deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total Drug deaths regressed on total layoffs with Per Capita Income and year fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")





