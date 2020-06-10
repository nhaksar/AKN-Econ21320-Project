#Again I feel like main.R is getting long, so I made a new file for running things w/ just one set of FEs


##### with individual effects only ########
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = total_deaths ~ Total_layoff, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = total_deaths ~ Total_layoff + lag(Total_layoff, 1),
                model="within", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", data=panel_df)

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
          title="Total alcohol and drug deaths regressed on total layoffs with county fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")



######## with time effects only ############
## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = total_deaths ~ Total_layoff, 
                 model="within", effect="time", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = total_deaths ~ Total_layoff + lag(Total_layoff, 1),
                model="within", effect="time", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="time",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="time",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="time", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=total_deaths ~ Total_layoff + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="time", data=panel_df)

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
          title="Total alcohol and drug deaths regressed on total layoffs with year fixed effects only",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")
