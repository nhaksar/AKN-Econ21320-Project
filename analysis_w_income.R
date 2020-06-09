#Again, b/c I felt main.R was getting long, I made this for doing the regressions w/income as a variable

##### Incorrect method of adding income #####
#inc_df <- read.csv("CAINC1__ALL_AREAS_1969_2018.csv")
#inc_df$description_w_unit <- paste(inc_df$Description, inc_df$Unit, sep = ", ")
#keeping only per capital income data (in $s)
#inc_df <- inc_df[inc_df$LineCode == 3,]
#getting rid of extra columns (& rows)
#inc_df <- inc_df[-which(is.na(inc_df$GeoFIPS)), -c(3, 4, 5, 6, 7, 8, 59)]
#inc_df <- inc_df[,-c(3:19)]       #this removes 1969-1985
#income_df <- melt(inc_df, id = c("GeoFIPS", "GeoName"))
#income_df$Year_i <- substr(as.character(income_df$variable), 2, 
#                           nchar(as.character(income_df$variable)))
#income_df <- income_df[,-c(3)]
#colnames(income_df)[3] <- "income_pc"
#income_df$GeoFIPS <- as.integer(as.character(income_df$GeoFIPS))
#income_df$ID <- paste(income_df$Year_i, income_df$GeoFIPS, sep = "_")


#Make sure to run stuff to create main_df before running this
#main_df <- merge(main_df, income_df, all.x = TRUE, by = "ID")

#####Make sure to run stuff to create main_df before running this######

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
#it appears that 1048 county-years have no per capita income data & have NAs; 
#96 of those 1048 rows have alcohol/drug deaths



#######beginning regressions########
panel_df <- pdata.frame(main_df, index=c("County_code", "Year"))

###### Repeating the OLS regressions w/income #######
mod_nofe_nolag <- plm(formula = total_deaths ~ Total_layoff + income_pc, data=panel_df)
mod_nofe_1lag <- plm(formula = total_deaths ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1), data=panel_df)
mod_nofe_2lag <- plm(formula = total_deaths ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2), data=panel_df)
mod_nofe_3lag <- plm(formula = total_deaths ~ Total_layoff + income_pc + 
                       lag(Total_layoff, 1) + lag(Total_layoff, 2) + 
                       lag(Total_layoff, 3), data=panel_df)

stargazer(mod_nofe_nolag, mod_nofe_1lag, mod_nofe_2lag, mod_nofe_3lag, 
          align=TRUE, no.space=TRUE, #omit.stat="f",
          dep.var.labels = c("Total alcohol and drug deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago"),
          title="Total alcohol and drug deaths regressed on total layoffs with Per Capita Income",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-nofe")

##### FE REGRESSIONS ON TOTAL LAYOFFS #####
# regressions stored as objects. use summary in console to view

## total deaths on total layoffs, no lag. within gives FE model
mod_nolag <- plm(formula = total_deaths ~ Total_layoff + income_pc, 
                 model="within", data = panel_df)

## total deaths on total layoffs + 1 year lag. FE model
mod_1lag <- plm(formula = total_deaths ~ Total_layoff + income_pc + lag(Total_layoff, 1),
                model="within", effect="twoways", data = panel_df)

## total deaths on total layoff + 1,2 year lag. FE model
mod_2lag <- plm(formula=total_deaths ~ Total_layoff + income_pc + lag(Total_layoff, 1) 
                + lag(Total_layoff, 2), model="within", effect="twoways",
                data=panel_df)

## total deaths on total layoff + 1,2,3 year lag. FE model
mod_3lag <- plm(formula=total_deaths ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3),
                model="within", effect="twoways",data=panel_df)

## total deaths on total layoff + 1,2,3,4 year lag. FE model
mod_4lag <- plm(formula=total_deaths ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4),
                model="within", effect="twoways", data=panel_df)
## total deaths on total layoff + 1,2,3,4,5 year lag. FE model
mod_5lag <- plm(formula=total_deaths ~ Total_layoff + income_pc + lag(Total_layoff, 1)
                + lag(Total_layoff, 2) + lag(Total_layoff, 3)
                + lag(Total_layoff, 4) + lag(Total_layoff, 5),
                model="within", effect="twoways", data=panel_df)

## generate LaTeX table
stargazer(mod_nolag, mod_1lag, mod_2lag, mod_3lag, mod_4lag, mod_5lag, 
          align=TRUE, no.space=TRUE, omit.stat=c("rsq","adj.rsq"),
          omit="Constant",
          dep.var.labels = c("Total alcohol and drug deaths"),
          covariate.labels = c("Total layoffs this year",
                               "Per Capita Income",
                               "Total layoffs 1 year ago",
                               "Total layoffs 2 years ago",
                               "Total layoffs 3 years ago",
                               "Total layoffs 4 years ago",
                               "Total layoffs 5 years ago"),
          title="Total alcohol and drug deaths regressed on total layoffs with Per Capita Income and year and county fixed effects",
          digits=6,
          column.sep.width = "-4pt",
          label="tb:total-fe")




