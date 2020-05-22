## <merge.R>
## R file for merging datasets. run after running cleaning.R

##### first, we create unique IDS that will be same across datasets #####
mls$ID <- paste(mls$Year, mls$`State FIPS`, mls$`County FIPS`, sep="")
#I'm confused by this b/c there's no data to load in this file?

