## <cleaning.R>
## file for loading and cleaning the data

#mls <- read.xlsx("cntyic.xlsx", startRow=5, colNames=FALSE)
mls <- read.csv("cntyic.csv")

## past County name, all variables are for number of initial unemployment claimants
names(mls) <- c("Year", "State FIPS", "County FIPS", "State_county FIPS",
               "County name", "Total", "White", "Black", "Hispanic origin",
               "American Indian or Alaskan Native", "Asian or Pacific islander",
               "Race N/A", "Men", "Women", "Gender N/A", "Under30","30-44",
               "45-54","55plus", "Age N/A")

## there are some entries with NA total entries; remove them
mls <- mls[!is.na(mls$Total),]

## remove entries before 1999
mls <- mls[(mls$Year >= 1999),]
