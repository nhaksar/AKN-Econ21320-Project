## <cleaning.R>
## file for loading and cleaning the data

df <- read.xlsx("cntyic.xlsx", startRow=5, colNames=FALSE)

## past County name, all variables are for number of initial unemployment claimants
names(df) <- c("Year", "State FIPS", "County FIPS", "State_county FIPS",
               "County name", "Total", "White", "Black", "Hispanic origin",
               "American Indian or Alaskan Native", "Asian or Pacific islander",
               "Race N/A", "Men", "Women", "Gender N/A", "Under30","30-44",
               "45-54","55plus", "Age N/A")

## there are some entries with NA total entries; remove them
df <- df[!is.na(df$Total),]
