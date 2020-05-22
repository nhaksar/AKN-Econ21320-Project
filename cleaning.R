## <cleaning.R>
## file for loading and cleaning the data

##### handle IPUMS data #####
#?Not sure what this is about-Alena
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

# ddi <- read_ipums_ddi("usa_00004.xml")
# ipums_data <- read_ipums_micro(ddi)

#mls <- read.xlsx("cntyic.xlsx", startRow=5, colNames=FALSE)
mls <- read.csv("mls.csv")

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


