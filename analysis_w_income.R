#Again, b/c I felt main.R was getting long, I made this for doing the regressions w/income as a variable
inc_df <- read.csv("CAINC1__ALL_AREAS_1969_2018.csv")
inc_df$description_w_unit <- paste(inc_df$Description, inc_df$Unit, sep = ", ")
#keeping only per capital income data (in $s)
inc_df <- inc_df[inc_df$LineCode == 3,]
#getting rid of extra columns (& rows)
inc_df <- inc_df[-which(is.na(inc_df$GeoFIPS)), -c(3, 4, 5, 6, 7, 8, 59)]
#removing some earlier years in order to figure out where the warning 
#message is coming from (message: "attributes are not identical across measure 
#variables; they will be dropped"); maybe from changing counties --> na values?
inc_df <- inc_df[,-c(3:19)]       #this removes 1969-1985
income_df <- melt(inc_df, id = c("GeoFIPS", "GeoName"))
