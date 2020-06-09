#Because main.R was getting long, I decided to add another R file
#before running the merge function in this file, it is important to run main.R 
#to the point where we have main_df

s1_df <- read.delim("selfharm_assault_indeterminateintent_1999-2006.txt", sep = "\t")
s2_df <- read.delim("selfharm_assault_indeterminateintent_2006-2013.txt", sep = "\t")
