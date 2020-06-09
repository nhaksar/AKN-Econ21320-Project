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


