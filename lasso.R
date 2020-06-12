## WARNING. THIS FILE DOES NOT PRODUCE OR CLEAN THE DATA.
## YOU MUST DO THIS BEFORE RUNNING IT. THIS FILE ONLY RUNS
## LASSO REGRESSIONS ON THE DATA AS WE HAVE FORMATTED

##### LASSO REGRESSION STUFF #####
library(glmnet)
library(caret)

##### LASSO REGRESSIONS FOR ALL DRUG/ALC DEATHS, INCOME
ad_lasso_df <- panel_df

## grab lags
ad_lasso_df$lag1 <- lag(panel_df$Total_layoff, 1)
ad_lasso_df$lag2 <- lag(panel_df$Total_layoff, 2)
ad_lasso_df$lag3 <- lag(panel_df$Total_layoff, 3)
ad_lasso_df$lag4 <- lag(panel_df$Total_layoff, 4)
ad_lasso_df$lag5 <- lag(panel_df$Total_layoff, 5)

## coerce to standard data.frame
ad_lasso_df <- data.frame(as.list(ad_lasso_df))

## drop NAs
ad_lasso_df <- na.omit(ad_lasso_df, cols="new_alc_and_drugs")

## columns of interest
all_vars <- c("Total_layoff", "lag1", "lag2", 
              "lag3", "lag4", "lag5", "income_pc",
              "State_county_FIPS", "Year",
              "new_alc_and_drugs")

## regularize data
preproc <- preProcess(ad_lasso_df[,all_vars],
                      method=c("center","scale"))
ad_lasso_df[,all_vars] <- predict(preproc, ad_lasso_df[,all_vars])

all_effects <- dummyVars(new_alc_and_drugs ~ ., data = ad_lasso_df[,all_vars])
all_effects <- predict(all_effects, ad_lasso_df[,all_vars])
all_effects <- as.matrix(all_effects)
all_outcome <- ad_lasso_df$new_alc_and_drugs


## use cross-validation to find optimal lambda
potent.lambdas <- 10^seq(-0.3,2, by=0.1)
ad_lasso_mod <- cv.glmnet(all_effects, all_outcome,
                       alpha=1, lambda=potent.lambdas,
                       standardize=TRUE)

plot(ad_lasso_mod, type="l")

## now that CV done, calculate coefficients and output
ad_lasso_mod_2 <- glmnet(all_effects, all_outcome,
                      alpha = 1,
                      standardize = TRUE,
                      lambda=potent.lambdas)
head(coef(ad_lasso_mod_2, ad_lasso_mod$lambda.min), 8)

##### DRUG LASSO #####
drug_lasso_df <- panel_df

## grab lags
drug_lasso_df$lag1 <- lag(panel_df$Total_layoff, 1)
drug_lasso_df$lag2 <- lag(panel_df$Total_layoff, 2)
drug_lasso_df$lag3 <- lag(panel_df$Total_layoff, 3)
drug_lasso_df$lag4 <- lag(panel_df$Total_layoff, 4)
drug_lasso_df$lag5 <- lag(panel_df$Total_layoff, 5)

## coerce to standard data.frame
drug_lasso_df <- data.frame(as.list(drug_lasso_df))

## drop NAs
drug_lasso_df <- na.omit(drug_lasso_df, cols="D")

## columns of interest
all_vars <- c("Total_layoff", "lag1", "lag2", 
              "lag3", "lag4", "lag5", "income_pc",
              "State_county_FIPS", "Year",
              "D")

## regularize data
preproc <- preProcess(drug_lasso_df[,all_vars],
                      method=c("center","scale"))
drug_lasso_df[,all_vars] <- predict(preproc, drug_lasso_df[,all_vars])

all_effects <- dummyVars(D ~ ., data = drug_lasso_df[,all_vars])
all_effects <- predict(all_effects, drug_lasso_df[,all_vars])
all_effects <- as.matrix(all_effects)
all_outcome <- drug_lasso_df$D


## use cross-validation to find optimal lambda
potent.lambdas <- 10^seq(-0.3,2, by=0.1)
drug_lasso_mod <- cv.glmnet(all_effects, all_outcome,
                            alpha=1, lambda=potent.lambdas,
                            standardize=TRUE)

plot(drug_lasso_mod, type="l")

## now that CV done, calculate coefficients and output
drug_lasso_mod_2 <- glmnet(all_effects, all_outcome,
                           alpha = 1,
                           standardize = TRUE,
                           lambda=potent.lambdas)
head(coef(drug_lasso_mod_2, drug_lasso_mod$lambda.min), 8)

##### ALCOHOL LASSO #####
alc_lasso_df <- panel_df

## grab lags
alc_lasso_df$lag1 <- lag(panel_df$Total_layoff, 1)
alc_lasso_df$lag2 <- lag(panel_df$Total_layoff, 2)
alc_lasso_df$lag3 <- lag(panel_df$Total_layoff, 3)
alc_lasso_df$lag4 <- lag(panel_df$Total_layoff, 4)
alc_lasso_df$lag5 <- lag(panel_df$Total_layoff, 5)

## coerce to standard data.frame
alc_lasso_df <- data.frame(as.list(alc_lasso_df))

## drop NAs
alc_lasso_df <- na.omit(alc_lasso_df, cols="D")

## columns of interest
all_vars <- c("Total_layoff", "lag1", "lag2", 
              "lag3", "lag4", "lag5", "income_pc",
              "State_county_FIPS", "Year",
              "A")

## regularize data
preproc <- preProcess(alc_lasso_df[,all_vars],
                      method=c("center","scale"))
alc_lasso_df[,all_vars] <- predict(preproc, alc_lasso_df[,all_vars])

all_effects <- dummyVars(A ~ ., data = alc_lasso_df[,all_vars])
all_effects <- predict(all_effects, alc_lasso_df[,all_vars])
all_effects <- as.matrix(all_effects)
all_outcome <- alc_lasso_df$A


## use cross-validation to find optimal lambda
potent.lambdas <- 10^seq(-0.3,2, by=0.1)
alc_lasso_mod <- cv.glmnet(all_effects, all_outcome,
                           alpha=1, lambda=potent.lambdas,
                           standardize=TRUE)

plot(alc_lasso_mod, type="l")

## now that CV done, calculate coefficients and output
alc_lasso_mod_2 <- glmnet(all_effects, all_outcome,
                          alpha = 1,
                          standardize = TRUE,
                          lambda=potent.lambdas)
head(coef(alc_lasso_mod_2, alc_lasso_mod$lambda.min), 8)

##### SUICIDE LASSO #####
suicide_lasso_df <- panel_df

## grab lags
suicide_lasso_df$lag1 <- lag(panel_df$Total_layoff, 1)
suicide_lasso_df$lag2 <- lag(panel_df$Total_layoff, 2)
suicide_lasso_df$lag3 <- lag(panel_df$Total_layoff, 3)
suicide_lasso_df$lag4 <- lag(panel_df$Total_layoff, 4)
suicide_lasso_df$lag5 <- lag(panel_df$Total_layoff, 5)

## coerce to standard data.frame
suicide_lasso_df <- data.frame(as.list(suicide_lasso_df))

## drop NAs
suicide_lasso_df <- na.omit(suicide_lasso_df, cols="Intentional_self_harm")

## columns of interest
all_vars <- c("Total_layoff", "lag1", "lag2", 
              "lag3", "lag4", "lag5", "income_pc",
              "State_county_FIPS", "Year",
              "Intentional_self_harm")

## regularize data
preproc <- preProcess(suicide_lasso_df[,all_vars],
                      method=c("center","scale"))
suicide_lasso_df[,all_vars] <- predict(preproc, suicide_lasso_df[,all_vars])

all_effects <- dummyVars(Intentional_self_harm ~ ., data = suicide_lasso_df[,all_vars])
all_effects <- predict(all_effects, suicide_lasso_df[,all_vars])
all_effects <- as.matrix(all_effects)
all_outcome <- suicide_lasso_df$Intentional_self_harm


## use cross-validation to find optimal lambda
potent.lambdas <- 10^seq(-0.3,2, by=0.1)
suicide_lasso_mod <- cv.glmnet(all_effects, all_outcome,
                               alpha=1, lambda=potent.lambdas,
                               standardize=TRUE)

plot(suicide_lasso_mod, type="l")

## now that CV done, calculate coefficients and output
suicide_lasso_mod_2 <- glmnet(all_effects, all_outcome,
                              alpha = 1,
                              standardize = TRUE,
                              lambda=potent.lambdas)
head(coef(suicide_lasso_mod_2, suicide_lasso_mod$lambda.min), 8)