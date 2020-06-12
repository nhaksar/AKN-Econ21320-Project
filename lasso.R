## WARNING. THIS FILE DOES NOT PRODUCE OR CLEAN THE DATA.
## YOU MUST DO THIS BEFORE RUNNING IT. THIS FILE ONLY RUNS
## LASSO REGRESSIONS ON THE DATA AS WE HAVE FORMATTED

##### LASSO REGRESSION STUFF #####
library(glmnet)
library(caret)

##### LASSO REGRESSIONS FOR ALL
lasso_df <- panel_df

## grab lags
lasso_df$lag1 <- lag(panel_df$Total_layoff, 1)
lasso_df$lag2 <- lag(panel_df$Total_layoff, 2)
lasso_df$lag3 <- lag(panel_df$Total_layoff, 3)
lasso_df$lag4 <- lag(panel_df$Total_layoff, 4)
lasso_df$lag5 <- lag(panel_df$Total_layoff, 5)

## coerce to standard data.frame
lasso_df <- data.frame(as.list(lasso_df))

## drop NAs
lasso_df <- na.omit(lasso_df, cols="new_alc_and_drugs")

## columns of interest
all_vars <- c("Total_layoff", "lag1", "lag2", 
              "lag3", "lag4", "lag5", 
              "State_county_FIPS", "Year",
              "new_alc_and_drugs")

## regularize data
preproc <- preProcess(lasso_df[,all_vars],
                      method=c("center","scale"))
lasso_df[,all_vars] <- predict(preproc, lasso_df[,all_vars])

all_effects <- dummyVars(new_alc_and_drugs ~ ., data = lasso_df[,all_vars])
all_effects <- predict(all_effects, lasso_df[,all_vars])
all_effects <- as.matrix(all_effects)
all_outcome <- lasso_df$new_alc_and_drugs


## use cross-validation to find optimal lambda
potent.lambdas <- 10^seq(-0.3,2, by=0.1)
lasso_mod <- cv.glmnet(all_effects, all_outcome,
                       alpha=1, lambda=potent.lambdas,
                       standardize=TRUE)

plot(lasso_mod, type="l")

## now that CV done, calculate coefficients and output
lasso_mod_2 <- glmnet(all_effects, all_outcome,
                      alpha = 1,
                      standardize = TRUE,
                      lambda=potent.lambdas)
head(coef(lasso_mod_2, lasso_mod$lambda.min), 7)