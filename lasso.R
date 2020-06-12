## WARNING. THIS FILE DOES NOT PRODUCE OR CLEAN THE DATA.
## YOU MUST DO THIS BEFORE RUNNING IT. THIS FILE ONLY RUNS
## LASSO REGRESSIONS ON THE DATA AS WE HAVE FORMATTED

##### LASSO REGRESSION STUFF #####
library(glmnet)
library(caret)
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
lasso_df <- na.omit(lasso_df, cols="total_deaths")

## separate train/test samples
# ind <- sample(1:nrow(lasso_df), 0.7*nrow(lasso_df))
# train <- lasso_df[ind,]
# test <- lasso_df[-ind,]
# 
# train <- na.omit(train, cols="total_deaths")
# test <- na.omit(test, cols="total_deaths")

## columns of interest
regressors <- c("Total_layoff", "lag1", "lag2", 
                "lag3", "lag4", "lag5", 
                "State_county_FIPS", "Year")
outcome <- "total_deaths"
all_vars <- c("Total_layoff", "lag1", "lag2", 
              "lag3", "lag4", "lag5", 
              "State_county_FIPS", "Year",
              "total_deaths")

## regularize samples
# preproc <- preProcess(train[,all_vars],
#                       method=c("center","scale"))
# train[,all_vars] <- predict(preproc, train[,all_vars])
# test[,all_vars] <- predict(preproc, test[,all_vars])
preproc <- preProcess(lasso_df[,all_vars],
                      method=c("center","scale"))
lasso_df[,all_vars] <- predict(preproc, lasso_df[,all_vars])

all_effects <- dummyVars(total_deaths ~ ., data = lasso_df[,all_vars])
all_effects <- predict(all_effects, lasso_df[,all_vars])
all_effects <- as.matrix(all_effects)
# train_effects <- predict(all_effects, train[,all_vars])
# test_effects <- predict(all_effects, test[,all_vars])

## make data matrices
# train_effects <- as.matrix(train_effects)
# test_effects <- as.matrix(test_effects)
# 
# train_outcome <- train$total_deaths
# test_outcome <- test$total_deaths
all_outcome <- lasso_df$total_deaths


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