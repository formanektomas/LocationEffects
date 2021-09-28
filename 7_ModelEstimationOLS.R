#####################################################################
library(lmtest)
library(sandwich)
#
# Dataset is not provided as it contains internal data of the retail chain
# .. however, model estimation procedure is shown.
# .. from the dataset, we make two separate dataframes:
#    deps: contains 58 columns with sales for each category/store
#    regressors: contains a set of all regressors used for estimation
#    .. clearly, rows (observations) from the two dataframes match each other
# 
#
# model formula follows from article
formula1 <- y ~ log(PopDens) + log(StoreArea) + log(mallDist) +  inMall + Post10mWalk + Rstn5mWalk + Pharm10mWalk + cityPer + log(WageMedian) + cc_scaled
#
models <- list()
for (ii in 1:58) { #
  print("   ")
  print("New model -------------------------------------------------------------------------")
  print(c("dependent variable is", colnames(deps)[ii]))
  y <- deps[ , ii]
  lm.fit <- lm(formula1, data=regressors)  
  models[[ii]] <- lm.fit
  print("R2")
  print(summary(lm.fit)$r.squared)
  print(coeftest(lm.fit, vcov = vcovHC(lm.fit, "HC1")))
}
#####################################################################
# Export estimates in "nice" format
library(stargazer)
# show estimates for six categories
show_models <- c(1,2,3,4,5,6)
# numbers are illustrative, selects ids of 6 products from the 1:58 range of dependent variables"
mdls <- models[show_models]
se_robust <- function(x)
  coeftest(x, vcov = vcovHC(x, "HC1"))[, 2]
stargazer(type="text",mdls, se = lapply(mdls, se_robust)) # also, type="latex"          

