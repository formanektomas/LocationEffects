#####################################################################
# Dataset is not provided as it contains internal data of the retail chain
# .. however, model estimation procedure is shown.
# data transformed into a "long" form, using the "product" variable (58 categories used)
# 
library(lmtest)
library(sandwich)
library(dplyr)
library(tidyr)
library(lme4)
library(nlme)
library(lattice)
#
#####################################################################
# LME + heterosk. robust errors 
#
#
#
#
library(nlme)
models <- list()
for (ii in c(1,2,4,5,6)) { # 3rd cluster (Services) is omitted from estimation
  print("   ")
  print("New model -------------------------------------------------------------------------")
  print("cluster number")
  print(ii)
  DF_odh <- DF_long %>% 
    filter(cluster == ii)
  lm.fit <- lme(logsales~log(PopDens) + log(SalesArea) + log(mallDist) +  inMall 
                # sales are log-transformed in the dataframe used
                + Post10mWalk + Rstn5mWalk + Pharm10mWalk + cityPer + log(WageMedian) + cc_scaled, 
                random=~1|product, data=DF_long, method="ML")
  lm.fit <- update(lm.fit,weights=varPower(form=~fitted(.)))
  models[[ii]] <- lm.fit
  print("Pseudo R2")
  print(cor(DF_odh$sales, fitted(lm.fit))^2)
  print(summary(lm.fit))
  # print(dotplot(ranef(lm.fit, which = "product", condVar=T)))
}
#
#
library(stargazer)
# 
show_models <- c(1,2,4,5,6)
mdls <- models[show_models]
stargazer(type="text",mdls)     
#
#