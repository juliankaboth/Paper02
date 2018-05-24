# Preliminary remarks ####
## Variables for data are 4 digits
## Variables for valuation parameters are 3 digits
install.packages("doParallel")
# Packages Load ####
library(readxl)
library(dplyr)
library(tidyr)
library(qrmtools)
library(quantmod)
library(scales)
library(ggplot2)
library(rJava)
library(openxlsx)
library(broom)
library(car)
library(leaps)
library(foreach)
library(doSNOW)
library(doParallel)

# Input ####
path <- "C:/Users/jkabo/Dropbox/HHL - Max, Julian/05 Liquidationspr?ferenzen"
setwd(path)

source("Solver.R")
source("Discount.R")

## Load Empirical Data #### incl. date formating
### Check if there are any "n/a", "n.a." in Excel and replace them with missing value (empty cell)
tmp1_data <- read_excel("C:/Users/jkabo/Dropbox/HHL - Max, Julian/05 Liquidationspr?ferenzen/DataBase.xlsx", sheet=1) %>%
  mutate(DateNotarized=as.Date(DateNotarized, format="%d-%m-%Y")) %>%
  mutate(DateCapture=as.Date(DateCapture, format="%d-%m-%Y")) %>%
  mutate(VentureIncorporation=as.Date(VentureIncorporation, format="%d-%m-%Y")) %>%
  mutate(CapTableDate=as.Date(CapTableDate, format="%d-%m-%Y"))

# Calc per company - tbd
cmpy <- c(1:8) #
ccy <- c(2,3,8,10,17,18,19,20,22,23,26,27,28,29,30,31,32,34,35,36,37,39,40,41,42,44,45,47,49,50,51,52)#unique(unlist(temp_cmpy))
c(1:8,9:15,17:52)


cy <- 1 #9999
sg <- 0.4
rf <- 0.01
te <- 4

data_regu <- data.frame()

for(b in 1:length(cmpy)){
  cy <- cmpy[b]
  for (a in 1:length(sigm)){
    sg <- sigm[a]
    for (c in 1:length(timeexit)){
      te <- timeexit[c]
      for (d in 1:length(riskfree)){
      rf <- riskfree[c]
      temp_root <- uniroot(dsct_slve, interval=c(1, 9000000000), tol=0.1)
      temp_valu <- temp_root$root
      tmp1 <- dsct_rslt(temp_valu)
      temp_cy <- c(rep(cy, length(unlist(tmp1$LP_Amounts))))
      temp_sg <- c(rep(sg, length(unlist(tmp1$LP_Amounts))))
      temp_te <- c(rep(te, length(unlist(tmp1$LP_Amounts))))
      temp_indx <- c(unlist(tmp1$Index$ShareClassIndex))
      temp_dsct <- c(unlist(tmp1$Discount))
      temp_snry <- c(unlist(tmp1$Seniority))
      temp_liqp <- c(unlist(tmp1$LP_Amounts))
      temp_allo <- c(unlist(tmp1$Allocation_Base$LPAllocation_allo))
      temp_pmv <- c(rep(unlist(tmp1$PMV_Value), length(tmp1$LP_Amounts)))
      temp_sig <- c(rep(tmp1$Scenario$Volatility, length(tmp1$LP_Amounts)))
      temp_tte <- c(rep(tmp1$Scenario$TimeToExit, length(tmp1$LP_Amounts)))
      temp_inv <- c(rep(tmp1$Total_Invest, length(tmp1$LP_Amounts)))
      temp_rela <- c(unlist(tmp1$Share_Invest))
      temp_rels <- c(unlist(tmp1$Share_Shares))
      temp_fmly <- c(unlist(tmp1$Investor_Family))
      temp_lown <- c(unlist(tmp1$LP_Own))
      temp_ltot <- c(rep(tmp1$Total_Liqp, length(tmp1$LP_Amounts)))
      temp_rf <- c(rep(rf, length(unlist(tmp1$LP_Amounts))))
      dsct_cy <- rbind(temp_cy, temp_sg, temp_te,temp_indx, temp_dsct, temp_snry, temp_liqp, temp_allo, temp_inv, temp_pmv, temp_sig, temp_tte, temp_rela, temp_rels, temp_fmly, temp_rf)
      data_regu <- rbind(data_regr, t(dsct_cy))
      }
    }  
  }
}



write.xlsx(data_regr, file = paste(b,"save.xlsx"), colNames = TRUE, borders = "columns")
dd <- as.data.frame(matrix(unlist(data_regr), nrow=length(unlist(data_regr[1]))))

hh <- c(colnames(data_regr), "temp_liqp.temp_pmv", "temp_inv.temp_pmv", "temp_liqp.temp_inv")
ee <- dd %>%
      mutate(V1=as.numeric(levels(V1))[V1],
             V2=as.numeric(levels(V2))[V2],
             V3=as.numeric(levels(V3))[V3],
             V4=as.numeric(levels(V4))[V4],
             V5=as.numeric(levels(V5))[V5],
             V6=as.numeric(levels(V6))[V6],
             V7=as.numeric(levels(V7))[V7],
             V9=as.numeric(levels(V9))[V9],
             V10=as.numeric(levels(V10))[V10],
             V11=as.numeric(levels(V11))[V11],
             V12=as.numeric(levels(V12))[V12],
             V13=as.numeric(levels(V13))[V13],
             V14=as.numeric(levels(V14))[V14],
             V15=as.numeric(levels(V15))[V15],
             V16=as.numeric(levels(V16))[V16],
             V17=as.numeric(levels(V17))[V17],
             V18=as.numeric(levels(V18))[V18])
      mutate(V19=V7/V10,V20=V9/V10,V21=V7/V9) %>% 
      filter()

colnames(ee) <- hh

ggplot(ee, aes(x=))

ee <- ee %>%
      filter(temp_cy %in% ccy)

company <- cmpy
sigm <- seq(from=0.45, to=0.9, by=0.45)
riskfree <- seq(from=0.01, to=0.05, by=0.04)
timeexit <- seq(from=2.00, to=5.00, by=3.00) 

regr_model <- lm(temp_dsct ~  temp_liqp.temp_inv + temp_rela , data=ee)
summary(regr_model)
ff <- augment(regr_model)

ggplot(ff, aes(x=.fitted, y=temp_dsct, col=temp_liqp.temp_inv))+
  geom_point()+
  geom_smooth(method="lm")+
  coord_fixed(xlim=c(-1.5,1.75),ylim=c(-1.5,1.75))

vif(regr_model)

regsubsets.out <-
  regsubsets(temp_dsct ~ temp_sg + temp_te + temp_indx + temp_snry + temp_liqp + temp_allo + temp_inv + temp_pmv + temp_sig + temp_rela + temp_rels + temp_rf + temp_liqp.temp_pmv + temp_inv.temp_pmv + temp_liqp.temp_inv,
             data = ee,
             nbest = 1,       # 1 best model for each number of predictors - variables
             nvmax = NULL,    # NULL for no limit on number of variables - 
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
regsubsets.out

summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
summary.out$adjr2
summary.out$cp

