# Preliminary remarks ####
## Variables for data are 4 digits
## Variables for valuation parameters are 3 digits

# Packages Load ####
library(readxl)
library(dplyr)
library(tidyr)
library(qrmtools)
library(quantmod)
library(scales)
library(ggplot2)

dsct_slve <- function(VALUE){

  
# Calculation 

## Retrieve relevant data####
  
### filter for relevant company  
temp_data <- filter(tmp1_data, VentureID==cy)
  
### Define relevant data
rlvt_data <- c("ID", "FinRoundID", "ShareClassName", "ShareClassIndex", "InvestorID", "DateNotarized", "TotalInvestment", "Shares", "PPS",
          "LPLiqPref", "LPMultiple", "LPRate", "LPRate_Start", "LPRate_End", "LPCap", "LPSeniority", "LPParticipation", "LPCreditable", "LPAllocation")
rlvt_shar <- c("ShareClassIndex", "InvestorID", "Shares")
rlvt_prce <- c("ShareClassIndex", "InvestorID", "PPS")
rlvt_liqp <- c("ShareClassIndex", "InvestorID", "LPLiqPref")
rlvt_mltp <- c("ShareClassIndex", "InvestorID", "LPMultiple")
rlvt_rate <- c("ShareClassIndex", "InvestorID", "DateNotarized", "LPRate", "LPRate_Start", "LPRate_End")
rlvt_lpcp <- c("ShareClassIndex", "InvestorID", "LPCap")
rlvt_snry <- c("ShareClassIndex", "InvestorID", "LPSeniority")
rlvt_part <- c("ShareClassIndex", "InvestorID", "LPParticipation")
rlvt_crdt <- c("ShareClassIndex", "InvestorID", "LPCreditable")
rlvt_allo <- c("ShareClassIndex", "InvestorID", "LPAllocation")

rlvt_parm <- c("ShareClassIndex", "DateNotarized", "Shares", "PPS")

### Select relevant data
data <- temp_data %>%
        select(rlvt_data) %>%
        arrange(ShareClassIndex)
  
## Arrange data for processing ####

### Select shares for shareholdings
data_shar <- data %>%
             select(rlvt_shar) %>%
             group_by(ShareClassIndex, InvestorID) %>%
             summarize(Shares=sum(Shares)) %>%
             spread(InvestorID, Shares)

### Select pricing for shareholdings - Remark: PPS is averaged for the individual investor! 
data_prce <- data %>%
             select(rlvt_prce) %>%
             group_by(ShareClassIndex, InvestorID) %>%
             summarize(PPS=mean(PPS)) %>% #PPS is averaged for the individual investor!
             spread(InvestorID, PPS)
             
### Select indication of Liquidation Preference (herinafter "LP") 
data_liqp <- data %>%
             select(rlvt_liqp) %>%
             mutate(LPLiqPref=ifelse(LPLiqPref==0, NA, LPLiqPref)) %>%
             group_by(ShareClassIndex, InvestorID) %>%
             summarize(LPLiqPref=mean(LPLiqPref)) %>%
             spread(InvestorID, LPLiqPref)

### Select multiple for LP calculation 
data_mltp <- data %>%
             select(rlvt_mltp) %>%
             group_by(ShareClassIndex, InvestorID) %>%
             summarize(LPMultiple=mean(LPMultiple)) %>%
             spread(InvestorID, LPMultiple)

### Select cap for shareholdings (cap refers to LP as well as Participation - please note that "cap < liqpref" must hold) 
data_lpcp <- data %>%
             select(rlvt_lpcp) %>%
             mutate(LPCap=ifelse(LPCap==0, NA, LPCap)) %>%
             group_by(ShareClassIndex, InvestorID) %>%
             summarize(LPCap=mean(LPCap)) %>%
             spread(InvestorID, LPCap)

### Select level of seniority for shareholdings 
data_snry <- data %>%
             select(rlvt_snry) %>%
             group_by(ShareClassIndex, InvestorID) %>%
             summarize(LPSeniority=mean(LPSeniority)) %>%
             spread(InvestorID, LPSeniority)

### Select indication for participation - please note, if not "participating", conversion-feature is assumed 
data_part <- data %>%
             select(rlvt_part) %>%
             group_by(ShareClassIndex, InvestorID) %>%
             summarize(LPParticipation=mean(LPParticipation)) %>%
             spread(InvestorID, LPParticipation)

### Select indication if LP amount is creditable 
data_crdt <- data %>%
             select(rlvt_crdt) %>%
             group_by(ShareClassIndex, InvestorID) %>%
             summarize(LPCreditable=mean(LPCreditable)) %>%
             spread(InvestorID, LPCreditable)

### Select indication about allocation mechanism - please note, there should be only one allocation mechanism 
data_allo <- data %>%
             select(rlvt_allo) %>%
             summarize(LPAllocation_allo=first(LPAllocation), Check=n_distinct(LPAllocation))

### Select valuation parameter data
totl_shar <- data  %>%
  select(rlvt_parm) %>%
  summarize(totl_shar=sum(Shares))

last_valu <- data  %>%
  select(rlvt_parm) %>%
  filter(ShareClassIndex==max(ShareClassIndex)) %>%
  summarize(last_valu=mean(PPS))

date_last_valu <- data  %>%
  select(rlvt_parm) %>%
  filter(ShareClassIndex==max(ShareClassIndex)) %>%
  summarize(last_valu=last(DateNotarized))

pstm_valu <- totl_shar*last_valu

## Valuation Parameter ####
beg <- pull(date_last_valu, last_valu)
end <- as.Date("31-12-2022", format="%d-%m-%Y") # - link to a model!

tte <- te #as.numeric(difftime(end, beg, unit="weeks"))/52.25 # Time to exit, corresponds to the "time to exit" for option value calculation
val <- VALUE #pull(pstm_valu, totl_shar) # Exit value of the company, corresponds to the exit proceeds - equate to the pmv of the latests round
rfr <- rf # Risk free rate
sig <- sg # Volatility of val

### Select rate for LP calculation! 
data_rate <- data %>%
  select(rlvt_rate) %>%
  mutate(LPRate_Start=as.Date(ifelse(LPRate_Start==0, DateNotarized, LPRate_Start))) %>%
  mutate(LPRate_End=as.Date(ifelse(LPRate_End==0, end, LPRate_End))) %>%
  mutate(LPRate_time=as.numeric(difftime(LPRate_End, LPRate_Start, unit="weeks"))/52.25) %>%
  mutate(LPRate_mltp=as.numeric(ifelse(LPRate==0, 0, (1+LPRate)^(LPRate_time)-1))) %>%
  select(-c(3:7)) %>%
  group_by(ShareClassIndex, InvestorID) %>%
  summarize(LPRate_mltp=mean(LPRate_mltp)) %>%
  spread(InvestorID, LPRate_mltp)

## Calculation of interim values ####

### Derive index (ShareClassIndex based) for all subsequent data.frames/tibbles
indx <- data_shar[,1]

### Calculate pro rata percantages based on nominal shares 
prrt_perc <- as_tibble(data_shar[,-1]/sum(data_shar[,-1], na.rm=TRUE))
prrt <- prrt_perc*val

## Calculation of LP ####

### Calculate relevant LP price per shareholding and LP amount
liqp_prce <- as_tibble(data_prce[,-1]*data_liqp[,-1]*(data_rate[,-1]+data_mltp[,-1]))
liqp_amnt <- as_tibble(data_shar[,-1]*liqp_prce)

### LP proceed steps according to seniority
liqp_prst <- rev(c(0:max(data_snry[,-1], na.rm=TRUE)))

### LP proceed steps layers in absolute amounts and percentage terms
liqp_prst_lyer <- list()
liqp_prst_lyer_perc <- list()

for (a in 1:length(liqp_prst)){
  name <- paste("lyer", a, sep="")
  tmp1 <- as_tibble(data_shar[,-1]*0)
  tmp2 <- as_tibble(data_shar[,-1]*0)
  for (i in 1:nrow(tmp1)){
    for (j in 1:ncol(tmp1)){
      tmp1[i,j] <- liqp_amnt[i,j]*(liqp_prst[a]==data_snry[,-1][i,j])
    }
  }
  liqp_prst_lyer[[name]] <- tmp1
  for (i in 1:nrow(tmp1)){
    for (j in 1:ncol(tmp1)){
      tmp2[i,j] <- liqp_amnt[i,j]*(liqp_prst[a]==data_snry[,-1][i,j])/sum(tmp1, na.rm=TRUE)
    }
  }
  liqp_prst_lyer_perc[[name]] <- tmp2
}
rm(tmp1, tmp2)

### Derive caps corrected for LPs - onsidering if creditable or not, if creditable, the cap is not reduced, if non-creditable, the cap is reduced (because shares always participate)
### The variables are not used here, but are later on applied in scba, shba and cpra 
liqp_lpcp <- as_tibble(data_lpcp[,-1]-liqp_prce*(1-data_crdt[,-1]))

### Calculation of breakpoints by proceed step / layer
temp_liqp_prst <- sapply(liqp_prst_lyer, sum, na.rm=TRUE)
liqp_prst <- temp_liqp_prst*0

for (b in 1:length(temp_liqp_prst)){
  liqp_prst[b] <- sum(temp_liqp_prst[1:b])
}

### Calculation of call values
liqp_call <- liqp_prst*0

for (b in 1:length(liqp_call)){
  liqp_call[b] <- Black_Scholes(t=0, S=val, r=rfr, sigma= sig, K=liqp_prst[b], T=tte, type="call")
}

### Calculation of breakpoint value (i.e., value per proceed step / layer)
liqp_valu <- liqp_call*0

for (b in 1:length(liqp_valu)){
  if(b==1){
    liqp_valu[b] <- val - liqp_call[b]
  }  else {
    liqp_valu[b] <- liqp_call[b-1]-liqp_call[b]
  }
}

### Calculation of interface values for participation
liqp_prst_infc <- max(liqp_prst)
liqp_call_infc <- liqp_call[length(liqp_call)]

### Allocation of LP value among layers
liqp_prst_lyer_allo <- list()

for (a in 1:length(liqp_prst)){
  name <- paste("lyer", a, sep="")
  tmp1 <- as_tibble(data_shar[,-1]*0)
  for (i in 1:nrow(tmp1)){
    for (j in 1:ncol(tmp1)){
      tmp1[i,j] <- liqp_prst_lyer_perc[[name]][i,j]*liqp_valu[a]
    }
  }
  liqp_prst_lyer_allo[[name]] <- tmp1
}
rm(tmp1)

### Aggreagtion of LP value across layers
liqp_allo <- as_tibble(liqp_prce*0)

for (i in 1:nrow(liqp_allo)){
  for (j in 1:ncol(liqp_allo)){
    for (a in 1:length(liqp_prst_lyer_allo)){
      liqp_allo[i,j] <- liqp_allo[i,j]+ifelse(is.na(liqp_prst_lyer_allo[[a]][i,j])==TRUE, 0, liqp_prst_lyer_allo[[a]][i,j])
    }
  }
}

# Allocation of remaining exit proceeds (shareclass-based, scba; shareholder-based, shba; convertible-preferred, cpra)

if(data_allo$LPAllocation_allo=="SCB"){

## Shareclass-based allocation ####

### scba proceed steps (price-based), considering unique prices, LP prices and caps
temp_scba_prst_prce <- unique(sort(unlist(bind_cols(data_prce[,-1], liqp_prce, liqp_lpcp))))
scba_prst_prce <- c(temp_scba_prst_prce, max(temp_scba_prst_prce))

### Calculate proceed steps layers (price-based) in absolute shares and in relative terms (in%)
scba_prst_lyer <- list()
scba_prst_lyer_perc <- list()
     
for (a in 1:length(scba_prst_prce)){
  name <- paste("lyer", a, sep="")
  tmp1 <- as_tibble(data_shar[,-1]*0)
  tmp2 <- as_tibble(data_shar[,-1]*0)
  if(a<length(scba_prst_prce)){
    for (i in 1:nrow(tmp1)){
      for (j in 1:ncol(tmp1)){
      tmp1[i,j] <- data_shar[,-1][i,j]*(data_part[,-1][i,j]*(1-data_crdt[,-1][i,j])
                   -ifelse(is.na(data_lpcp[,-1][i,j])==TRUE, 0, data_part[,-1][i,j]*(1-data_crdt[,-1][i,j])*(liqp_lpcp[i,j]<scba_prst_prce[a]))
                   +data_part[,-1][i,j]*data_crdt[,-1][i,j]*(data_prce[,-1][i,j]<scba_prst_prce[a])*(ifelse(is.na(liqp_prce[i,j])==TRUE, 0, liqp_prce[i,j])<scba_prst_prce[a])
                   -ifelse(is.na(data_lpcp[,-1][i,j])==TRUE, 0, data_part[,-1][i,j]*data_crdt[,-1][i,j]*(liqp_lpcp[i,j]<scba_prst_prce[a])))
      }
    }
  }
  if(a==length(scba_prst_prce)){
    for (i in 1:nrow(tmp1)){
      for (j in 1:ncol(tmp1)){
      tmp1[i,j] <- data_shar[,-1][i,j]*(data_part[,-1][i,j]*(1-data_crdt[,-1][i,j])
                   -ifelse(is.na(data_lpcp[,-1][i,j])==TRUE, 0, data_part[,-1][i,j]*(1-data_crdt[,-1][i,j])*(liqp_lpcp[i,j]<=scba_prst_prce[a]))
                   +data_part[,-1][i,j]*data_crdt[,-1][i,j]*(data_prce[,-1][i,j]<=scba_prst_prce[a])*(ifelse(is.na(liqp_prce[i,j])==TRUE, 0, liqp_prce[i,j])<=scba_prst_prce[a])
                   -ifelse(is.na(data_lpcp[,-1][i,j])==TRUE, 0, data_part[,-1][i,j]*data_crdt[,-1][i,j]*(liqp_lpcp[i,j]<=scba_prst_prce[a])))
      }  
    } 
  }
  scba_prst_lyer[[name]] <- tmp1
  for (i in 1:nrow(tmp1)){
    for (j in 1:ncol(tmp1)){
      tmp2[i,j] <- tmp1[i,j]/sum(tmp1, na.rm=TRUE)
    }
  }
  scba_prst_lyer_perc[[name]] <- tmp2
}
rm(tmp1, tmp2)

### Calcuate breakingpoints for proceed steps / layers 
### First, the number of shares per layer is calculated (via apply); second, the number of shares is multiplied by their price (ie the prst as per layer); third, the last brpt/prst is replaced, because it refers to pro-rata, so no additional strike needed; finally, the LP prst/brpt is added
scba_prst_lyer_shar <- sapply(scba_prst_lyer, sum, na.rm=TRUE)
temp_scba_prst <- scba_prst_prce*scba_prst_lyer_shar
temp_scba_prst[length(temp_scba_prst)] <- temp_scba_prst[length(temp_scba_prst)-1]
scba_prst <- temp_scba_prst+liqp_prst_infc

### Calculate call value of proceed steps as per breakpoint
scba_call <- scba_prst*0

for (b in 1:length(scba_call)){
  scba_call[b] <- Black_Scholes(t=0, S=val, r=rfr, sigma= sig, K=scba_prst[b], T=tte, type="call")
}

### Calculation of breakpoint value (i.e., value per proceed step / layer)
scba_valu <- scba_call*0

for (b in 1:length(scba_valu)){
  if(b==1){
    scba_valu[b] <- liqp_call_infc - scba_call[b]
  }else if(b==length(scba_valu)){
    scba_valu[b] <- scba_call[b]
  }else{
    scba_valu[b] <- scba_call[b-1]-scba_call[b]
  }
}

### Allocation of participation value among layers
scba_prst_lyer_allo <- list()

for (a in 1:length(scba_valu)){
  name <- paste("lyer", a, sep="")
  tmp1 <- as_tibble(data_shar[,-1]*0)
  for (i in 1:nrow(tmp1)){
    for (j in 1:ncol(tmp1)){
      tmp1[i,j] <- scba_prst_lyer_perc[[name]][i,j]*scba_valu[a]
    }
  }
  scba_prst_lyer_allo[[name]] <- tmp1
}
rm(tmp1)

### Aggreagtion of participation value across layers
scba_allo <- as_tibble(data_shar[,-1]*0)

for (i in 1:nrow(scba_allo)){
  for (j in 1:ncol(scba_allo)){
    for (a in 1:length(scba_prst_lyer_allo)){
      scba_allo[i,j] <- scba_allo[i,j]+ifelse(is.na(scba_prst_lyer_allo[[a]][i,j])==TRUE, 0, scba_prst_lyer_allo[[a]][i,j])
    }
  }
}

### Aggregation of participation and LP
scba <- as_tibble(data_shar[,-1]*0)

for (i in 1:nrow(scba)){
  for (j in 1:ncol(scba)){
    scba[i,j] <- ifelse(is.na(scba_allo[i,j])==TRUE, 0, scba_allo[i,j])
    scba[i,j] <- scba[i,j] + ifelse(is.na(liqp_allo[i,j])==TRUE, 0, liqp_allo[i,j])
  }
}
scba <- as_tibble(scba+data_shar[,-1]*0)
scba_dsct <- round(as_tibble((scba/data_shar[,-1])/rowMeans(data_prce[,-1], na.rm=TRUE)[nrow(data_shar)]-1), 4)


}else if(data_allo$LPAllocation_allo=="SHB"){


# Shareholder-based allocation ####

### shba proceed steps (price-based), considering unique prices, LP prices and caps

### Primary idea: Derive breakpoints, sort breakpoints, calculate layers, determine call value, allocate call value across layers;

### Shares that are participating
shba_shar <- as_tibble(data_shar[,-1]*data_part[,-1])

### Shares that are capped with regard to allocated proceeds (per share)
shba_shar_prtc <- as_tibble(data_shar[,-1]*data_part[,-1]*(ifelse(is.na(data_lpcp[,-1])==TRUE, 0, 1)))

### Calculation of percentage share of each shareholding
shba_shar_perc <- as_tibble(data_shar[,-1]*data_part[,-1]/(sum(data_shar[,-1]*data_part[,-1], na.rm=TRUE)))

### Calculation of creditable LPs per shareholding
shba_crdt_liqp <- as_tibble(shba_shar*liqp_prce)

### Calculation of critical cap amount per shareholding
shba_prtc <- as_tibble(shba_shar_prtc*liqp_lpcp)

### Calculate percentage share of shareholdings per investor
shba_nvsr_perc <- colSums(shba_shar_perc, na.rm=TRUE)

### Calculate creditable LPs per investor
shba_nvsr_crdt_liqp <- colSums(shba_crdt_liqp, na.rm=TRUE)

### Now, the amounts of creditable LPs and caps per investor are scaled-up according to their shareholding (across all single shareholdings for one investor)
shba_scle_crdt_liqp <- shba_shar
shba_scle_prtc <- shba_shar
for (i in 1:nrow(shba_shar)){
  for (j in 1:ncol(shba_shar)){
    if(!is.na(shba_shar[i,j])){
      shba_scle_crdt_liqp[i,j] <- shba_nvsr_crdt_liqp[j]/shba_nvsr_perc[j]
      shba_scle_prtc[i,j] <- shba_prtc[i,j]/shba_shar_perc[i,j]
    }else{
      shba_scle_crdt_liqp[i,j] <- NA
      shba_scle_prtc[i,j] <- NA
    }
  }
}

### Derive and sort shba breakpoints/proceed steps, considering scaled creditable LPs and caps
temp_shba_prst_prce <- unique(sort(unlist(bind_cols(shba_scle_crdt_liqp, shba_scle_prtc))))
shba_prst_prce <- c(temp_shba_prst_prce, max(temp_shba_prst_prce))

### Calcuate layers for the shareholdings according to the breakpoints/proceed steps dervide beforehand
shba_prst_lyer <- list() # this will be used to catch the share of shares as per each layer
shba_prst_lyer_perc <- list() # this will be used to catch the percentage share of shares as per each layer

shba_prst_lyer_lpcp_shar <- list() # this will be used to catch the total shares participating (i.e. although calculated per each layer, the total number of all participating shares is considered) EXCEPT for the consideration of the cap (i.e. capped shares will be "dismissed" as per corresponding layer)

for (a in 1:length(shba_prst_prce)){
  name <- paste("lyer", a, sep="")
  tmp1 <- as_tibble(data_shar[,-1]*0)
  tmp2 <- as_tibble(data_shar[,-1]*0)
  tmp3 <- as_tibble(data_shar[,-1]*0)
  if(a<length(shba_prst_prce)){
    for (i in 1:nrow(shba_shar)){
      for (j in 1:ncol(shba_shar)){
        tmp1[i,j] <- (shba_shar[i,j]*(shba_prst_prce[a]>shba_scle_crdt_liqp[i,j])
                     -shba_shar[i,j]*(is.na(shba_scle_prtc[i,j])==FALSE)*(shba_prst_prce[a]>ifelse(is.na(shba_scle_prtc[i,j])==TRUE, 0, shba_scle_prtc[i,j])))
        tmp3[i,j] <- (shba_shar[i,j]
                     -shba_shar[i,j]*(is.na(shba_scle_prtc[i,j])==FALSE)*(shba_prst_prce[a]>ifelse(is.na(shba_scle_prtc[i,j])==TRUE, 0, shba_scle_prtc[i,j])))
      }
    }
  }
  if(a==length(shba_prst_prce)){
    for (i in 1:nrow(shba_shar)){
      for (j in 1:ncol(shba_shar)){
        tmp1[i,j] <- (shba_shar[i,j]*(shba_prst_prce[a]>=shba_scle_crdt_liqp[i,j])
                     -shba_shar[i,j]*(is.na(shba_scle_prtc[i,j])==FALSE)*(shba_prst_prce[a]>=ifelse(is.na(shba_scle_prtc[i,j])==TRUE, 0, shba_scle_prtc[i,j])))
        tmp3[i,j] <- (shba_shar[i,j]
                     -shba_shar[i,j]*(is.na(shba_scle_prtc[i,j])==FALSE)*(shba_prst_prce[a]>=ifelse(is.na(shba_scle_prtc[i,j])==TRUE, 0, shba_scle_prtc[i,j])))
      }  
    } 
  }
  shba_prst_lyer[[name]] <- tmp1
  shba_prst_lyer_lpcp_shar[[name]] <- tmp3
  for (i in 1:nrow(shba_shar)){
    for (j in 1:ncol(shba_shar)){
      tmp2[i,j] <- tmp1[i,j]/sum(tmp1, na.rm=TRUE)
    }
  }
  shba_prst_lyer_perc[[name]] <- tmp2
}
rm(tmp1, tmp2, tmp3)

### Next, the proceed steps/breakpoints are weighted by the share of actually participating for each layer
### Calculate shares participating in each layer (i.e. per proceed step/breakpoint)
shba_prst_shar <- sapply(shba_prst_lyer, sum, na.rm=TRUE)

### Calculate shares participating in total considering capped shares)
shba_prst_lpcp_shar <- sapply(shba_prst_lyer_lpcp_shar, sum, na.rm=TRUE)

### Calculate weighted proceed steps/breakpoints - please note, that capped shares are also considered, adjusting to 100%, whenever capped shares are dismissed
shba_prst <- (shba_prst_prce*(shba_prst_shar/shba_prst_lpcp_shar))+liqp_prst_infc

### Calculate call value of proceed steps as per breakpoint
shba_call <- shba_prst*0

for (b in 1:length(shba_call)) {
  shba_call[b] <- Black_Scholes(t=0, S=val, r=rfr, sigma= sig, K=shba_prst[b], T=tte, type="call")
}

### Calculate breakpoint value (i.e., value per proceed step / layer)
shba_valu <- shba_call*0

for (b in 1:length(shba_valu)){
  if(b==1){
    shba_valu[b] <- liqp_call_infc - shba_call[b]
  }else if(b==length(shba_valu)){
    shba_valu[b] <- shba_call[b]
  }else{
    shba_valu[b] <- shba_call[b-1]-shba_call[b]
  }
}

### Allocation of participation value among layers
shba_prst_lyer_allo <- list()

for (a in 1:length(shba_valu)){
  name <- paste("lyer", a, sep="")
  tmp1 <- as_tibble(data_shar[,-1]*0)
  for (i in 1:nrow(tmp1)){
    for (j in 1:ncol(tmp1)){
      tmp1[i,j] <- shba_prst_lyer_perc[[name]][i,j]*shba_valu[a]
    }
  }
  shba_prst_lyer_allo[[name]] <- tmp1
}
rm(tmp1)

### Aggreagtion of participation value across layers
shba_allo <- as_tibble(data_shar[,-1]*0)

for (i in 1:nrow(shba_allo)){
  for (j in 1:ncol(shba_allo)){
    for (a in 1:length(shba_prst_lyer_allo)){
      shba_allo[i,j] <- shba_allo[i,j]+ifelse(is.na(shba_prst_lyer_allo[[a]][i,j])==TRUE, 0, shba_prst_lyer_allo[[a]][i,j])
    }
  }
}

### Aggregation of participation and LP
shba <- as_tibble(data_shar[,-1]*0)

for (i in 1:nrow(shba)){
  for (j in 1:ncol(shba)){
    shba[i,j] <- ifelse(is.na(shba_allo[i,j])==TRUE, 0, shba_allo[i,j])
    shba[i,j] <- shba[i,j] + ifelse(is.na(liqp_allo[i,j])==TRUE, 0, liqp_allo[i,j])
  }
}
shba <- as_tibble(shba+data_shar[,-1]*0)
shba_dsct <- round(as_tibble((shba/data_shar[,-1])/rowMeans(data_prce[,-1], na.rm=TRUE)[nrow(data_shar)]-1), 4)


}else{


# Preferred convertible allocation ####

### Shares that are convertible
cpra_shar <- as_tibble(data_part[,-1]*data_shar[,-1]) 

### Calculate proceed steps / breakpoints; first, the breakpoints are determined, second, layers are formed (absolut shares and percentage)
cpra_prst_prce <- rev(c(unique(sort(unlist(liqp_prce), decreasing=TRUE)),0))

cpra_prst_lyer <- list()
cpra_prst_lyer_perc <- list()

for (a in 1:length(cpra_prst_prce)){
  name <- paste("lyer", a, sep="")
  tmp1 <- as_tibble(data_shar[,-1]*0)
  for (i in 1:nrow(tmp1)){
    for (j in 1:ncol(tmp1)){
      if(is.na(liqp_prce[i,j] <= cpra_prst_prce[a])){
        tmp1[i,j] <- cpra_shar[i,j]
      }else if(liqp_prce[i,j] <= cpra_prst_prce[a]){
        tmp1[i,j] <- cpra_shar[i,j]
      }else{
        tmp1[i,j] <- NA
      }
    }
  }
  cpra_prst_lyer[[name]] <- tmp1
  cpra_prst_lyer_perc[[name]] <- as_tibble(tmp1/(sum(tmp1, na.rm=TRUE)))
}
rm(tmp1)

### Derive range from prices of breakpoints/proceed steps
cpra_prst_prce_rnge <- cpra_prst_prce*0

for (b in 1:length(cpra_prst_prce_rnge)){
  if(b==1){
    cpra_prst_prce_rnge[b] <- cpra_prst_prce[b+1]
  }else if(b==length(cpra_prst_prce_rnge)){
    cpra_prst_prce_rnge[b] <- 0
  }else{
    cpra_prst_prce_rnge[b] <- cpra_prst_prce[b+1]-cpra_prst_prce[b]
  }
}

### Derive proceed steps by multiplying shares times price ranges from proceed steps/breakpoints
cpra_prst_temp <- cpra_prst_prce_rnge*sapply(cpra_prst_lyer, sum, na.rm=TRUE)
cpra_prst <- cpra_prst_temp*0

for (b in 1:length(cpra_prst)){
  if(b==1){
    cpra_prst[b] <- cpra_prst_temp[b] + liqp_prst_infc
  }else{
    cpra_prst[b] <- cpra_prst[b-1]+cpra_prst_temp[b]
  }
}

### Determine call value of proceed steps
cpra_call <- rep(0,length(cpra_prst))

for (b in 1:length(cpra_prst)){
  cpra_call[b] <- Black_Scholes(t=0, S=val, r=rfr, sigma= sig, K=cpra_prst[b], T=tte, type="call")
}

### Allocate call value of the respective proceed steps/breakpoints
cpra_valu <- cpra_call*0

for (b in 1:length(cpra_valu)){
  if (b==1){
    cpra_valu[b] <- liqp_call_infc - cpra_call[b]
  }else if(b==length(cpra_valu)){
    cpra_valu[b] <- cpra_call[length(cpra_call)]
  }else{
    cpra_valu[b] <- cpra_call[b-1]-cpra_call[b]
  }
}

### Allocation of participation value among layers
cpra_prst_lyer_allo <- list()

for (a in 1:length(cpra_valu)){
  name <- paste("lyer", a, sep="")
  tmp1 <- as_tibble(data_shar[,-1]*0)
  for (i in 1:nrow(tmp1)){
    for (j in 1:ncol(tmp1)){
      tmp1[i,j] <- cpra_prst_lyer_perc[[name]][i,j]*cpra_valu[a]
    }
  }
  cpra_prst_lyer_allo[[name]] <- tmp1
}
rm(tmp1)

### Aggreagtion of participation value across layers
cpra_allo <- as_tibble(data_shar[,-1]*0)

for (i in 1:nrow(cpra_allo)){
  for (j in 1:ncol(cpra_allo)){
    for (a in 1:length(cpra_prst_lyer_allo)){
      cpra_allo[i,j] <- cpra_allo[i,j]+ifelse(is.na(cpra_prst_lyer_allo[[a]][i,j])==TRUE, 0, cpra_prst_lyer_allo[[a]][i,j])
    }
  }
}

### Aggregation of participation and LP
cpra <- as_tibble(data_shar[,-1]*0)

for (i in 1:nrow(cpra)){
  for (j in 1:ncol(cpra)){
    cpra[i,j] <- ifelse(is.na(cpra_allo[i,j])==TRUE, 0, cpra_allo[i,j])
    cpra[i,j] <- cpra[i,j] + ifelse(is.na(liqp_allo[i,j])==TRUE, 0, liqp_allo[i,j])
  }
}
cpra <- as_tibble(cpra+data_shar[,-1]*0)
cpra_dsct <- round(as_tibble((cpra/data_shar[,-1])/rowMeans(data_prce[,-1], na.rm=TRUE)[nrow(data_shar)]-1), 4)

}
### Checks ####
# val==round(sum(shba, na.rm=TRUE),0)
# val==round(sum(scba, na.rm=TRUE),0)
# 
# check_part <- function(x,y,z){
#   if(x==round(sum(y, na.rm=TRUE),0)){
#     message(paste0("Val corresponds to scba"))
#     if(x==round(sum(z, na.rm=TRUE),0)){
#       message(paste0("Val corresponds to shba"))
#       message("All checks ok")
#     }else{
#       message(paste0("Please check calculation of shba"))
#     }
#   }else{
#     if(x==round(sum(z, na.rm=TRUE),0)){
#       message(paste0("Val corresponds to shba"))
#       message(paste0("Please check calculation of scba"))
#     }
#   }
# }
# 
# sum(scba, na.rm=TRUE)
# check_part(val, scba, shba)

# End of the function ###

print(
  if(data_allo$LPAllocation_allo=="SHB"){
    trgt_shba <- rowMeans(shba/data_shar[,-1], na.rm=TRUE)[nrow(data_shar)]-rowMeans(data_prce[,-1], na.rm=TRUE)[nrow(data_shar)]
  }else if(data_allo$LPAllocation_allo=="SCB"){
    trgt_scba <- rowMeans(scba/data_shar[,-1], na.rm=TRUE)[nrow(data_shar)]-rowMeans(data_prce[,-1], na.rm=TRUE)[nrow(data_shar)]
  }else{
    trgt_cpra <- rowMeans(cpra/data_shar[,-1], na.rm=TRUE)[nrow(data_shar)]-rowMeans(data_prce[,-1], na.rm=TRUE)[nrow(data_shar)]
  }
)
}



