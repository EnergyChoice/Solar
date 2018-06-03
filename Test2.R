library(shiny)
library(readr)
library(shinydashboard)
library(reshape2)
library(ggplot2)
library(htmltools)
library(bsplus)
pmt <- function(rate, nper, pv, fv=0, type=0) {
  rr <- 1/(1+rate)^nper
  res <- (-pv-fv*rr)*rate/(1-rr)
  return(res/(1+rate*type))} #pmt function



agency <- "MCE"
ProgramCost <- 200000
Budget <- 20000000
rev_perc <- 10
Escal <- 2
Elec1 <- 0.238
Elec1_perc <- 68
Elec2 <- 0.140
Elec2_perc <- 28
Elec3 <- 0.199
Elec3_perc <- 4
Elec4 <- 0.010
Elec4_perc <- 1
Avg_non <- 574
Avg_CARE_non_green <- 339
Avg_non_CARE_green <- 387
Avg_CARE_green <- 294

mix1 <- 0
mix2 <- 12
mix3 <- 0
mix4 <- 0 
mix5<- 13
mix6 <- 5
mix7 <- 0
mix8 <- 50
mix9 <- 0 
mix10 <- 20

Impact <- "Mid"
Rebound <- 3
Trans <- 5

CDiscount <- 6
ADiscount <- 2.5
SDiscount <- 5
carbon_p <- 13


SizeOwn <- 5.7
CapFactrOwn <-20   
FixedOwn <- 146.32
VarOwn <- 0
NonCCEOwn <- 7.9
CCEOwn <- 8
PayPeriodOwn <- 20
RebateOwn <- 0
PercRebateOwn <- 0
BonusOwn <- 0.01
PercGenOwn <- 99.4
Marketing <- 10
DefaultOwn <- 5


Size3rd <- 5.7
CapFactr3rd <-20   
Fixed3rd <- 146.32
Var3rd <- 0
NonCCE3rd <- 9
PayPeriod3rd <- 20
Rebate3rd <- 0
PercRebate3rd <- 0 
Bonus3rd <- 0.01
PercGen3rd <- 0

CCEOwn_SD <- 0.2
NonCCE3rd_SD <- 0.2
PerceivedOwn <- 100
Perceived3rd <- 92
Lifetime <- 25


Flat_OAS <- "Flat rate"
Flat_rate <- 0.06
BonusOwn <- 0.01
Price_P <- "Yes"

Av_elec <- read_csv("Average_elec.csv")
PVcost <- read_csv("PV_cost.csv")
House <- read_csv("Suitable_House.csv") 
Emis <- read_csv("Emission.csv", col_types = cols(CO = col_number(), CO2e = col_number(),  Lead = col_number(), Nox = col_number(), PM2.5 = col_number(),PM10 = col_number(), Sox = col_number(), VOC = col_number()))
Health <- read_csv("Health_factor.csv")


Emission <- Emis[,2:9]
rownames(Emission) <- Emis$Type
N = 100000 # Number of simulation

##########################################################################
# MODEL(BASE)


SD_elec <- 0.4 # Standard deviation of Ln(electricity)
ln_Mean_elec1<- log(Avg_non) 
ln_Mean_elec2<- log(Avg_CARE_non_green) 
ln_Mean_elec3<- log(Avg_non_CARE_green) 
ln_Mean_elec4<- log(Avg_CARE_green) 

CARE_dis <- as.numeric(sample(c(1,0),N,prob=c(Elec2_perc/100,1-Elec2_perc/100),replace=TRUE))
Green_dis <- as.numeric(sample(c(1,0),N,prob=c(Elec4_perc/100,1-Elec4_perc/100),replace=TRUE))

Elec <- matrix(rep(0,N),nrow=N)
for(i in 1:N){
  Elec[i]<- ifelse(CARE_dis[i]==0,ifelse(Green_dis[i]==0,exp(rnorm(n=1,mean=ln_Mean_elec1, sd=SD_elec)),exp(rnorm(n=1,mean=ln_Mean_elec3, sd=SD_elec))),ifelse(Green_dis[i]==0,exp(rnorm(n=1,mean=ln_Mean_elec2, sd=SD_elec)),exp(rnorm(n=1,mean=ln_Mean_elec4, sd=SD_elec))))
  
}
# these are the values we use and subject to change
Data_matrix <- matrix(rep(NA,N*27),nrow=N, ncol=27)
colnames(Data_matrix)<- c("1.Electricity Usage(Kwh/month)","2.PV Generation(kWh/month)","3.System Size(kW)", "4.Rounded Size (kW)", "5.PV cost per kW","6.PV unit cost with noise","7.Old interest","8.3rd interest", "9.Energy Cost(CCA)","10.Energy Cost(Old)","11.Energy Cost(3rd)","12.Annual O&M cost(Own)","13.Annual O&M cost(3rd)","14.NEM(Own & 3rd)", "15.None", "16.Perceived Cost(Own)","17.Perceived Cost(3rd)","18.CCE interest", "19.Final CCA interest", "20.Capital Cost","21.Energy Cost(Own)","22.Base-total cost(CCA)", "23.Base-total cost(Own)", "24.Base-total cost(3rd)", "25.Base choices", "26.CCE-total cost","27.CCE choices")
Elec[Elec>3000]=3000
Data_matrix[,1] <- Elec # add the electricity consumption distribution in the first column
PV_Gen_ln_mean <- log(PercGenOwn/100)
PV_Gen_distribution <- (-(exp(rnorm(n=N,mean=PV_Gen_ln_mean, sd=0.1))-1)+1)
Data_matrix[,2] <- Data_matrix[,1]*PV_Gen_distribution 
# add the PV generation in the second column
Data_matrix[,3] <- Data_matrix[,2]*12/(8760*CapFactrOwn/100) # add the PV size in 3rd col
Data_matrix[,4] <- round(Data_matrix[,3], digits=0) # round PV size
PV_table <- subset(PVcost, select=agency)
for (i in 1:N) {
  Data_matrix[i,5] = ifelse(Data_matrix[i,4]>20, 3000, ifelse(Data_matrix[i,4]<1, 6000,as.numeric(PV_table[match(Data_matrix[i,4],PVcost$Size),1])))} # Pick the PV cost based on agency and PV size, and add in 5th col. 
Rebate_dis <- as.numeric(sample(c(1,0),N,prob=c(PercRebateOwn/100,1-PercRebateOwn/100),replace=TRUE))*-1000*RebateOwn

Data_matrix[,6] <- Data_matrix[,5]*rnorm(n=N,mean=1,sd=CCEOwn_SD)+Rebate_dis # add noisy because the PV cost per kw can be different across PV companies, and add rebate distribution In the excel, the column 5 and 6 are combined

Data_matrix[,7] <- rnorm(n=N, mean=NonCCEOwn, sd=NonCCEOwn*CCEOwn_SD)/100 # old interest rate. 
Data_matrix[,8] <- Data_matrix[,7]+(NonCCE3rd-NonCCEOwn)/100 # 3rd party own interest rate

Common_perc <- Elec1_perc/(Elec1_perc+Elec3_perc)
Common_rate <- as.numeric(sample(c(Elec1,Elec3),N, prob=c(Common_perc,1-Common_perc),replace=TRUE)) 
# sample CCA rate based on the probability
elec_rate <-matrix(rep(0,N),nrow=N)
for(i in 1:N){elec_rate[i]<-ifelse(CARE_dis[i]==1,Elec2,Common_rate[i])+Green_dis[i]*Elec4}

Data_matrix[,9] <- Data_matrix[,1]*elec_rate # add monthly energy cost when getting electricity from CCA
Data_matrix[,20]<- Data_matrix[,3]*Data_matrix[,6]# solar PV capital cost
Data_matrix[,10]<-(-pmt(Data_matrix[,7]/12, PayPeriodOwn*12,Data_matrix[,20])) # add monthly energy cost when owning PV (Own)
Data_matrix[,11]<- (-pmt(Data_matrix[,8]/12, PayPeriod3rd*12, Data_matrix[,20])) # add monthly energy cost when owning PV (3rd)
Data_matrix[,12]<-(FixedOwn)*Data_matrix[,3] #add Annual O&M cost for PVOwn and 3rd party own



for(i in 1:N){
  Data_matrix[i,14] = ifelse((Data_matrix[i,2]-Data_matrix[i,1])<0,(Data_matrix[i,2]-Data_matrix[i,1])*elec_rate[i],ifelse(Flat_OAS=="Flat rate",(Data_matrix[i,2]-Data_matrix[i,1])*Flat_rate,(Data_matrix[i,2]-Data_matrix[i,1])*(elec_rate[i]+BonusOwn-ifelse(Price_P=="Yes",0,Green_dis[i]*Elec4))))} 

 # add NEM credits and (No Data_matrix[,15] anymore)
Data_matrix[,16] <- rnorm(n=N, mean=PerceivedOwn, sd=PerceivedOwn*CCEOwn_SD)
Data_matrix[,17] <- rnorm(n=N, mean=Perceived3rd, sd=Perceived3rd*NonCCE3rd_SD) # add perceived cost for both PVOwn and 3rdOwn
CCA_discount <- (1+CDiscount/100)/(1+Escal/100)-1 # calculate the escalator and discount combined rate. 

Data_matrix[,22] <- (Data_matrix[,9]*12)/CCA_discount*(1-1/((1+CCA_discount)^Lifetime))
Connection_fee <- ifelse((agency=="Lancaster")|(agency=="Apple Valley"),75,145)
Data_matrix[,23] <- ((Data_matrix[,10]-Data_matrix[,14]+Data_matrix[,16])*12+Data_matrix[,12])/(CDiscount/100)*(1-1/((1+CDiscount/100)^Lifetime))+Connection_fee
Data_matrix[,24] <- ((Data_matrix[,11]-Data_matrix[,14]+Data_matrix[,17])*12+Data_matrix[,12])/(CDiscount/100)*(1-1/((1+CDiscount/100)^Lifetime))+Connection_fee

for (i in 1:N) {
  Data_matrix[i,25] = which.min(Data_matrix[i,22:24])} # choose the lowest cost. 
Base <- length(which(Data_matrix[,25]==2)) #count the PVOwn


















