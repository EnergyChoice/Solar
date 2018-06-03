library(shiny)
library(readr)
library(shinydashboard)
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
Elec4_perc <- 0.01

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
Discount <- 5
carbon_p <- 13


SizeOwn <- 5.7
CapFactrOwn <-20   
FixedOwn <- 146.32
VarOwn <- 0
NonCCEOwn <- 7.9
CCEOwn <- 6
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

Mean_elec <- as.numeric(Av_elec[match(agency,Av_elec$Agency),2]) # Mean value of electricity consumption by agency
ln_Mean_elec <- log(Mean_elec)
SD_elec <- 0.4543209 # Standard deviation of Ln(electricity)
Elec <- exp(rnorm(n=N,mean=ln_Mean_elec, sd=SD_elec)) # Calulate electricity distribution from normally distributed ln(elec consumption)
Elec[Elec>3000]=3000 # Electricity consumption that is larger than 3,000 is 3,000
# these are the values we use and subject to change
Data_matrix <- matrix(rep(NA,N*27),nrow=N, ncol=27)
colnames(Data_matrix)<- c("1.Electricity Usage(Kwh/month)","2.PV Generation(kWh/month)","3.System Size(kW)", "4.Rounded Size (kW)", "5.PV cost per kW","6.PV unit cost with noise","7.Old interest","8.3rd interest", "9.Energy Cost(CCA)","10.Energy Cost(Old)","11.Energy Cost(3rd)","12.Annual O&M cost(Own)","13.Annual O&M cost(3rd)","14.NEM(Own & 3rd)", "15.None", "16.Perceived Cost(Own)","17.Perceived Cost(3rd)","18.CCE interest", "19.Final CCA interest", "20.Capital Cost","21.Energy Cost(Own)","22.Base-total cost(CCA)", "23.Base-total cost(Own)", "24.Base-total cost(3rd)", "25.Base choices", "26.CCE-total cost","27.CCE choices")

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
Data_matrix[,6] <- Data_matrix[,5]*rnorm(n=N,mean=1,sd=CCEOwn_SD) # add noisy because the PV cost per kw can be different across PV companies. In the excel, the column 5 and 6 are combined. 
Data_matrix[,7] <- rnorm(n=N, mean=NonCCEOwn, sd=NonCCEOwn*CCEOwn_SD)/100 # old interest rate. 
Data_matrix[,8] <- Data_matrix[,7]+(NonCCE3rd-NonCCEOwn)/100 # 3rd party own interest rate
Rate_table <- matrix(c(Elec1,Elec2,Elec3, Elec1_perc/100, Elec2_perc/100, Elec3_perc/100), nrow=3, ncol=2) # make electricity rate table based on the inputs
elec_rate <- as.numeric(sample(as.character(unlist(Rate_table[,1])),N, prob=as.character(unlist(Rate_table[,2])),replace=TRUE)) 
# sample CCA rate based on the probability
Data_matrix[,9] <- Data_matrix[,1]*elec_rate # add monthly energy cost when getting electricity from CCA
Data_matrix[,20]<- Data_matrix[,3]*Data_matrix[,6]# solar PV capital cost
Data_matrix[,10]<-(-pmt(Data_matrix[,7]/12, PayPeriodOwn*12,Data_matrix[,20])) # add monthly energy cost when owning PV (Own)
Data_matrix[,11]<- (-pmt(Data_matrix[,8]/12, PayPeriod3rd*12, Data_matrix[,20])) # add monthly energy cost when owning PV (3rd)
Data_matrix[,12]<-(FixedOwn+VarOwn)*Data_matrix[,3]/SizeOwn #add Annual O&M cost for PVOwn
Data_matrix[,13]<-(Fixed3rd+Var3rd)*Data_matrix[,3]/SizeOwn #add Annual O&M cost for 3rd party own

#old#Data_matrix[,12] <- rnorm(n=N, mean=(FixedOwn+VarOwn), sd=(FixedOwn+VarOwn)*CCEOwn_SD)*Data_matrix[,3]/SizeOwn #add Annual O&M cost for PVOwn
#old#Data_matrix[,13] <- rnorm(n=N, mean=(Fixed3rd+Var3rd), sd=(FixedOwn+VarOwn)*NonCCE3rd_SD)*Data_matrix[,3]/SizeOwn #add Annual O&M cost for 3rd party own

for(i in 1:N){
Data_matrix[i,14] = ifelse((Data_matrix[i,2]-Data_matrix[i,1])>0,(Data_matrix[i,2]-Data_matrix[i,1])*(elec_rate[i]+BonusOwn),(Data_matrix[i,2]-Data_matrix[i,1])*elec_rate[i]) 
} # add NEM credits and (No Data_matrix[,15] anymore)
Data_matrix[,16] <- rnorm(n=N, mean=PerceivedOwn, sd=PerceivedOwn*CCEOwn_SD)
Data_matrix[,17] <- rnorm(n=N, mean=Perceived3rd, sd=Perceived3rd*NonCCE3rd_SD) # add perceived cost for both PVOwn and 3rdOwn
CCA_discount <- (1+Discount/100)/(1+Escal/100)-1 # calculate the escalator and discount combined rate. 

Data_matrix[,22] <- (Data_matrix[,9]*12)/CCA_discount*(1-1/((1+CCA_discount)^Lifetime))
Connection_fee <- ifelse((agency=="Lancaster")|(agency=="Apple Valley"),75,145)
Data_matrix[,23] <- ((Data_matrix[,10]-Data_matrix[,14]+Data_matrix[,16])*12+Data_matrix[,12])/(Discount/100)*(1-1/((1+Discount/100)^Lifetime))+Connection_fee
Data_matrix[,24] <- ((Data_matrix[,11]-Data_matrix[,14]+Data_matrix[,17])*12+Data_matrix[,13])/(Discount/100)*(1-1/((1+Discount/100)^Lifetime))+Connection_fee

for (i in 1:N) {
    Data_matrix[i,25] = which.min(Data_matrix[i,22:24])} # choose the lowest cost. 
Base <- length(which(Data_matrix[,25]==2)) #count the PVOwn

##########################################################################
# MODEL(CCE)

Data_matrix[,18]<- Data_matrix[,7]-(NonCCEOwn-CCEOwn)/100
Data_matrix[,19]<- ifelse(Data_matrix[,7]<CCEOwn/100, Data_matrix[,7], ifelse(Data_matrix[,7]<NonCCEOwn/100, CCEOwn/100, Data_matrix[,18])) #Final CCE interest rate
Data_matrix[,21]<- -pmt(Data_matrix[,19]/12, PayPeriodOwn*12, Data_matrix[,20]) # monthly energy cost (CCE)

Data_matrix[,26] <- ((Data_matrix[,21]-Data_matrix[,14]+Data_matrix[,16])*12+Data_matrix[,12])/(Discount/100)*(1-1/(1+Discount/100)^Lifetime)
for (i in 1:N) {
 Data_matrix[i,27] = which.min(Data_matrix[i,c(22,24,26)])}
CCE <- length(which(Data_matrix[,27]==3)) #count the CCEOwn

##########################################################################
# Emission

Suitable_H <- as.numeric(House[match(agency,House$Agency),2])
Average_loan <- mean(Data_matrix[Data_matrix[,27]==3,20])
Max_house <- Budget/Average_loan # max number of houses based on the budget
Total_uptakes <- Suitable_H*CCE/N*Marketing/100 # predicted uptakes based on the model
Final_uptakes <- ifelse(Max_house>Total_uptakes,Total_uptakes, Max_house) # Final solar pv uptakes. 
Uptakes_caused <- Suitable_H*(CCE-Base)/N*Marketing/100 # the pv uptakes when not considering budget constraint
Final_uptakes_caused <- ifelse(Max_house<Total_uptakes,Max_house*(CCE-Base)/Base, Uptakes_caused) # the final pv uptakes caused by program.
Average_Gen <- mean(Data_matrix[Data_matrix[,27]==3,2])*12

Energymix <- matrix(c(mix1,mix2,mix3,mix4,mix5,mix6,mix7,mix8,mix9,mix10)/100, ncol=10)
Emission <- as.matrix(Emission)
Emission_Factor <- Energymix %*% Emission
Degrade_matrix <- matrix(rep(NA,Lifetime*2),ncol=2)
Degrade_matrix[1,1] <- 1
Degrade_matrix[2,1] <- 1-0.02
Degrade_matrix[1,2] <- 1/(1+Discount/100)
Degrade_matrix[2,2] <- Degrade_matrix[2,1]/(1+Discount/100)^2

for (i in 3:Lifetime){
  Degrade_matrix[i,1] <- Degrade_matrix[i-1,1]-0.008
  Degrade_matrix[i,2] <- Degrade_matrix[i,1]/(1+Discount/100)^i
}
colnames(Degrade_matrix) <- c("degradation","discounted")
Aggreated_degradation <- sum(Degrade_matrix[,1])

Lifetime_Emission_Reduction <- as.matrix(Emission_Factor[1,]*Aggreated_degradation*Final_uptakes_caused*Average_Gen/1000*(1+Trans/100)/(1+Rebound/100))
Health_factor <- subset(Health,Health$Level==Impact)
E_Reduced <- Lifetime_Emission_Reduction[2:5,]
Health_Benefit <- matrix(rep(NA,4),ncol=4)
for (i in 1:4){
Health_Benefit[1,i] <-as.numeric(Health_factor[1,1+i])*as.numeric(E_Reduced[i])
}
Total_health_benefit <-sum(Health_Benefit)
##########################################################################
# Revenue
Number_month <- PayPeriodOwn*12
Monthly_payment <- -pmt(CCEOwn/100/12, Number_month, Average_loan)
Collected_revenue <- (1-DefaultOwn/100)*(Monthly_payment)/(Discount/100/12)*(1-1/(Discount/100/12+1)^(Number_month))*Uptakes_caused
Loan_provided <- Average_loan*Uptakes_caused

Avg_NEM_bonus<- sum(Data_matrix[(Data_matrix[,14]>0) & (Data_matrix[,27]==3),14])/CCE*12*(BonusOwn)/(BonusOwn+Elec1)
Revenue_lost <- sum(Degrade_matrix[,2])*Final_uptakes_caused*(Average_Gen*(Elec1*Elec1_perc/100+Elec2*Elec2_perc/100+Elec3*Elec3_perc/100)*rev_perc/100+Avg_NEM_bonus)
Revenue_change <- Collected_revenue-Loan_provided-ProgramCost-Revenue_lost
##########################################################################
# Outputs

FinalTable <- matrix(c(Final_uptakes_caused,(Final_uptakes-Final_uptakes_caused),Lifetime_Emission_Reduction[1,1],Total_health_benefit, Revenue_lost, Revenue_change), nrow=6, ncol=1)
rownames(FinalTable)<-c("Solar Uptakes Caused by Pgoram", "Total Solar Uptakes", "GHG Emission Reduction(ton)","Health Improvement(dollar)","Revenue Lost due to PV Uptakes(dollar)","Total Revenue Change(dollar)")
colnames(FinalTable)<-c("Estimated Results")

TCM <-FinalTable
Finalsale <- as.data.frame(t(TCM[1:2,1]))
Finalsale[,3] <- Finalsale[,2]-Finalsale[,1]
Finalsale[,4] <- Finalsale[,1]
Final <- as.data.frame(Finalsale[1,3:4])
colnames(Final) <-c("Total Uptakes","Caused by Program")
DF <- data.frame(Final)
DF$Type <- "Solar Uptakes"
DF1 <- melt(DF, id.var="Type")
library(ggplot2)
ggplot(DF1, aes(x = Type, y = value, fill = variable)) + 
  geom_bar(stat = "identity",width = 0.5)+
  ggtitle("The Number of Solar Uptakes through Solor Financing Program")+
  ylab("Solar PV Installation")+
  xlab("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), legend.title=element_blank())


agency="Sonoma"
TCMfunction <- function(var.vec){
  PerceivedOwn <-var.vec[1]
  Perceived3rd <-var.vec[2]
  N = 2000 # Number of simulation
  
  Mean_elec <- as.numeric(Av_elec[match(agency,Av_elec$Agency),2]) # Mean value of electricity consumption by agency
  ln_Mean_elec <- log(Mean_elec)
  SD_elec <- 0.4543209 # Standard deviation of Ln(electricity)
  Elec <- exp(rnorm(n=N,mean=ln_Mean_elec, sd=SD_elec)) # Calulate electricity distribution from normally distributed ln(elec consumption)
  Elec[Elec>3000]=3000 # Electricity consumption that is larger than 3,000 is 3,000
  # these are the values we use and subject to change
  Data_matrix <- matrix(rep(NA,N*27),nrow=N, ncol=27)
  colnames(Data_matrix)<- c("1.Electricity Usage(Kwh/month)","2.PV Generation(kWh/month)","3.System Size(kW)", "4.Rounded Size (kW)", "5.PV cost per kW","6.PV unit cost with noise","7.Old interest","8.3rd interest", "9.Energy Cost(CCA)","10.Energy Cost(Old)","11.Energy Cost(3rd)","12.Annual O&M cost(Own)","13.Annual O&M cost(3rd)","14.NEM(Own & 3rd)", "15.None", "16.Perceived Cost(Own)","17.Perceived Cost(3rd)","18.CCE interest", "19.Final CCA interest", "20.Capital Cost","21.Energy Cost(Own)","22.Base-total cost(CCA)", "23.Base-total cost(Own)", "24.Base-total cost(3rd)", "25.Base choices", "26.CCE-total cost","27.CCE choices")
  
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
  Data_matrix[,6] <- Data_matrix[,5]*rnorm(n=N,mean=1,sd=CCEOwn_SD) # add noisy because the PV cost per kw can be different across PV companies. In the excel, the column 5 and 6 are combined. 
  Data_matrix[,7] <- rnorm(n=N, mean=NonCCEOwn, sd=NonCCEOwn*CCEOwn_SD)/100 # old interest rate. 
  Data_matrix[,8] <- Data_matrix[,7]+(NonCCE3rd-NonCCEOwn)/100 # 3rd party own interest rate
  Rate_table <- matrix(c(Elec1,Elec2,Elec3, Elec1_perc/100, Elec2_perc/100, Elec3_perc/100), nrow=3, ncol=2) # make electricity rate table based on the inputs
  elec_rate <- as.numeric(sample(as.character(unlist(Rate_table[,1])),N, prob=as.character(unlist(Rate_table[,2])),replace=TRUE)) 
  # sample CCA rate based on the probability
  Data_matrix[,9] <- Data_matrix[,1]*elec_rate # add monthly energy cost when getting electricity from CCA
  Data_matrix[,20]<- Data_matrix[,3]*Data_matrix[,6]# solar PV capital cost
  Data_matrix[,10]<-(-pmt(Data_matrix[,7]/12, PayPeriodOwn*12,Data_matrix[,20])) # add monthly energy cost when owning PV (Own)
  Data_matrix[,11]<- (-pmt(Data_matrix[,8]/12, PayPeriod3rd*12, Data_matrix[,20])) # add monthly energy cost when owning PV (3rd)
  Data_matrix[,12]<-(FixedOwn+VarOwn)*Data_matrix[,3]/SizeOwn #add Annual O&M cost for PVOwn
  Data_matrix[,13]<-(Fixed3rd+Var3rd)*Data_matrix[,3]/SizeOwn #add Annual O&M cost for 3rd party own
  
  for(i in 1:N){
    Data_matrix[i,14] = ifelse((Data_matrix[i,2]-Data_matrix[i,1])>0,(Data_matrix[i,2]-Data_matrix[i,1])*(elec_rate[i]+BonusOwn),(Data_matrix[i,2]-Data_matrix[i,1])*elec_rate[i]) 
  } # add NEM credits and (No Data_matrix[,15] anymore)
  Data_matrix[,16] <- rnorm(n=N, mean=PerceivedOwn, sd=PerceivedOwn*CCEOwn_SD)
  Data_matrix[,17] <- rnorm(n=N, mean=Perceived3rd, sd=Perceived3rd*NonCCE3rd_SD) # add perceived cost for both PVOwn and 3rdOwn
  CCA_discount <- (1+Discount/100)/(1+Escal/100)-1 # calculate the escalator and discount combined rate. 
  
  Data_matrix[,22] <- (Data_matrix[,9]*12)/CCA_discount*(1-1/((1+CCA_discount)^Lifetime))
  Connection_fee <- ifelse((agency=="Lancaster")|(agency=="Apple Valley"),75,145)
  Data_matrix[,23] <- ((Data_matrix[,10]-Data_matrix[,14]+Data_matrix[,16])*12+Data_matrix[,12])/(Discount/100)*(1-1/((1+Discount/100)^Lifetime))+Connection_fee
  Data_matrix[,24] <- ((Data_matrix[,11]-Data_matrix[,14]+Data_matrix[,17])*12+Data_matrix[,13])/(Discount/100)*(1-1/((1+Discount/100)^Lifetime))+Connection_fee
  
  for (i in 1:N) {
    Data_matrix[i,25] = which.min(Data_matrix[i,22:24])} # choose the lowest cost. 
  Base <- length(which(Data_matrix[,25]==2)) #count the PVOwn
  
  # these are to calculate the market share of each car model
  Marketshare <-matrix(rep(NA,3),nrow=1, ncol=3)
  for (i in 1:3){
    Marketshare[,i]= length(which(Data_matrix[,25]==i))/N
  }
  Marketshare_List <- read_csv("Marketshare.csv")
  Marketshare_Table <- (Marketshare_List[match(agency,Marketshare_List$Agency),2:4])
  Marketshare_Table[2,] <- Marketshare[1,]
  Marketshare_Table[3,] <- (Marketshare_Table[2,]-Marketshare_Table[1,])^2
  return(sum(Marketshare_Table[3,]))
}
test <-c(80,90)
TCMfunction(test)

PerceivedCost <- optim(
  c(test), TCMfunction, method = "Nelder-Mead") 

var.vec <- PerceivedCost$par

TCMfunction(var.vec)

perceived_cost <- optim( # Store optimization output in a list called OO
  c(0,0), # Guess starting values 
  simple.function, # Objective function
  upper = Inf, # Upper boundary for the decision variables
  lower = -Inf, # Lower boundary for the decision variables
  method = "L-BFGS-B") # Optimization algorithm that allows for interval specification



###################################################################################
pmt <- function(rate, nper, pv, fv=0, type=0) {
  rr <- 1/(1+rate)^nper
  res <- (-pv-fv*rr)*rate/(1-rr)
  return(res/(1+rate*type))} #pmt function

TCMfunction <- function(PerceivedOwn,Perceived3rd){
  
  
  
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
  Marketshare_List <- read_csv("Marketshare.csv")
  N = 10000 # Number of simulation
  
  ##########################################################################
  # MODEL(BASE)
SD_elec <- 0.4 # Standard deviation of Ln(electricity)
other_non_green <- round((Elec3_perc)/100*(1-Elec4_perc/100)*N,digits=0)
common_green <- round((Elec1_perc)/100*Elec4_perc/100*N,digits=0)
other_green <- round((Elec3_perc)/100*Elec4_perc/100*N,digits=0)
Care_green <- round(Elec2_perc/100*Elec4_perc/100*N,digits=0)
Care_non_green <-round(Elec2_perc/100*(1-Elec4_perc/100)*N,digits=0)
common_non_green <- N-other_non_green-common_green-other_green-Care_green-Care_non_green

ln_Mean_elec1<- log(Avg_non) 
ln_Mean_elec2<- log(Avg_CARE_non_green) 
ln_Mean_elec3<- log(Avg_non_CARE_green) 
ln_Mean_elec4<- log(Avg_CARE_green) 

Elec_dis1 <- exp(rnorm(n=common_non_green,mean=ln_Mean_elec1, sd=SD_elec))
Elec_dis2 <- exp(rnorm(n=other_non_green,mean=ln_Mean_elec1, sd=SD_elec))
Elec_dis3 <- exp(rnorm(n=Care_non_green,mean=ln_Mean_elec2, sd=SD_elec))
Elec_dis4 <- exp(rnorm(n=common_green,mean=ln_Mean_elec3, sd=SD_elec))
Elec_dis5 <- exp(rnorm(n=other_green,mean=ln_Mean_elec3, sd=SD_elec))
Elec_dis6 <- exp(rnorm(n=Care_green,mean=ln_Mean_elec4, sd=SD_elec))

Elec_distribution <- c(Elec_dis1,Elec_dis2,Elec_dis3,Elec_dis4,Elec_dis5,Elec_dis6)

Elec_distribution[Elec_distribution>3000]=3000 # Electricity consumption that is larger than 3,000 is 3,000

# these are the values we use and subject to change
Data_matrix <- matrix(rep(NA,N*27),nrow=N, ncol=27)
colnames(Data_matrix)<- c("1.Electricity Usage(Kwh/month)","2.PV Generation(kWh/month)","3.System Size(kW)", "4.Rounded Size (kW)", "5.PV cost per kW","6.PV unit cost with noise","7.Old interest","8.3rd interest", "9.Energy Cost(CCA)","10.Energy Cost(Old)","11.Energy Cost(3rd)","12.Annual O&M cost(Own)","13.Annual O&M cost(3rd)","14.NEM(Own & 3rd)", "15.None", "16.Perceived Cost(Own)","17.Perceived Cost(3rd)","18.CCE interest", "19.Final CCA interest", "20.Capital Cost","21.Energy Cost(Own)","22.Base-total cost(CCA)", "23.Base-total cost(Own)", "24.Base-total cost(3rd)", "25.Base choices", "26.CCE-total cost","27.CCE choices")

Data_matrix[,1] <- Elec_distribution # add the electricity consumption distribution in the first column
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

elec_rate <- c(rep(Elec1,common_non_green),rep(Elec3,other_non_green),rep(Elec2,Care_non_green),rep(Elec1+Elec4,common_green),rep(Elec3+Elec4,other_green),rep(Elec2+Elec4,Care_green))


Data_matrix[,9] <- Data_matrix[,1]*elec_rate # add monthly energy cost when getting electricity from CCA
Data_matrix[,20]<- Data_matrix[,3]*Data_matrix[,6]# solar PV capital cost
Data_matrix[,10]<-(-pmt(Data_matrix[,7]/12, PayPeriodOwn*12,Data_matrix[,20])) # add monthly energy cost when owning PV (Own)
Data_matrix[,11]<- (-pmt(Data_matrix[,8]/12, PayPeriod3rd*12, Data_matrix[,20])) # add monthly energy cost when owning PV (3rd)
Data_matrix[,12]<-(FixedOwn)*Data_matrix[,3] #add Annual O&M cost for PVOwn

green_dis<- c(rep(0,common_non_green),rep(0,other_non_green),rep(0,Care_non_green),rep(1,common_green),rep(1,other_green),rep(1,Care_green))


for(i in 1:N){
  Data_matrix[i,14] = ifelse((Data_matrix[i,2]-Data_matrix[i,1])<0,(Data_matrix[i,2]-Data_matrix[i,1])*elec_rate[i],ifelse(Flat_OAS=="Flat rate",(Data_matrix[i,2]-Data_matrix[i,1])*Flat_rate,(Data_matrix[i,2]-Data_matrix[i,1])*(elec_rate[i]+BonusOwn-ifelse(Price_P=="Yes",0,green_dis[i]*Elec4))))}      

Data_matrix[,17] <- rnorm(n=N, mean=Perceived3rd, sd=Perceived3rd*NonCCE3rd_SD) # add perceived cost for both PVOwn and 3rdOwn
CCA_discount <- (1+CDiscount/100)/(1+Escal/100)-1 # calculate the escalator and discount combined rate. 

Data_matrix[,22] <- (Data_matrix[,9]*12)/CCA_discount*(1-1/((1+CCA_discount)^Lifetime))
Connection_fee <- ifelse((agency=="Lancaster")|(agency=="Apple Valley"),75,145)
Data_matrix[,23] <- ((Data_matrix[,10]-Data_matrix[,14]+Data_matrix[,16])*12+Data_matrix[,12])/(CDiscount/100)*(1-1/((1+CDiscount/100)^Lifetime))+Connection_fee
Data_matrix[,24] <- ((Data_matrix[,11]-Data_matrix[,14]+Data_matrix[,17])*12+Data_matrix[,12])/(CDiscount/100)*(1-1/((1+CDiscount/100)^Lifetime))+Connection_fee

for (i in 1:N){
  Data_matrix[i,25] = which.min(Data_matrix[i,22:24])} # choose the lowest cost. 

# these are to calculate the market share of each car model
Marketshare <-matrix(rep(NA,3),nrow=1, ncol=3)
for (i in 1:3){
  Marketshare[,i]= length(which(Data_matrix[,25]==i))/N
}
Marketshare_Table <- (Marketshare_List[match(agency,Marketshare_List$Agency),2:4])
Marketshare_Table[2,] <- Marketshare[1,]
Marketshare_Table[3,] <- ((Marketshare_Table[2,]-Marketshare_Table[1,])*100)^2
rownames(Marketshare_Table)<- c("Actual Marketshare","Estimated Marketshare from Model","The difference between Actual Marketshare and Model")
return(Marketshare_Table)
}

P_Own <- 126
P_third <- 120
Marketshare <- TCMfunction(P_Own, P_third)

