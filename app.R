library(shiny)
library(readr)
library(shinydashboard)
library(reshape2)
library(ggplot2)
library(htmltools)
library(bsplus)

# seperate normal distribution
# calculate the environmental benefits, health benenfits. 
#reactive?


# Define UI for app that draws a bar graph ----
ui <-  dashboardPage(
  dashboardHeader(title="Solar Financing Program Toolkit"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Model Overview", tabName = "tab_1"),
      menuItem("User Guide", tabName = "tab_2"), 
      menuItem("[Step1] Toolkit-Inputs", tabName = "tab_3"),
      menuItem("[Step2] Toolkit-Results", tabName = "tab_4")
      
      
    )), 
  dashboardBody(tabItems(
    tabItem(tabName = "tab_1",
            fluidPage(h3("Model Overview"),
                      box(width = 12, h4("Introduction"),p("This is a tool meant to help Community Choice Energy Agencies predict the costs and benefits associated with offering a financing to install solar PV")
                      ),
                      box(width = 12, h4("Using the Toolkit"),p("To use this model, at a minimum, users will need to enter values into the Primary Inputs section of the Toolkit tab. This section includes: [NEED TO BE FILLED] There are a variety of additional inputs that allow users to users add further program specifications as appropriate. A detailed breakdown of all available input options is included in the User Guide tab.")
                      ),
                      box(width = 12, h4("Results"), p("Once the user has filled in the appropriate inputs and run the model, results will be displayed on the right-hand side of the Toolkit tab. The main results show the predicted solar PV uptakes caused by solar financing program. These results show the total number of solar financing offered, and the predicted solar PV uptakes caused by the financing program. The model then displays predicted health, greenhouse gas, and monetary impacts associated with the financing program. ")
                      ))) , 
    tabItem(tabName = "tab_2",
            fluidPage(h3("User Guide"),
                      box( width = 12, h4("Primary Inputs"),p("These inputs represent the minimum amount of information necessary to run the model. They are:"), 
                           br(),tags$div(tags$ul(tags$li("Agency (Region): Which CCE Agency will be running the program. The model uses this) information to set the correct population level and predict local emissions impacts."), 
                                                 tags$li("Total Incentives Budget: The total available budget for providing EV and PHEV incentives."),
                                                 tags$li("Year: The year that the incentives program will run."),
                                                 tags$li("Electric Vehicle (BEV) Incentive: The dollar amount that the agency will offer for each electric vehicle purchase."), 
                                                 tags$li("Plug-in Hybrid (PHEV) Incentive: The dollar amount that the agency will offer for each plug-in hybrid purchase."), 
                                                 tags$li("Energy mix: These values specify the composition of the energy mix that is used to charge electric vehicles."),  style = "font-size: 13px"))),    box( width = 12, h4("Incentive Details"),p("These allow agencies to add further details to their incentive offerings. These are included with general default values that can be altered if necessary to match the agency’s needs."), br(),tags$div(tags$ul(tags$li("Include incentive for High end BEV and luxury PHEV: These are Yes/No inputs set at No by default. If switched to Yes, the model will include Tesla and luxury plug-in hybrid vehicles among those that receive their respective incentives."), 
                                                                                                                                                                                                                                                                                                                                                                       tags$li("Federal Tax Credit Availability/Clean Vehicle Rebate Project Availability: These are Yes/No inputs set at Yes by default. If switched to No the model will remove that credit or rebate from its calculations of vehicle cost."), 
                                                                                                                                                                                                                                                         tags$li("Additional Discount EV/Plug-in: These inputs give the user the option to add additional discounts on the cost of BEVs or PHEVs. These are not included in the agency’s overall program costs and may represent discounts offered by vehicle dealers or manufacturers. They benefit the customer but are not costs incurred by the agency.")),  style = "font-size: 13px")),
                      box( width = 12, h4("Program Details"), p("These allow the user to add details about their program, including administrative costs and program length. Defaults are provided based on the pilot incentive program that Sonoma Clean Power ran in 2016. Inputs include:"), br(), tags$div(tags$ul(tags$li("Program length: The number of months that the incentive program will run, with the default set at 12."), tags$li("Number of staff required: The number of full-time employees needed to run the program."), tags$li("Administrative costs per person: The salary and administrative costs per full time employee working on the program."), tags$li("Additional implementation costs: Any additional costs to run the program that the user anticipates. Defaults have been set based on the costs to run Sonoma Clean Power’s pilot EV program."),tags$li("Percent Net Revenue: This input allows the user to set the portion of electricity sales that goes to revenues with a default set at 10%."), tags$li("Marketing effectiveness: This input represents a way to account for the role of marketing on influencing program effectiveness. The user may input the percentage of  eligible customers they expect will be aware of the program being offered. This percentage directly modifies the predicted number of rebates redeemed. Because this only modifies the number of people aware of available discounts, it does not take into account marketing that changes the likelihood of customers taking advantage of the discounts (i.e. marketing that is more or less persuasive).", footer = NULL, status = NULL,solidHeader = FALSE, background = NULL, height = NULL, collapsible = FALSE, collapsed = FALSE)),  style = "font-size: 13px")
                      ))), 
    tabItem(tabName ="tab_3",
            fluidPage(
              titlePanel("Insert all the inputs first, calibrate, and then go to the next tab to get results"),
          
              fluidRow(
                column(6,box(title = "Calibration main inputs", width=NULL, status = "success", solidHeader = TRUE, collapsible = TRUE, 
                   selectInput( inputId="Agency", "Agency (region)", choices = list("Apple Valley" = "Apple Valley", "San Francisco" = "San Francisco", "Lancaster" = "Lancaster", "MCE" ="MCE", "Peninsular"="Peninsular", "Redwood Coast"="Redwood Coast", "Sillcon Valley"="Sillcon Valley", "Sonoma"="Sonoma"), selected = "MCE")%>% shinyInput_label_embed(
                  shiny_iconlink() %>%
                    bs_embed_tooltip(title = "Agency (Region): Which CCE Agency will be running the program. The model uses this information to set the correct population level and predict local emissions impacts.",  placement = "right")), 
                  numericInput(inputId ="PerceivedOwn", "Perceived cost per month for owning solar PV($)", value = 103)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                  numericInput(inputId ="Perceived3rd", "Perceived cost per month for owning 3rd party owned solar PV ($)", value = 96)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "Fill this later",  placement = "right"))
                  
                  )),column(6,box(title = "Calibration", width=NULL, status = "success", solidHeader = TRUE, collapsible = TRUE,p("Change the perceived costs on the left side until the actual and estimated market shares are matched! Set the perceived costs and click [Calibrate]"),br(),p("If Actual makrket is higher than Estimated one, decrease the perceived cost. When each differences are below 0.01, it is good to go. For the future use without calibration, save perceived costs you find.") , actionButton("go2", "Calibrate"),br(),tableOutput("table3")
                                  
                                  ))), 
              fluidRow(
              column(3,
                     box(title = "CCE Agency Basic Information Inputs",width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                     numericInput(inputId ="rev_perc", "Percentage of electricity sales received as revenue(%)", value = 10)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                    numericInput(inputId ="Trans", "Transmission Losses (%)", value = 5)%>% shinyInput_label_embed(
                      shiny_iconlink() %>%
                        bs_embed_tooltip(title = "Fill this later",  placement = "right"))),
                    box(title = "Rate Data Inputs", width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                numericInput(inputId ="Escal", "Agency electricity cost escalation rate (%/year)",  value = 2)%>% shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                  numericInput(inputId ="Elec1", "Average electricity rate - most popular rate schedule ($/kWh)",  value = 0.238)%>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                  
                 numericInput(inputId ="Elec1_perc", "  Percentage of accounts on above rate schedule, non-CARE (%)",  value = 68)%>% shinyInput_label_embed(shiny_iconlink() %>%bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                 
                  numericInput(inputId ="Elec2", "Average CARE electricity rate ($/kWh)",  value = 0.140),
                 
                  numericInput(inputId ="Elec2_perc", " Percentage of accounts on CARE (%)",  value = 28),
                  
                  numericInput(inputId ="Elec3", "Average electricity rate - other rate schedules ($/kWh)",  value = 0.199),
                  numericInput(inputId ="Elec3_perc", "   Percentage of accounts on other rate schedules (%)",  value = 4),
                  numericInput(inputId ="Elec4", "Price premium for greener electricity ($)",  value = 0.010),
                  numericInput(inputId ="Elec4_perc", "   Percentage of accounts purchasing greener electricity (%)",  value = 1))),
                 
              
              column(3, 
                     box(title = "Energy Mix Data Inputs", width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  
                  numericInput(inputId ="mix6","Energy Mix - Biomass (%)", value = 5),       
                  numericInput(inputId ="mix7", "Energy Mix - Biogas (%)", 
                               value = 0),
                  numericInput(inputId ="mix3","Energy Mix - Geothermal (%)", value = 0),
                  numericInput(inputId ="mix8", "Energy Mix - Eligible Renewable (%)", value = 50),
                  numericInput(inputId ="mix1", "Energy Mix - Coal (%)", value = 0)%>% shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                  numericInput(inputId ="mix5","Energy Mix - Large Hydro (%)", value = 13),
                  numericInput(inputId ="mix2", "Energy Mix - Natural Gas (%)", value = 12),
                  numericInput(inputId ="mix9","Energy Mix - Nuclear (%)", 
                               value = 0),
                  numericInput(inputId ="mix4","Energy Mix - Petroleum (%)", value = 0),
                  numericInput(inputId ="mix10", "Energy Mix - Unspecified (%)", value = 20)
                  ),
                  box(title = "CCE PV Financing Program Basic Inputs", width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                       numericInput(inputId ="ProgramCost", "Program administrative costs per year ($/year)", value = 200000)%>% shinyInput_label_embed(shiny_iconlink() %>%  bs_embed_tooltip(title = "Program cost : program costs generally includes all administrative and implementation costs for the program except for loan provided.",  placement = "right")),
                      numericInput(inputId ="Budget", "Total budget available for loans ($)", value = 20000000)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Budget : Total available budgets to provide loans for solar PV installation",  placement = "right")),
                      sliderInput(inputId ="Marketing", "Percentage of solar buyers using program (%)", min = 0, max = 100, value = 10),
                      numericInput(inputId ="DefaultOwn", "Expected default rate (%)", value = 5)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                        numericInput(inputId ="Rebound", "Rebound Effect (%)", value = 3)%>% shinyInput_label_embed(
                          shiny_iconlink() %>%
                            bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                        numericInput(inputId ="Discount", "Agency discount rate (%)", value = 5)%>% shinyInput_label_embed(
                          shiny_iconlink() %>%
                            bs_embed_tooltip(title = "Fill this later",  placement = "right")),
              numericInput(inputId ="C_Discount", "Customers' discount rate (%)", value = 5)%>% shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_tooltip(title = "Fill this later",  placement = "right")),
              numericInput(inputId ="CarbonV", "Carbon Value ($/ton CO2e)", value = 13)%>% shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_tooltip(title = "Fill this later",  placement = "right")),
              selectInput(inputId="Impact", "Value of Health Impact Estimates", choices = list("Low","Mid","High"), selected = "Mid")%>%
                        shinyInput_label_embed(shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right"))
                      )),
              
              
              column(3, 
                     box(title = "Customer-Owned Solar PV Inputs", width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                     numericInput(inputId ="CapFactrOwn", "[PV-own] Average capacity factor (%)", value = 20)%>% shinyInput_label_embed(
                           shiny_iconlink() %>%
                             bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                     numericInput(inputId ="FixedOwn", "[PV-own] Average operation & maintenance costs-fixed ($/kW/year)", value = 20)%>% shinyInput_label_embed(shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                  
                      numericInput(inputId ="NonCCEOwn", "[PV-own] Existing (non-CCE) financing interest rate (%)", value = 7.9)%>% 
                      shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="CCEOwn", "[PV-own] CCE agency program financing interest rate (%)", value = 6)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="PayPeriodOwn", "[PV-own] Financing payback period (years)", value = 20)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="RebateOwn", "[PV-own] Local rebates ($/W)", value = 0)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="PercRebateOwn", "[PV-own] Percentage of accounts that can access local rebates (%)", value = 0)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="BonusOwn", "[PV-own] NEM generator payments ($/kWh)", value = 0.01),
                      numericInput(inputId ="PercGenOwn", "[PV-own] Average percentage of bill that is net generation (%)", value = 99.4))),
                      
              
              column(width=3, box(title = "3rd Party-Owned Solar PV Inputs", width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                  
                      numericInput(inputId ="CapFactr3rd", "[3rd-own] Average capacity factor (%)", value = 20)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="Fixed3rd", "[3rd-own] Average operations & maintenance cost - fixed ($/kW/year)", value = 20)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                    
                      numericInput(inputId ="NonCCE3rd", "[3rd-own] Existing (non-CCE) financing interest rate (%)", value = 9)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="PayPeriod3rd", "[3rd-own] Financing payback period (years)", value = 20)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="Rebate3rd", "[3rd-own] Local rebates ($/W)", value = 0)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="PercRebate3rd", "[3rd-own] Percentage of accounts that can access local rebates (%)", value = 0)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right")),
                      numericInput(inputId ="PercGen3rd", "[3rd-own] Average percentage of bill that is net generation (%)", value = 0)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "Fill this later",  placement = "right"))))
                  
         
                
)) #fluid page
    ),
tabItem(tabName = "tab_4",
        fluidPage(h3("To see the results, click [Calculate] button here >> ", actionButton("go", "Calculate")),
                      fluidRow(
                        column(6, 
                               box(title = "The Estimated Number of Solar Uptakes", width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, tableOutput("table1")),
                               box(title = "Total Benefits and Costs", width=12, status = "success", solidHeader = TRUE, collapsible = TRUE, tableOutput("table2"))),
                        column(6,
                               box(title = "The Estimated Number of Solar Uptakes", width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE,plotOutput("plot1")))
                        
                  )))

)))

server <- function(input, output) {
  
  Calibrate <- eventReactive(input$go2, {
    pmt <- function(rate, nper, pv, fv=0, type=0) {
      rr <- 1/(1+rate)^nper
      res <- (-pv-fv*rr)*rate/(1-rr)
      return(res/(1+rate*type))} #pmt function
    
    TCMfunction <- function(PerceivedOwn,Perceived3rd){

      agency <- input$Agency
      Escal <- input$Escal
      Elec1 <- input$Elec1
      Elec2_perc <- input$Elec2_perc
      Elec2 <- input$Elec2
      Elec1_perc <- input$Elec1_perc
      Elec3 <- input$Elec3
      Elec3_perc <- input$Elec3_perc
      Elec4 <- input$Elec4
      Elec4_perc <- input$Elec4_perc
      Discount <- input$Discount
      
      CapFactrOwn <-input$CapFactrOwn  
      FixedOwn <- input$FixedOwn
      NonCCEOwn <- input$NonCCEOwn
      CCEOwn <- input$CCEOwn
      PayPeriodOwn <- input$PayPeriodOwn
      RebateOwn <- input$RebateOwn
      PercRebateOwn <- input$PercRebateOwn
      BonusOwn <- input$BonusOwn
      PercGenOwn <- input$PercGenOwn
      Marketing <- input$Marketing 
      DefaultOwn <- input$DefaultOwn
      CapFactr3rd <- input$CapFactr3rd  
      Fixed3rd <- input$Fixed3rd
      NonCCE3rd <- input$NonCCE3rd
      PayPeriod3rd <- input$PayPeriod3rd
      Rebate3rd <- input$Rebate3rd
      PercRebate3rd <- input$PercRebate3rd 
      Bonus3rd <- input$Bonus3rd 
      PercGen3rd <- input$PercGen3rd
      
      CCEOwn_SD <- 0.2
      NonCCE3rd_SD <- 0.2
      Lifetime <- 25
      
      Av_elec <- read_csv("Average_elec.csv")
      PVcost <- read_csv("PV_cost.csv")
      Marketshare_List <- read_csv("Marketshare.csv")
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
      Data_matrix[,12]<-(FixedOwn)*Data_matrix[,3] #add Annual O&M cost for PVOwn
      Data_matrix[,13]<-(Fixed3rd)*Data_matrix[,3] #add Annual O&M cost for 3rd party own
      
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
      
      # these are to calculate the market share of each car model
      Marketshare <-matrix(rep(NA,3),nrow=1, ncol=3)
      for (i in 1:3){
        Marketshare[,i]= length(which(Data_matrix[,25]==i))/N
      }
      Marketshare_Table <- (Marketshare_List[match(agency,Marketshare_List$Agency),2:4])
      Marketshare_Table[2,] <- Marketshare[1,]
      Marketshare_Table[3,] <- ((Marketshare_Table[2,]-Marketshare_Table[1,])*100)^2
      rownames(Marketshare_Table)<- c("Actual Marketshare","Estimated Marketshare from Model","The difference bewteen Actual Marketshare and Model")
      return(Marketshare_Table)
    }
  
    P_Own <- input$PerceivedOwn
    P_third <- input$Perceived3rd
    
    Marketshare <- TCMfunction(P_Own, P_third)
    
  })

  
    
##########################################################################  
  
  TCM <- eventReactive(input$go, {
    pmt <- function(rate, nper, pv, fv=0, type=0) {
      rr <- 1/(1+rate)^nper
      res <- (-pv-fv*rr)*rate/(1-rr)
      return(res/(1+rate*type))} #pmt function

    
    
    agency <- input$Agency
    ProgramCost <- input$ProgramCost
    Budget <- input$Budget
    rev_perc <- input$rev_perc
    Escal <- input$Escal
    Elec1 <- input$Elec1
    Elec2_perc <- input$Elec2_perc
    Elec2 <- input$Elec2
    Elec1_perc <- input$Elec1_perc
    Elec3 <- input$Elec3
    Elec3_perc <- input$Elec3_perc
    Elec4 <- input$Elec4
    Elec4_perc <- input$Elec4_perc
    
    mix1 <- input$mix1
    mix2 <- input$mix2
    mix3 <- input$mix3
    mix4 <- input$mix4 
    mix5<- input$mix5
    mix6 <- input$mix6
    mix7 <- input$mix7
    mix8 <- input$mix8
    mix9 <- input$mix9
    mix10 <- input$mix10
    
    Impact <- input$Impact
    Rebound <- input$Rebound
    Trans <- input$Trans
    Discount <- input$Discount
    Carbon_p <-input$Carbon_p
    
    CapFactrOwn <-input$CapFactrOwn  
    FixedOwn <- input$FixedOwn
    NonCCEOwn <- input$NonCCEOwn
    CCEOwn <- input$CCEOwn
    PayPeriodOwn <- input$PayPeriodOwn
    RebateOwn <- input$RebateOwn
    PercRebateOwn <- input$PercRebateOwn
    BonusOwn <- input$BonusOwn
    PercGenOwn <- input$PercGenOwn
    Marketing <- input$Marketing 
    DefaultOwn <- input$DefaultOwn
    
    
    CapFactr3rd <- input$CapFactr3rd  
    Fixed3rd <- input$Fixed3rd
    NonCCE3rd <- input$NonCCE3rd
    PayPeriod3rd <- input$PayPeriod3rd
    Rebate3rd <- input$Rebate3rd
    PercRebate3rd <- input$PercRebate3rd 
    Bonus3rd <- input$Bonus3rd 
    PercGen3rd <- input$PercGen3rd
    
    CCEOwn_SD <- 0.2
    NonCCE3rd_SD <- 0.2
    PerceivedOwn <- input$PerceivedOwn
    Perceived3rd <- input$Perceived3rd
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
    Data_matrix[,12]<-(FixedOwn)*Data_matrix[,3] #add Annual O&M cost for PVOwn
    Data_matrix[,13]<-(Fixed3rd)*Data_matrix[,3] #add Annual O&M cost for 3rd party own
    
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
    Model_CCE <- length(which(Data_matrix[,27]==3)) #count the CCEOwn
    CCE <- ifelse(CCEOwn>NonCCEOwn, Base,Model_CCE)
    ##########################################################################
    # Emission
    
    Suitable_H <- as.numeric(House[match(agency,House$Agency),2])
    Average_loan <- mean(Data_matrix[Data_matrix[,27]==3,20])
    Max_house <- Budget/Average_loan # max number of houses based on the budget
    Total_uptakes <- ifelse(CCEOwn>NonCCEOwn,0,Suitable_H*CCE/N*Marketing/100) # predicted uptakes based on the model
    Final_uptakes <- ifelse(Max_house>Total_uptakes,Total_uptakes, Max_house) # Final solar pv uptakes. 
    Uptakes_caused <- Suitable_H*(CCE-Base)/N*Marketing/100 # the pv uptakes when not considering budget constraint
    Final_uptakes_caused <- ifelse(Max_house<Total_uptakes,Max_house*(CCE-Base)/CCE, Uptakes_caused) # the final pv uptakes caused by program.
    
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
    GHG_reduction <-Lifetime_Emission_Reduction[1,1]
    Budget_remained <- Budget-Loan_provided
    GHG_cost <- -Revenue_change/GHG_reduction
    GHG_reduction_value <- GHG_reduction*Carbon_p
    
    
    FinalTable <- matrix(c(Final_uptakes_caused,Final_uptakes,Loan_provided, Budget_remained ,Revenue_lost, Collected_revenue, Revenue_change, GHG_reduction,GHG_cost,GHG_reduction_value,Total_health_benefit), nrow=11, ncol=1)
    rownames(FinalTable)<-c("Solar uptakes caused by program", "Total solar uptakes","Total value of loans provided ($)","Program budget remaining ($)","Revenue lost due to PV uptakes ($)","Revenue from collected principal and interest ($)","Total revenue change due to program ($)", "GHG emission averted (tons)","Cost per ton of GHG emission reduction ($/ton CO2e)","Societal value of GHG emission reductions ($)","Health improvements ($)")
    colnames(FinalTable)<-c("Estimated Results")
    print(FinalTable)
    
  })
  
  
  output$table1 <- renderTable({
    TCM <- TCM()
    Finalsale <- as.data.frame(t(TCM[1:2,1]))
    
  }, rownames = TRUE, colnames = TRUE, digits=0)
  
  output$plot1 <- renderPlot({
    TCM <- TCM()
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
      geom_bar(stat = "identity", width = 0.5)+
      ylab("Solar PV Installation")+
      xlab("")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), legend.title=element_blank(),text=element_text(size=15))
    
    
    
    
    
  })
  
  output$table2 <- renderTable({ 
    TCM <- TCM()
    Cost_Benfit <- as.data.frame(TCM[3:11,1])
  },rownames = TRUE, colnames=FALSE, digits=0)
  
  
  output$table3 <- renderTable({ 
    P_value <- Calibrate()
  },rownames = TRUE, colnames=TRUE, digits=4)
  

  
  
}

shinyApp(ui, server) 