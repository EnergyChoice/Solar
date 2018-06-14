library(shiny)
library(readr)
library(shinydashboard)
library(reshape2)
library(ggplot2)
library(htmltools)
library(bsplus)



# Define UI for app that draws a bar graph ----
ui <-  dashboardPage(
  dashboardHeader(title="Solar Financing Program"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Tool Overview", tabName = "tab_1"),
      menuItem("User Guide", tabName = "tab_2"), 
      menuItem("Inputs", tabName = "tab_3"),
      menuItem("Results", tabName = "tab_4")
      
      
    )), 
  dashboardBody(tabItems(
    tabItem(tabName = "tab_1",
            fluidPage(h3("Tool Overview"),
                      box(width = 12, h4("Introduction"),p("This is a tool meant to help Community Choice Energy agencies predict costs and benefits associated with offering financing to their residential customers to install solar photovoltaic (PV) systems for their homes. Based on a variety of program, agency, and other specifications, the model predicts the number of solar installations that will be directly caused by a financing program, and then calculates associated financial implications, greenhouse gas (GHG) emission reductions, and health impacts. "),p("Users can visualize relationships among program design parameters by using the tool with a range of parameter values and recording the results. For example, a user might hold all input values constant and vary only agency program interest rate in order to isolate its effect on cost per ton of GHG emission reductions, or another tool output."),p("This tool is available at www.ccetoolkit.weebly.com and from the Center for Climate Protection (www.climateprotection.org). Also available are tool documentation and the same tool in Microsoft Excel format.")
                      ),
                      box(width = 12, h4("Using the Tool: Inputs"),p("The tool requires that users enter values for the agency’s energy mix, customer electricity usage, cost to the customer of electricity from the agency and solar, and some program design specifications. There are a variety of additional fields in the Inputs page that allow users to users add further program specifications as desired. A detailed explanation of all available input options is included in the User Guide page.")
                      ),
                      box(width = 12, h4("Using the Tool: Results"), p("The tool's primary results are the predicted solar PV uptake caused by solar financing program.  The tool's secondary results are the predicted greenhouse gase, health, and monetary impacts associated with the financing program.")
                      ))) , 
    tabItem(tabName = "tab_2",
            fluidPage(h3("User Guide"),
                      box( width = 12, h4("Basic Information"),p("These inputs are required to run the tool."), 
                           br(),tags$div(tags$ul(tags$li("Agency: The CCE agency that will run the program. The model uses this information to set the baseline market share, average solar system size, cost of solar, and electricity usage."), 
                                                 tags$li("Percent Net Revenue: The percentage of electricity sales that goes to revenues."),
                                                 tags$li("Transmission losses (%): the average percentage of electricity supplied by the CCE agency that is lost during transmission."),
                                                 tags$li("These values specify the composition of the energy mix to calculate the greenhouse gas emissions that are avoided when electricity is sourced from the homeowner’s solar panels rather than agency supply."),  style = "font-size: 13px"))),
                      box( width = 12, h4("Electricity Rates & Usage"),p("These required inputs affect consumer decisions and are also used to calculate the foregone revenue from CCE agency electricity sales to customers who will get electricity from their solar panels instead of the grid."), p("Rate data:"), 
                           tags$div(tags$ul(tags$li("Electricity price escalation rate"), 
                                                 tags$li("Average rate - most common schedule"),
                                                 tags$li("Average rate - CARE"),
                                                 tags$li("Average rate - other schedules"),  
                                                 tags$li("Greener product premium"),style = "font-size: 13px")), 
                      p("Usage data (non-NEM accounts):"),
                          tags$div(tags$ul(tags$li("Percentage of non-CARE accounts on most common rate schedule"), 
                                                tags$li("Percentage of accounts on CARE"),
                                                tags$li("Percentage of accounts on other rate schedules"),
                                                tags$li("Percentage of accounts purchasing greener electricity"),   
                                                tags$li("Average electricity usage - non-Care, non-green"),
                                                tags$li("Average electricity usage - CARE, non-green"),
                                                tags$li("Average electricity usage - non-CARE, green"),                       
                                                tags$li("Average electricity usage - CARE, green"),style = "font-size: 13px"))),
                      box( width = 12, h4("Energy Mix"),p("These required inputs specify the composition of the energy mix to calculate the effects on anthropogenic and biogenic greenhouse gas and criteria pollutant emissions when electricity is sourced from the homeowner’s solar panels rather than the agency."),  style = "font-size: 13px"),                      
                      box( width = 12, h4("Program Details"),p("These required inputs are used to calculate consumer uptake and resulting monetary costs and benefits of the program. Some are initially populated with default values that can be altered to match the agency’s needs."), 
                           tags$div(tags$ul(tags$li("Total budget available for loans"),
                                                 tags$li("Program administrative costs"),
                                                 tags$li("Agency’s discount rate"),
                                                 tags$li("Agency program financing interest rate"),
                                                 tags$li("Average existing (non-CCE) financing interest rate"),
                                                 tags$li("Financing payback period"),
                                                 tags$li("NEM rate credited"),
                                                 tags$li("If \"Flat Rate\" is chosen, rate"),
                                                 tags$li("If \"OAS\" is chosen, bonus payment"),
                                                 tags$li("Credit green premium in NEM?"), style = "font-size: 13px"))),
                      box( width = 12, h4("Additional Inputs"), p("These optional inputs allow users to add details about factors affecting both consumer decisions and agency cost-benefit calculations, enabling more accurate predictions of consumer uptake and resulting financial implications, greenhouse gas emission reductions, and health impacts. They are initially populated with default values that can be altered to match the agency’s needs."), 
                           tags$div(tags$ul(tags$li("Expected default rate"), 
                                                  tags$li("Marketing effectiveness"),
                                                  tags$li("Rebound effect"), 
                                                  tags$li("Customer’s discount rate"),
                                                  tags$li("Societal discount rate"),
                                                  tags$li("Carbon value"),
                                                  tags$li("Health impact level"), 
                                                  tags$li("Average PV lifetime"),
                                                  tags$li("PV average capacity factor"),
                                                  tags$li("PV average operations & maintenance cost"),
                                                  tags$li("Average percentage of bill that is net generation"),
                                                  tags$li("Local rebates"),
                                                  tags$li("Percentage of accounts that qualify for local rebates",
                                                  tags$li("Decrease in PV cost per watt since 2016"),
                                                          footer = NULL, status = NULL,solidHeader = FALSE, background = NULL, height = NULL, collapsible = FALSE, collapsed = FALSE)),  style = "font-size: 13px")
                      ))), 
    tabItem(tabName ="tab_3",
            fluidPage(
          
              fluidRow(h2("Instruction:"), h3("1) Set all inputs in blue boxes below."), h3("2) Calibrate as directed in Calibration box."),h3("3) Once satisfied with calibration, proceed to Results page."),br(),
                column(12,box(title = "Calibration", width=NULL, status = "success", solidHeader = TRUE, collapsible = TRUE,
                             p("Do this step after you enter values in the blue input boxes below. To calibrate the tool:"),
p("1)	Click the Calibrate button once and wait 10-30 seconds for results to appear in the Calibration Results box."),
p("2)	If observed market share is HIGHER than the predicted market share in the CO and/or TPO column(s), DECREASE the respective CO and/or TPO perceived cost in the entry fields below. If the observed market share is LOWER than the predicted market share, INCREASE the perceived costs."),
p("3)	Repeat steps 1) and 2) until the differences between the observed and predicted market shares in the Calibration Results box are under 0.02 for both customer-owned (CO) and third party-owned (TPO) solar PV. This is a trial-and-error process, and may take 5-10 tries."),
p("4)	Proceed to the Results page"),
p(" For future use without calibration, record these perceived cost values elsewhere. Recalibrate if you change any of the following inputs: Agency, Electricity Rates & Usage, Average existing (non-CCE) financing interest rate, any NEM-related inputs, Customer’s Discount Rate, or the last seven Additional Details inputs."),br(),
                             
                  numericInput(inputId ="PerceivedOwn", "Perceived cost per month for owning solar PV($)", value = 120)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "If Actual Marketshare of PVOwn is higher than Estimated Marketshare decrease this value by several units.",  placement = "left")),
                  numericInput(inputId ="Perceived3rd", "Perceived cost per month for owning 3rd party owned solar PV ($)", value = 126)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "If Actual Marketshare of 3rdOwn is higher than Estimated Marketshare decrease this value by several units.",  placement = "left")),actionButton("go2", "Calibrate"),br(),tableOutput("table3")
                  
                  )),column(6)), 
              fluidRow(
              column(3,
                     box(title = "CCE Agency Basic Information Inputs",width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                         selectInput(inputId="Agency", "Agency (region)",
                                     choices = list("Apple Valley Choice Energy" = "Apple Valley", "CleanPowerSF" = "San Francisco", "Lancaster Choice Energy" = "Lancaster", "MCE Clean Energy" ="MCE", "Peninsula Clean Energy"="Peninsula", "Redwood Coast Energy Authority"="Redwood Coast", "Silicon Valley Clean Energy"="Silicon Valley", "Sonoma Clean Power"="Sonoma"), selected = "MCE")%>%
                           shinyInput_label_embed(
                             shiny_iconlink() %>%
                               bs_embed_tooltip(
                                 title = "Which CCE agency will be running the program. The model uses this information to set the correct population level and determine the number of suitable homes for solar PV installation.
                                 ", placement = "right")),
                     numericInput(inputId ="rev_perc", "Percentage of electricity sales received as revenue(%)", value = 10)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "The % revenue from electricity sales that results in net revenue",  placement = "right")),
                    numericInput(inputId ="Trans", "Transmission Losses (%)", value = 4.2)%>% shinyInput_label_embed(
                      shiny_iconlink() %>%
                        bs_embed_tooltip(title = "This input accounts for electricity losses during transmission from the electricity source to the vehicle. The default value is set at 5%, representing the US average transmission losses, according to the EIA.",  placement = "right"))),
                    box(title = "Rate Data Inputs", width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                numericInput(inputId ="Escal", "Agency electricity cost escalation rate (%/year)",  value = 2)%>% shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_tooltip(title = "the expected increase in electricity rates to customers each year",  placement = "right")),
                  numericInput(inputId ="Elec1", "Average electricity rate - most popular rate schedule ($/kWh)",  value = 0.238)%>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip(title = "the average electricity rate charged to regular customers on their monthly bill",  placement = "right")),
                  
                 numericInput(inputId ="Elec1_perc", "  Percentage of accounts on above rate schedule, non-CARE (%)",  value = 68)%>% shinyInput_label_embed(shiny_iconlink() %>%bs_embed_tooltip(title = "the percentage of your agency’s customers who are typical customers on the above rate and are not in the CARE program",  placement = "right")),
                 
                  numericInput(inputId ="Elec2", "Average CARE electricity rate ($/kWh)",  value = 0.140)%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_tooltip(
                        title = "the average adjusted electricity rate charged to customers in the CARE program", placement = "right")),
                 
                  numericInput(inputId ="Elec2_perc", " Percentage of accounts on CARE (%)",  value = 28)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = " the percentage of your agency’s customers who are in the CARE program",  placement = "right")),
                  
                  numericInput(inputId ="Elec3", "Average electricity rate - other rate schedules ($/kWh)",  value = 0.199)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = " the percentage of your agency’s customers who are in the CARE program",  placement = "right")),
                  numericInput(inputId ="Elec3_perc", "   Percentage of accounts on other rate schedules (%)",  value = 4)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "the percentage of your agency’s customers who are not in the aforementioned categories",  placement = "right")),
                  numericInput(inputId ="Elec4", "Price premium for greener electricity ($)",  value = 0.010)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "the additional cost charged to consumers on getting electricity from greener mixes than the standard one offered by the agency",  placement = "right")),
                  numericInput(inputId ="Elec4_perc", "   Percentage of accounts purchasing greener electricity (%)",  value = 1)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "the percentage of your agency’s customers who are purchasing greener electricity than the baseline agency offering",  placement = "right")),
                numericInput(inputId ="Avg_non", "Average electricity usage - non-CARE, non-green (kWh/month)",  value = 574 )%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "Average electricity consumption per month of non-CARE non-green customers",  placement = "right")),
                numericInput(inputId ="Avg_CARE_non_green", "Average electricity usage - CARE, non-green (kWh/month)",  value = 339)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "Average electricity consumption per month of CARE non-green customers",  placement = "right")),
                numericInput(inputId ="Avg_non_CARE_green", "Average electricity usage - non-CARE, green (kWh/month)",  value = 387)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "Average electricity consumption per month of non-CARE green customers",  placement = "right")),
                numericInput(inputId ="Avg_CARE_green", "Average electricity usage - CARE, green (kWh/month)",  value = 294)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "Average electricity consumption per month of CARE green customers",  placement = "right"))
                )),
                 
              
              column(3, 
                     box(title = "Energy Mix Data Inputs", width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  
                  numericInput(inputId ="mix6","Energy Mix - Biomass (%)", value = 5)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "these values specify the composition of the agency’s energy mix that supplies agency customers’ electricity",  placement = "right")),       
                  numericInput(inputId ="mix7", "Energy Mix - Biogas (%)", 
                               value = 0),
                  numericInput(inputId ="mix3","Energy Mix - Geothermal (%)", value = 0),
                  numericInput(inputId ="mix8", "Energy Mix - Eligible Renewable (%)", value = 50),
                  numericInput(inputId ="mix1", "Energy Mix - Coal (%)", value = 0),
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
                      sliderInput(inputId ="Marketing", "Marketing Efectiveness (%)", min = 0, max = 100, value = 10)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "This input represents a way to account for the role of marketing on influencing program effectiveness. The user may input the percentage of eligible customers they expect will be aware of the program being offered. This percentage directly modifies the predicted number of loans offered and resulting solar uptake. Because this only modifies the number of people aware of the financing program, it does not take into account marketing that changes the likelihood of customers taking advantage of the discounts (i.e. marketing that is more or less persuasive). Recommended realistic input is between 5-10%.",  placement = "right")),
                      
                      
                      numericInput(inputId ="Carbon_p", "Carbon Value ($/ton CO2e)", value = 13)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "The monetary value given to an avoided ton of CO2e emissions. The default value is set at $13/ton, based on the 2016 California market trading rate for CO2.",  placement = "right")),
                      selectInput(inputId="Impact", "Value of Health Impact Estimates", choices = list("Low","Mid","High"), selected = "Mid")%>%
                        shinyInput_label_embed(shiny_iconlink() %>%
                                                 bs_embed_tooltip(title = "the relative monetary value (low, med, high)  that the agency places on emissions from electricity production (based on energy mix) that are avoided when solar PV is used",  placement = "right")),
                      
                      numericInput(inputId ="DefaultOwn", "Expected default rate (%)", value = 0)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "the % of financing program participants that you expect will default on their loans",  placement = "right")),
                        numericInput(inputId ="Rebound", "Rebound Effect (%)", value = 3)%>% shinyInput_label_embed(
                          shiny_iconlink() %>%
                            bs_embed_tooltip(title = "The rebound effect describes the tendency for individuals to increase their electricity consumption once they switch to using solar panels. Input a value estimate for the % increase in average electricity consumption after solar panels are installed. Leave as 3% default if you are unsure.",  placement = "right")),
                        numericInput(inputId ="ADiscount", "Agency discount rate (%)", value = 2.5)%>% shinyInput_label_embed(
                          shiny_iconlink() %>%
                            bs_embed_tooltip(title = "The annual rate at which future costs and benefits are devalued by the agency. The default value is set at 2.5%.",  placement = "right")),
              numericInput(inputId ="CDiscount", "Customers' discount rate (%)", value = 6)%>% shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_tooltip(title = "The annual rate at which future costs and benefits are devalued by the average customer. The default value is set at 6%.",  placement = "right")),
              numericInput(inputId ="SDiscount", "Societal goods discount rate (%)", value = 5)%>% shinyInput_label_embed(
                shiny_iconlink() %>%
                  bs_embed_tooltip(title = "The annual rate at which future costs and benefits are devalued by the society. (Used to calculate heatlh and environmental costs/benefits that impact the society.) The default value is set at 2.5%.",  placement = "right"))
                      )),
              
              column(3, 
                     box(title = "Customer-Owned Solar PV Inputs", width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                  
                      numericInput(inputId ="NonCCEOwn", " Existing (non-CCE) financing interest rate (%)", value = 7.9)%>% 
                      shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "The average financing interest rates that solar buyers can currently get on the market without the CCE agency financing program.",  placement = "right")),
                      numericInput(inputId ="CCEOwn", " CCE agency program financing interest rate (%)", value = 6)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "the financing interest rate that the agency would like to offer to their customers",  placement = "right")),
                      numericInput(inputId ="PayPeriodOwn", " Financing payback period (years)", value = 20)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "the length of time given to the financing customer to pay back their loan plus interest",  placement = "right")),
                      numericInput(inputId ="RebateOwn", " Local rebates ($/W)", value = 0.5)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "any local PV rebates available to customers within the agency region",  placement = "right")),
                      numericInput(inputId ="PercRebateOwn", " Percentage of agency customer accounts with access to local rebates (%)", value = 0),
                      
                      selectInput(inputId="Flat_OAS", " NEM rate credited", choices = list("Flat rate","OAS"), selected = "Flat rate"),
                      numericInput(inputId ="Flat_rate", "Rate credited if Flat rate is chosen ($/kWh)", value = 0.06)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = " the rate at which money is credited back to the PV system owners who participate in NEM",  placement = "right")),
                      numericInput(inputId ="BonusOwn", "NEM bonus payments if OAS is chosen ($/kWh)", value = 0.01)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = " the rate at which money is credited back to the PV system owners who participate in NEM",  placement = "right")),
                      selectInput(inputId="Price_P", "Credit greener product customers with a price premium?", choices = list("Yes","No"), selected = "Yes"),
                      numericInput(inputId ="Lifetime", "Average solar PV panel lifetime (years)", value = 25),
                      numericInput(inputId ="CapFactrOwn", " Average capacity factor (%)", value = 20),
                      numericInput(inputId ="FixedOwn", " Average fixed operation & maintenance costs ($/kW/year)", value = 20),
                      numericInput(inputId ="PercGenOwn", " Average percentage of bill that is net generation (%)", value = 99.4)%>% shinyInput_label_embed( shiny_iconlink() %>% bs_embed_tooltip(title = "the % of electricity of a homeowner’s bill that is generated by the PV system in excess of the electricity used. (For example, if a homeowner’s PV system produced 105% of the homeowner’s monthly usage, then the net generation is 5%)",  placement = "right"))
                     )),
                      
              
              column(width=3, box(title = "3rd Party-Owned Solar PV Inputs", width=NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                  numericInput(inputId ="NonCCE3rd", " Existing (non-CCE) financing interest rate (%)", value = 9)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = " the average financing interest rates that solar buyers can currently get on the market without the CCE agency financing program.",  placement = "left")),
                      numericInput(inputId ="PayPeriod3rd", " Financing payback period (years)", value = 20)%>% shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_tooltip(title = "the length of time given to the financing customer to pay back their loan plus interest",  placement = "left"))
                      ))
                  
         
                
)) #fluid page
    ),
tabItem(tabName = "tab_4",
        fluidPage(h3("To see results, click Calculate button >>", actionButton("go", "Calculate")),
                      fluidRow(
                        column(6, 
                               box(title = "Total and Program-Caused Solar Installation Predictions", width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, tableOutput("table1")),
                               box(title = "Program-Caused Installations Benefits and Costs", width=12, status = "success", solidHeader = TRUE, collapsible = TRUE, tableOutput("table2"))),
                        column(6,
                               box(title = "Total and Program-Caused Solar Installation Predictions", width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE,plotOutput("plot1")))
                        
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
      Avg_non <- input$Avg_non
      Avg_CARE_non_green <- input$Avg_CARE_non_green
      Avg_non_CARE_green <- input$Avg_non_CARE_green
      Avg_CARE_green <- input$Avg_CARE_green
      
      
      CDiscount <- input$CDiscount
      
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
      NonCCE3rd <- input$NonCCE3rd
      PayPeriod3rd <- input$PayPeriod3rd

      
      Flat_OAS <- input$Flat_OAS
      Flat_rate <- input$Flat_rate
      BonusOwn <- input$BonusOwn
      Price_P <- input$Price_P
      
      CCEOwn_SD <- 0.2
      NonCCE3rd_SD <- 0.2
      Lifetime <- input$Lifetime
      
      Av_elec <- read_csv("Average_elec.csv")
      PVcost <- read_csv("PV_cost.csv")
      Marketshare_List <- read_csv("Marketshare.csv")
      N = 20000 # Number of simulation
      
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
      Data_matrix[,12]<-(FixedOwn)*Data_matrix[,3] #add Annual O&M cost for PVOwn and 3rdOwn
      green_dis<- c(rep(0,common_non_green),rep(0,other_non_green),rep(0,Care_non_green),rep(1,common_green),rep(1,other_green),rep(1,Care_green))
      for(i in 1:N){
        Data_matrix[i,14] = ifelse((Data_matrix[i,2]-Data_matrix[i,1])<0,(Data_matrix[i,2]-Data_matrix[i,1])*elec_rate[i],ifelse(Flat_OAS=="Flat rate",(Data_matrix[i,2]-Data_matrix[i,1])*Flat_rate,(Data_matrix[i,2]-Data_matrix[i,1])*(elec_rate[i]+BonusOwn-ifelse(Price_P=="Yes",0,green_dis[i]*Elec4))))}      
      Data_matrix[,16] <- rnorm(n=N, mean=PerceivedOwn, sd=PerceivedOwn*CCEOwn_SD)                     
      Data_matrix[,17] <- rnorm(n=N, mean=Perceived3rd, sd=Perceived3rd*NonCCE3rd_SD) # add perceived cost for both PVOwn and 3rdOwn
      CCA_discount <- (1+CDiscount/100)/(1+Escal/100)-1 # calculate the escalator and discount combined rate. 
      
      Data_matrix[,22] <- (Data_matrix[,9]*12)/CCA_discount*(1-1/((1+CCA_discount)^Lifetime))
      Connection_fee <- ifelse((agency=="Lancaster")|(agency=="Apple Valley"),75,145)
      Data_matrix[,23] <- ((Data_matrix[,10]-Data_matrix[,14]+Data_matrix[,16])*12+Data_matrix[,12])/(CDiscount/100)*(1-1/((1+CDiscount/100)^Lifetime))+Connection_fee
      Data_matrix[,24] <- ((Data_matrix[,11]-Data_matrix[,14]+Data_matrix[,17])*12)/(CDiscount/100)*(1-1/((1+CDiscount/100)^Lifetime))+Connection_fee
      
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
      rownames(Marketshare_Table)<- c("Observed baseline market share","Predicted baseline market share","The difference between observed and predicted market shares")
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
    Avg_non <- input$Avg_non
    Avg_CARE_non_green <- input$Avg_CARE_non_green
    Avg_non_CARE_green <- input$Avg_non_CARE_green
    Avg_CARE_green <- input$Avg_CARE_green
    
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
    CDiscount <- input$CDiscount
    ADiscount <- input$ADiscount
    SDiscount <- input$SDiscount
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
    
    NonCCE3rd <- input$NonCCE3rd
    PayPeriod3rd <- input$PayPeriod3rd
    
    Flat_OAS <- input$Flat_OAS
    Flat_rate <- input$Flat_rate
    BonusOwn <- input$BonusOwn
    Price_P <- input$Price_P

    CCEOwn_SD <- 0.2
    NonCCE3rd_SD <- 0.2
    PerceivedOwn <- input$PerceivedOwn
    Perceived3rd <- input$Perceived3rd
    Lifetime <- input$Lifetime
    
    
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
      Data_matrix[i,14] = ifelse((Data_matrix[i,2]-Data_matrix[i,1])<0,(Data_matrix[i,2]-Data_matrix[i,1])*elec_rate[i],ifelse(Flat_OAS=="Flat rate",(Data_matrix[i,2]-Data_matrix[i,1])*Flat_rate,(Data_matrix[i,2]-Data_matrix[i,1])*(elec_rate[i]+BonusOwn-ifelse(Price_P=="Yes",0,green_dis[i]*Elec4))))} # add NEM credits and (No Data_matrix[,15] anymore)
    
    Data_matrix[,16] <- rnorm(n=N, mean=PerceivedOwn, sd=PerceivedOwn*CCEOwn_SD)
    Data_matrix[,17] <- rnorm(n=N, mean=Perceived3rd, sd=Perceived3rd*NonCCE3rd_SD) # add perceived cost for both PVOwn and 3rdOwn
    CCA_discount <- (1+CDiscount/100)/(1+Escal/100)-1 # calculate the escalator and discount combined rate. 
    
    Data_matrix[,22] <- (Data_matrix[,9]*12)/CCA_discount*(1-1/((1+CCA_discount)^Lifetime))
    Connection_fee <- ifelse((agency=="Lancaster")|(agency=="Apple Valley"),75,145)
    Data_matrix[,23] <- ((Data_matrix[,10]-Data_matrix[,14]+Data_matrix[,16])*12+Data_matrix[,12])/(CDiscount/100)*(1-1/((1+CDiscount/100)^Lifetime))+Connection_fee
    Data_matrix[,24] <- ((Data_matrix[,11]-Data_matrix[,14]+Data_matrix[,17])*12)/(CDiscount/100)*(1-1/((1+CDiscount/100)^Lifetime))+Connection_fee
    
    for (i in 1:N){
      Data_matrix[i,25] = which.min(Data_matrix[i,22:24])} # choose the lowest cost. 
    Base <- length(which(Data_matrix[,25]==2)) #count the PVOwn
    
    ##########################################################################
    # MODEL(CCE)
    
    Data_matrix[,18]<- Data_matrix[,7]-(NonCCEOwn-CCEOwn)/100
    Data_matrix[,19]<- ifelse(Data_matrix[,7]<CCEOwn/100, Data_matrix[,7], ifelse(Data_matrix[,7]<NonCCEOwn/100, CCEOwn/100, Data_matrix[,18])) #Final CCE interest rate
    Data_matrix[,21]<- -pmt(Data_matrix[,19]/12, PayPeriodOwn*12, Data_matrix[,20]) # monthly energy cost (CCE)
    
    Data_matrix[,26] <- ((Data_matrix[,21]-Data_matrix[,14]+Data_matrix[,16])*12+Data_matrix[,12])/(CDiscount/100)*(1-1/(1+CDiscount/100)^Lifetime)
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
    Emission_total <- Energymix %*% Emission
    Biogenic <- Energymix[1,6]*Emission[6,]
    Emission_Factor <- Emission_total-Biogenic
    
    
    Degrade_matrix <- matrix(rep(NA,Lifetime*2),ncol=2)
    Degrade_matrix[1,1] <- 1
    Degrade_matrix[2,1] <- 1-0.02
    Degrade_matrix[1,2] <- 1/(1+SDiscount/100)
    Degrade_matrix[2,2] <- Degrade_matrix[2,1]/(1+SDiscount/100)^2
    
    for (i in 3:Lifetime){
      Degrade_matrix[i,1] <- Degrade_matrix[i-1,1]-0.008
      Degrade_matrix[i,2] <- Degrade_matrix[i,1]/(1+SDiscount/100)^i
    }
    colnames(Degrade_matrix) <- c("degradation","discounted")
    Aggreated_degradation <- sum(Degrade_matrix[,1])
    
    Lifetime_Emission_Reduction <- as.matrix(Emission_Factor[1,]*Aggreated_degradation*Final_uptakes_caused*Average_Gen/1000*(1+Trans/100)/(1+Rebound/100))
    Lifetime_Emission_Reduction_Biogenic <- as.matrix(Biogenic*Aggreated_degradation*Final_uptakes_caused*Average_Gen/1000*(1+Trans/100)/(1+Rebound/100))
    Biogenic_emission <-Lifetime_Emission_Reduction_Biogenic[1,]
    
    
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
    
    Collected_revenue <- (1-DefaultOwn/100)*(Monthly_payment)/(ADiscount/100/12)*(1-1/(ADiscount/100/12+1)^(Number_month))*Final_uptakes
    Loan_provided <- Average_loan*Final_uptakes
    Avg_NEM_bonus<- sum(Data_matrix[(Data_matrix[,14]>0) & (Data_matrix[,27]==3),14])/CCE*12*(BonusOwn)/(BonusOwn+Elec1)
    Revenue_lost <- sum(Degrade_matrix[,2])*Final_uptakes_caused*(Average_Gen*(Elec1*Elec1_perc/100+Elec2*Elec2_perc/100+Elec3*Elec3_perc/100)*rev_perc/100+Avg_NEM_bonus)
    Revenue_change <- Collected_revenue-Loan_provided-ProgramCost-Revenue_lost
    GHG_reduction <-Lifetime_Emission_Reduction[1,1]
    Budget_remained <- Budget-Loan_provided
    GHG_cost <- -Revenue_change/GHG_reduction
    GHG_reduction_value <- GHG_reduction*Carbon_p
    
    
    FinalTable <- matrix(c(Final_uptakes_caused,Final_uptakes,Loan_provided, Budget_remained ,Revenue_lost, Collected_revenue, Revenue_change, GHG_reduction,Biogenic_emission, GHG_cost,GHG_reduction_value,Total_health_benefit), nrow=12, ncol=1)
    rownames(FinalTable)<-c("Solar Installations Caused by Program", "Total Solar Installations","Total value of loans provided ($)","Program budget remaining ($)","Electricity sales revenue lost due to program ($)","Revenue from collected principal and interest ($)","Total revenue change due to program ($)", "Anthropogenic GHG emission reduction (tons)","Biogenic GHG emission reduction (tons)","Cost of GHG emission reduction ($/ton CO2e)","Societal value of GHG emission reductions ($)","Health benefits ($)")
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
    colnames(Final) <-c("Remaining_Installations_Using_Program","Installations_Caused_by_Program")
    DF <- data.frame(Final)
    DF$Type <- ""
    DF1 <- melt(DF, id.var="Type")
    library(ggplot2)
    ggplot(DF1, aes(x = Type, y = value, fill = variable)) + 
      geom_bar(stat = "identity", width = 0.5)+
      ylab("Solar PV Installations (#)")+
      xlab("")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), legend.title=element_blank(),text=element_text(size=15))
    
    
    
    
    
  })
  
  output$table2 <- renderTable({ 
    TCM <- TCM()
    Cost_Benfit <- as.data.frame(TCM[3:12,1])
  },rownames = TRUE, colnames=FALSE, digits=0)
  
  
  output$table3 <- renderTable({ 
    P_value <- Calibrate()
    P_value_final <- P_value[1:3,2:3]
    rownames(P_value_final) <-c("Observed baseline market share","Predicted baseline market share", "Difference between observed and predicted market shares")
    colnames(P_value_final) <-c("CO","TPO")
    P_final <- P_value_final
    
  },rownames = TRUE, colnames=TRUE, digits=4)
  

  
  
}

shinyApp(ui, server) 