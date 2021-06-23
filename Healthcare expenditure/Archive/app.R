#-----------------------------------
# 
#-----------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(rgdal)
library(stargazer)
# install.packages('rsconnect')

# setwd("E:/Repositories/HealthcareSpending/Healthcare expenditure")
# NOTE: this setwd() needs to be removed otherwise the deployment of the shinyapp will fail

load('meps.rda')
# NOTE: need to use weighted mean

# This begins the app
############### Server ####################
###########################################
server <- function(input, output) {

  #-----------------------------------------------------------------------
  # Tab 1: graphs-line
  # Enrollment rate and uninsured rate over time by different demographics
  #-----------------------------------------------------------------------
  output$plot1 <- renderPlot({
    
    meps["byvar"] <- meps[input$demo1]#

    mcr <- meps %>%
      group_by(Year, byvar) %>%
      summarise(value = weighted.mean(CoveredByMedicare, FinalWeight,na.rm = T)*100)
    mcr["instype"] <- "Medicare"

    mcd <- meps %>%
      group_by(Year, byvar) %>%
      summarise(value = weighted.mean(CoveredByMedicaid, FinalWeight,na.rm = T)*100)
    mcd["instype"] <- "Medicaid"

    phi <- meps %>%
      group_by(Year, byvar) %>%
      summarise(value = weighted.mean(CoveredByPrivateInsurance, FinalWeight,na.rm = T)*100)
    phi["instype"] <- "Private"

    unins <- meps %>%
      group_by(Year, byvar) %>%
      summarise(value = weighted.mean(NoInsuranceCoverage, FinalWeight,na.rm = T)*100)
    unins["instype"] <- "No Insurance"

    df <- mcr %>% bind_rows(mcd) %>% bind_rows(phi) %>% bind_rows(unins) %>%
      filter(byvar %in% input$demolevels1)
    
    df["instype"] <- factor(df$instype, levels = c("Medicare", "Medicaid", "Private", 
                                                   "No Insurance"))
    
    # define the title based on different 
    ggplot(df) +
      # ggplot() +
      geom_line(aes(x = Year, y = value, group=byvar, color = byvar))+
      scale_x_continuous(breaks=seq(2008,2018,1)) +
      facet_wrap(~instype, scales = "free_y") +
      theme_bw()+
      labs(title= 'Medicare/Medicaid/PHI coverage Rate and Uninsurance Rate', 
           x = "Year", y = "Percent", colour = input$demo1) # 
  })
  output$checkbox1 <- renderUI({
    choice <-  levels(meps[,input$demo1])
    checkboxGroupInput("demolevels1", paste0("Select groups"), 
                       choices = choice, selected = choice)
    
  })

  #-------------------------
  # Second Graph
  #-------------------------  
  output$plot2 <- renderPlot({
    
    meps["group"] <- meps[input$demo2]
    
    oop <- meps %>%
      filter(TotalHealthExp>0) %>%
      group_by(Year, group) %>%
      summarise(OOPShare = weighted.mean(OOPShare, FinalWeight,na.rm = T)*100) 
    oop['service'] <- "Total"
    
    for (service in c("Outpatient", "ER", "Hospitalization", "Dental", "Homehealth",
                      "Rx", "MedEquip")) {
      meps['tot'] <- meps[,paste0(service, "TotalExp")]
      meps['oop'] <- meps[,paste0(service, "OOPShare")]
      
      temp <- meps %>%
        filter(tot>0) %>%
        group_by(Year, group) %>%
        summarise(OOPShare = weighted.mean(oop, FinalWeight,na.rm = T)*100)
      temp['service'] <- service
      oop <- oop %>% bind_rows(temp)
    }
    
    oop["service"] <- factor(oop$service,levels = c("Total","Hospitalization", "Outpatient", 
                                                    "ER", "Dental", "Homehealth", "Rx", 
                                                    "MedEquip"))
    oop <- oop %>%
      filter(group %in% input$demolevels2)
    
    ggplot(oop) +
      geom_line(aes(x = Year, y = OOPShare, group=group, color = group)) +
      facet_wrap(~service, scales = "free_y") +
      scale_x_continuous(breaks=seq(2008,2018,1)) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45)) +
      labs(title= 'OOP Share', x = "Year", colour = input$demo2)
  })
  output$checkbox2 <- renderUI({
    choice <-  levels(meps[,input$demo2])
    checkboxGroupInput("demolevels2", paste0("Select groups"), 
                       choices = choice, selected = choice)
  }) 

  #-------------------------
  # Third Graph
  #-------------------------  
  output$plot3 <- renderPlot({
    
    service <- input$service3
    
    expvarlist <- c("TotalExp", "MedicareExp", "MedicaidExp", 
                    "PHIandTricareExp", "OtherExp", "OOPExp")
    varlist <- lapply(service, paste0, expvarlist)[[1]]
    
    df_prep <- function(exp, grp, pay) {
      
      df <- meps[meps$Year==input$year3 & meps[exp]>0,c('Year', "FinalWeight", exp, grp)]
      df['group'] <- df[, grp]
      df['expenditure'] <- df[,exp]
      
      df <- df %>%
        group_by(Year, group) %>%
        summarise(Population = sum(FinalWeight),
                  Spending = sum(FinalWeight*expenditure)) %>%
        group_by(Year) %>%
        mutate(TotalPop = sum(Population),
               TotalExp = sum(Spending),
               PopShare = Population/TotalPop*100,
               ExpShare = Spending/TotalExp*100) %>%
        select(Year, group, PopShare, ExpShare) %>%
        pivot_longer(cols=c("PopShare", "ExpShare"),  # reshape from wide to long
                     names_to = 'var',
                     values_to = 'value') 
      df$var <- factor(df$var, levels =c("PopShare", "ExpShare"))
      df['PaymentType'] <- pay
      return(df)
    }
    
    tot <- df_prep(varlist[1], input$demo3, "Total")
    mcr <- df_prep(varlist[2], input$demo3, "Medicare")
    mcd <- df_prep(varlist[3], input$demo3, "Medicaid")
    phi <- df_prep(varlist[4], input$demo3, "PHI/Tricare")
    oth <- df_prep(varlist[5], input$demo3, "Other insurance")
    oop <- df_prep(varlist[6], input$demo3, "OOP")
    
    df <- tot %>% bind_rows(mcr) %>% bind_rows(mcd) %>% bind_rows(phi) %>%
      bind_rows(oth) %>% bind_rows(oop)
    
    df["PaymentType"] <- factor(df$PaymentType, levels = c("Total", "Medicare", "Medicaid", "PHI/Tricare", "Other insurance", "OOP")) 
    df <- df %>%
      filter(group %in% input$demolevels3)
    
    # plot
    df %>%
      ggplot(aes(x = var, y = value, fill = group)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(labels=c("PopShare" = "Population", 
                                "ExpShare" = "Expenditure")) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 20)) +
    geom_text(aes(label = paste0(round(value,1), '%')), size = 3, position = position_stack(vjust = 0.5)) +
      facet_grid(~PaymentType) +
      labs(title= "Share of population and corresponding spending", y="Share (%)", x = "") 
    
  })
  output$checkbox3 <- renderUI({
    choice <-  levels(meps[,input$demo3])
    checkboxGroupInput("demolevels3", paste0("Select groups"), 
                       choices = choice, selected = choice)
  })
  
  output$text <- renderText({
    paste0("<f><font size = 4>",
           "<strong>Notes: </strong>",
           "Under each payment source, the population universe includes only people who have positive payment paid by that source"
           )
  })
      
}

############### UI ####################
#######################################
ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  # This applies preset aesthetic decisions to the entire app
  navbarPage(
    "Healthcare Facts App",
    # navbarPage lets us set up a navigation bar to toggle between tabPanels
    
    #-------------------------
    # Welcome Tab
    #-------------------------  
    tabPanel("Welcome", # tabPanel() creates the tabs
             tags$head(
               tags$style("h2 {color: #04B4AE; }
               h1 {color: #04B4AE}; }")), # this is setting the color palette for our tab headers 1 and 2
             headerPanel("About the App"), # I haven't created a title for this page, try adding one!
             h4("This App visualizes health insurance coverage and the health expenditures patterns. With the visualization,
                we can compare health insurance coverage and cost by different groups of people. We can look into more details of 
                the cost paid by different payment sources and for different service types."),
             # br(), #a break creates a space
             h2("How to Use This App"), # the number following h corresponds to size
             h4(tags$ul(
               tags$li("Health Insurance: Display the enrollment rate of different insurance and the uninsured rate by different characteristics"), #bullet point
               tags$li("Out-of-pocket Share: Present the out-of-pocket share for different health services by different characteristics"), #bullet point
               tags$li("Health Expenditure Distribution: Visualize the distribution by different characteristics of population as well as the corresponding spending for certain health service under different payment sources") #bullet point
             )),
             h4("To begin, select \"Health Insurance\" on the navigation bar. You will be asked to choose 
                the demographic or social characteristics and the corresponding groups that you are interested in, the plots will display the enrollment rate of 
                Medicare, Medicaid and Private Health Insurance, as well as the uninsurance rate over time."),
             h4("The second tab \"Out-of-pocket Share\" on the navigation bar will again allow you to choose the demographic or social characteristics
             and the corresponding groups, once you set your selection, the main panel will 
             present the out-of-pocket (OOP) share paid for different health services by the groups of people that you are interested in."),
             h4("In the third tab \"Health Expenditure Distribution\", other than selection of the characteristics and the corresponding groups, users can also
             select the year and health service type. The plots display the distribution of population by different characteristic groups, and the share of health spending 
                associated with each group. The parallel distributions are plotted for different payments sources simultaneously. Note that, for each payment source, the population 
                universe is the people who have positive payment made by that source."),
             h2("The Data"),
             h4("This app uses MEPS data. The data includes all different person-level characteristics, such as
                age, gender, race, marital status, education, employment status, poverty status and region. The data also includes genderal health insurance coverage information
                as well as the expenditures of different health services that paid by different payment sources. Health services include: Hospital inpatient, 
                Outpatient (including office-based medical providers), Emergency Room (ER), Dental, Prescription Drugs (Rx), Homehealth (either Agency or non-Agency), 
                Vision and other medical equipment. Payment source include Medicare, Medicaid, Private Health Insurance (including Tricare), Other Insurance, and Out-of-pocket (OOP)")),
    
    #-------------------------
    # Insurance Tab
    #-------------------------  
    tabPanel(
      # First tab
      "Health Insurance",
      headerPanel("Plotting trend of insurance coverage"),
      # Side bar layout
      sidebarLayout(position = "left",
        # Side panel
        sidebarPanel(width = 3,
                     selectInput("demo1",
                                 "Characteristics:",
                                 c('Age Group' = 'AgeGroup', 
                                   'Gender' = 'Gender', 
                                   'Race' = 'Race',
                                   'Marital Status' = 'MaritalStatus', 
                                   'Education' = 'Education', 
                                   'Employment Status' = 'EmploymentStatus',
                                   'Poverty Status' = 'PovertyStatus', 
                                   'Region' = 'RegionMEPS'), 
                                 selected = ('Age Group' = 'AgeGroup')),
                     uiOutput("checkbox1")
                     # helpText("")
                     ),
        # Main panel
          mainPanel(
            # Create the plot.
            plotOutput("plot1")
      ))
    ), # 1st Graph Tab
    
    #-------------------------
    # OOP Share Tab
    #-------------------------  
    tabPanel(
      # Second tab
      "Out-of-pocket Share",
      headerPanel("Plotting trend of OOP share"),
      # Side bar layout
      sidebarLayout(position = "left",
                    # Side panel
                    sidebarPanel(width = 3,
                                 selectInput("demo2",
                                             "Characteristics:",
                                             c('Age Group' = 'AgeGroup', 
                                               'Gender' = 'Gender', 
                                               'Race' = 'Race',
                                               'Marital Status' = 'MaritalStatus', 
                                               'Education' = 'Education', 
                                               'Employment Status' = 'EmploymentStatus',
                                               'Poverty Status' = 'PovertyStatus',
                                               'Insurance Type' = 'CoverageType',
                                               'Region' = 'RegionMEPS'), 
                                             selected = ('Age Group' = 'AgeGroup')),
                                 uiOutput("checkbox2")
                                 # helpText("Red dots are Elementary school, Blue dots are Middle school or above")
                    ),
                    # Main panel
                    mainPanel(
                      # Create the plot.
                      plotOutput("plot2")
                    ))
    ), # 2nd Graph Tab
    #-------------------------
    # OOP Share Tab
    #-------------------------  
    tabPanel(
      # Second tab
      "Healthcare Expenditure Distribution",
      headerPanel("Plotting healthcare expenditure distribution"),
      # Side bar layout
      sidebarLayout(position = "left",
                    # Side panel
                    sidebarPanel(width = 3,
                                 selectInput("year3",
                                             "Year",
                                             c(seq(2008, 2018, 1)), 
                                             selected = (2018)),
                                 selectInput("service3",
                                             "Health service:",
                                             c('Total' = 'Total',
                                               'Hospitalization' = 'Hospitalization', 
                                               'Outpatient' = 'Outpatient', 
                                               'ER' = 'ER',
                                               'Dental' = 'Dental', 
                                               'Home health' = 'Homehealth',
                                               'Prescription drugs' = 'Rx',
                                               'Vision & Medical equipment' = 'MedEquip'), 
                                             selected = ('Total' = 'Total')),
                                 selectInput("demo3",
                                             "Characteristics:",
                                             c('Age Group' = 'AgeGroup', 
                                               'Gender' = 'Gender', 
                                               'Race' = 'Race',
                                               'Marital Status' = 'MaritalStatus', 
                                               'Education' = 'Education', 
                                               'Employment Status' = 'EmploymentStatus',
                                               'Poverty Status' = 'PovertyStatus',
                                               'Insurance Type' = 'CoverageType',
                                               'Region' = 'RegionMEPS'), 
                                             selected = ('Age Group' = 'AgeGroup')),
                                 uiOutput("checkbox3")
                                 # helpText("Red dots are Elementary school, Blue dots are Middle school or above")
                    ),
                    # Main panel
                    mainPanel(
                      # Create the plot.
                      plotOutput("plot3"),
                      htmlOutput('text')
                    ))
    )# 3rd Graph Tab
  )
))
shinyApp(ui = ui, server = server)
