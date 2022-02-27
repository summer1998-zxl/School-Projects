#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application 
# library(DT)
# source("global.R")
library(shinydashboard)
# library(shinythemes)

dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "Helping NYC Youth During COVID"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home")),
    menuItem("NYC Interactive Map", tabName = "NYCMap", icon = icon("fas fa-globe-americas")),
    # menuItem("Crime Map", tabName = "CrimeMap", icon = icon("fas fa-globe-americas")),
    menuItem(
      "Statistical Analysis",
      tabName = "StatisticalAnalysis",
      icon = icon("fas fa-chart-area"),
      menuSubItem("Crime", tabName = "Crime"),
      menuSubItem(
        "Housing",
        tabName = "Housing"
      )
    ),
    menuItem("About", tabName = "About", icon = icon("fas fa-asterisk"))
  )),
  
  dashboardBody(
    
    tabItems(
      #     
      #     #----------------------------Home Page------------------------------------
      #     
      tabItem(tabName = "Home",
              fluidPage(
                
                # header image
                img(src = 'homepage.jpg', width = '100%', height = '30%'),
                
                # dashboard for current stats
                fluidRow(
                  
                  
                  # the introduction section
                  column(8,
                         box(width = '100%',
                             h1("Find your place in NYC", align = "center"),
                             tags$div(tags$ul("The COVID-19 pandemic has had a drastic impact on all of us whether 
                                                  it’s losing a job, getting behind on rent or mortgage payments, or 
                                                  struggling to bring food to the table. It has been even harder for 
                                                  low income families to have access to the resources they need to stay 
                                                  safe and healthy. In an effort to help low income families, specifically 
                                                  the youth, as they might find it challenging to navigate these unprecedented 
                                                  times, we created this app. We’re hopeful that our aggregated resources to 
                                                  covid and flu vaccination sites, food, shelter, after school centers, 
                                                  job/internship opportunities, and crime rates will play a small but 
                                                  crucial role in supporting the youth of New York City.",
                                              br(),br(),
                                              span(strong("Check out our amazing functions!")),
                                              br(),br(),
                                              span(strong("NYC Interactive Map:")),
                                              tags$li("Locations for Covid vaccination, flu shots, wifi, food centers, drop in centers, youth shelters, job/internship centers"),
                                              tags$li("Heatmap distribution of covid cases and crime"),
                                              br(),
                                              span(strong("Statistical Analysis:")),
                                              tags$li("Visualization of relationships between crime status and Covid and housing status and Covid"),
                                              tags$li("Plausible explanations and conclusions for trends observed"),
                                              br(),
                                              span(strong("About:")),
                                              tags$li("Links to data sources, data disclaimer, app contributors")
                             )),
                             
                             
                             
                             # h2(id ="smalltitle", "You have come to the right place!!!", align = "center"),
                             tags$style(HTML("#smalltitle{color:green; font-style: bold;}"))
                             
                             
                             
                         )),
                  
                  
                  
                  column(4,
                         fluidRow(
                           h2("NYC Youth Status", align = "center")),
                         br(),
                         fluidRow(infoBoxOutput("covidtotal", width = 14)),
                         fluidRow(infoBoxOutput("student_temphouse", width = 14)),
                         fluidRow(infoBoxOutput("crime_no", width = 14))
                  ),
                ))),
      #     
      #     #------------------------------NYC Map------------------------------------
      tabItem(tabName = "NYCMap", 
              fluidPage(
                actionButton("covid_vaccination","Covid Vaccination",icon=icon("syringe",  lib = "font-awesome")),
                actionButton("flu_vaccination","Flu Shot",icon=icon("syringe",  lib = "font-awesome")),
                actionButton("wifi","Wifi Spot",icon=icon("wifi",  lib = "font-awesome")),
                actionButton("food","Food Centers",icon=icon("utensils",  lib = "font-awesome")),
                actionButton("drop_in","Drop In Centers",icon=icon("building",  lib = "font-awesome")),
                actionButton("youth_drop_in","Youth Shelters",icon=icon("bed",  lib = "font-awesome")),
                actionButton("job","Job & Internship Centers",icon=icon("briefcase",  lib = "font-awesome"))
              ),
              
              
              selectInput("choice",
                          label = "case type: ",
                          choices = c("7 days positive case count","cumulative cases","cumulative death", "crime"), 
                          selected = "7 days positive case count"),
              
              leafletOutput("map", width="100%", height=600)
      ), 
      
      #     
      
      #     #----------------------------Statistical Analysis------------------------------------
      #     
      tabItem(tabName = "Crime",
              fluidPage(
                # tab title
                fluidRow(width = 60,
                         h1("Crime status during COVID pandemic")),
                wellPanel(style = "overflow-y:scroll; height: 850px; max-height: 750px;  background-color: #ffffff;",
                          tabsetPanel(
                            type = "tabs",
                            tabPanel(
                              "Visualization",
                              
                              fluidRow(
                                width = 15,
                                h2(
                                  "COVID cases v.s. Overall crime cases:"
                                )
                              ),
                              
                              # time series chart on average 7 day cases and crimes
                              fluidRow(plotlyOutput("pcr1")),
                              
                              fluidRow(plotlyOutput("pcr2")),
                              
                              fluidRow(
                                width = 15,
                                h5(
                                  "*The average 7 days COVID cases larger than 6000 are manually set as 6000 for better plotting."
                                )
                              ),
                              
                              fluidRow(
                                width = 15,
                                h2(
                                  "COVID cases v.s. Special crime cases:"
                                )
                              ),
                              
                              fluidRow(
                                width = 30,
                                selectInput(
                                  "CrimeType",
                                  label = "Select the crime type:",
                                  choices = c(
                                    'Burglary',
                                    'Felony Assault',
                                    'Grand Larceny',
                                    'Robbery'
                                  ),
                                  multiple = F,
                                  selected = "Burglary"
                                )
                              ),
                              
                              fluidRow(plotlyOutput("pcr3")),
                              
                              fluidRow(
                                width = 15,
                                h5(
                                  "*The Monthly COVID cases of December, 2021 is manually set as 200k for better plotting, The actual cases is 503,737."
                                )
                              ),
                              
                              fluidRow(
                                width = 30,
                                h3(
                                  "We may see the 7-days average youth crime cases or all crime cases have the same trend, which would be like:"
                                ),
                                HTML(
                                  "<h4>1) At first the 7 Day average crimes seems to decrease, and it has a sudden drop for three times when COVID reached its peak and dropped simultaneously. We may say the burst of COVID caused a decrease for crime in NYC, and you can check related news <a href='https://www1.nyc.gov/site/nypd/news/p0106a/overall-crime-new-york-city-reaches-record-low-2020' target='_blank'>here</a>. However, one thing that we can't ignore is some upticks in burglaries, felony assaults and grand larcenies in 2020, which also continues in 2021.<h4>"
                                ),
                                h4(
                                  "2) COVID remains stable in 2020 after the peak, but the crime condition went to another peak because of the burst of burglaries, and then quickly dropped."
                                ),
                                h4("We may come up with the conclusion that though COVID may cause crime drops so that police departments may have a lighter pressure for the crime, the thing they still need to pay attention to is the robbery, larceny, assault and burglary."
                                )
                              )
                            ),
                            tabPanel(
                              "Detail",
                              fluidRow(width =
                                         30,
                                       h3("Pie charts of crime details in specific areas")),
                              # Barplot of crimes by Specific groups, top
                              fluidRow(
                                width = 30,
                                selectInput(
                                  "Borough",
                                  label = "Select the Borough:",
                                  choices = c(
                                    'Bronx',
                                    'Brooklyn',
                                    'Manhattan',
                                    'Queens',
                                    'Staten Island',
                                    'All'
                                  ),
                                  multiple = F,
                                  selected = "All"
                                )
                              ),
                              
                              fluidRow(plotlyOutput("pcr4")),
                              
                              fluidRow(width =
                                         30,
                                       h3("Dataset")),
                              dataTableOutput ('crime_data')
                            )
                          ))
                
              )),
      
      tabItem(tabName = "Housing",
              fluidPage(
                # tab title
                fluidRow(width = 60,
                         h1(
                           "Housing status during COVID pandemic"
                         )),
                wellPanel(style = "overflow-y:scroll; height: 850px; max-height: 750px;  background-color: #ffffff;",
                          tabsetPanel(
                            type = "tabs",
                            tabPanel(
                              "Visualization",
                              # time series charts on average 7 day cases, cumulative low income property units and children in shelters
                              fluidRow(plotlyOutput("pth2")),
                              
                              fluidRow(plotlyOutput("pth1")),
                              
                              fluidRow(plotlyOutput("pth3")),
                              
                              fluidRow(
                                width = 15,
                                h5(
                                  "*Covid data and Population in shelters data are selected from 01/03/2019 to 12/01/2021. The low income property units data ends in 06/30/2021, which accounts for the horizontal line after that day."
                                )
                              ),
                              
                              fluidRow(
                                width = 30,
                                h3(
                                  "We may see the dramatical drop of population in shelter in pandemic period, and here are some possible reasons:"
                                ),
                                h4(
                                  "1) The total amount of low-income units, which are affordable housing resources offered by NYC government for the low-income population, was continuously increasing in this period. And we can see a rapid increase when COVID began and after COVID reached the second peak. The aim of this housing program is to help the homeless move out of shelters into permanent housing."
                                ),
                                h4(
                                  "2) Though it's not shown on the plot, some policies, such as COVID-19 Eviction Protections for Tenants, which protects New Yorkers from eviction for failing to pay their rent if they suffer a financial hardship due to COVID-19, may cause the reduction of population in shelter. In addition, the fears of getting infected by COVID in shelters is another possible reason for the homeless not to apply for shelter beds."
                                ),
                                HTML(
                                  " <p> If you are interested, please check: <a href='https://bkreader.com/2021/09/24/nyc-shelter-population-declined-during-pandemic-but-problems-persist-for-homeless/' target='_blank'>NYC Shelter Population Declined During Pandemic, But Problems Persist For Homeless</a> and <a href='https://www.coalitionforthehomeless.org/press/state-of-the-homeless-2019/#:~:text=The%20report%20finds%20that%20policy,York%20City%20shelters%20each%20night.' target='_blank'>State of the Homeless 2019</a>.</p>",  
                                ),
                                # h4(
                                #   "3) During the COVID pandemic, government continued offering property units, and we can see when COVID began the second peak, the speed of constructing properties also increased."
                                # ),
                                HTML(
                                  "<h4>Generally, we can conclude that the aid government give in this COVID pandemic for the homeless including homeless youth are quite effective and influential, but it's still not the perfect resolution in the long run. A small effort from everyone in this city will be a huge contribution to them. You can click <a href='https://thebridgefund.org/2016/06/05/eviction-hits-children-most/?gclid=CjwKCAiA6seQBhAfEiwAvPqu15-RSRC-GF-6zSNlrCNlvcn5-HUAMV0TeU3JNVcA5o-r_XQ-oOz_NxoCDG4QAvD_BwE' target='_blank'>here</a> for more information to help.<h4>"
                                )
                              )
                            ),
                            tabPanel(
                              "Detail",
                              fluidRow(
                                width = 30,
                                h3("Detailed data about schools with temporary housing students:")
                              ),
                              
                              # Barplot of crimes by Specific groups, top
                              fluidRow(
                                width = 30,
                                selectInput(
                                  "Year",
                                  label = "Select the year:",
                                  choices = c(2019, 2020, 2021),
                                  multiple = F,
                                  selected = 2021
                                )
                              ),
                              fluidRow(textOutput('covidcase')),
                              
                              fluidRow(plotlyOutput("pth4")),
                              
                              # fluidRow(
                              #   width = 30,
                              #   h3("We may find when COVID lasts, the population of students in temporary housing also seem to decrease.")
                              # ),
                              
                              fluidRow(width =
                                         30,
                                       h3("Dataset")),
                              dataTableOutput('school_data')
                            )
                          ))
                
              )),
      #     
      #     
      tabItem(tabName='About',
              HTML(
                "<h2> Data Sources : </h2>
                                  <h4><li><a href='https://github.com/nychealth/coronavirus-data' target='_blank'>NYC Covid Data</a></li></h4> 
                                  <h4><li><a href='https://www1.nyc.gov/site/doh/covid/covid-19-data-vaccines.page' target='_blank'>NYC Covid Vaccination Data</a></li></h4>
                                  <h4><li><a href='https://data.cityofnewyork.us/Health/New-York-City-Locations-Providing-Seasonal-Flu-Vac/w9ei-idxz' target='_blank'>NYC Flu Shot Data</a></li></h4>
                                  <h4><li><a href='https://data.cityofnewyork.us/City-Government/LinkNYC-Kiosk-Status/n6c5-95xh' target='_blank'>NYC Wifi Spots Data</a></li></h4>
                                  <h4><li><a href='https://www1.nyc.gov/site/hra/locations/snap-locations.page' target='_blank'>NYC Food Centers Data</a></li></h4>
                                  <h4><li><a href='https://data.cityofnewyork.us/Social-Services/Directory-Of-Homeless-Drop-In-Centers/bmxf-3rd4' target='_blank'>NYC Drop in Centers Data</a></li></h4>
                                  <h4><li><a href='https://data.cityofnewyork.us/Social-Services/DYCD-RHY-Runaway-and-Homeless-Youth-Services/h682-ywyg' target='_blank'>NYC Youth Shelters Data</a></li></h4>
                                  <h4><li><a href='https://data.cityofnewyork.us/Education/DYCD-after-school-programs-Jobs-and-Internships/99br-frp6' target='_blank'>NYC Jobs & Internships Data</a></li></h4>
                                  <h4><li><a href='https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc' target='_blank'>NYPD Arrest Data</a></li></h4>"
              ),
              titlePanel("Data Disclaimer : "),
              HTML(
                " <p> The linked data is subject to change as it is updated by its sources. </p>",  
              ),
              titlePanel("Credits : "),
              HTML(
                " <p>Our app was built using RShiny.  </p>",
                "<p>The following R packages were used to build this RShiny application:</p>
                                    <p>
                                    <code>shiny</code><code>dplyr</code><code>tidyverse</code>
                                    <code>DT</code><code>ggplot2</code><code>lubridate</code><code>stringr</code>
                                    <code>plotly</code><code>hrbrthemes</code><code>hrbrthemes</code>
                                    <code>highcharter</code><code>RColorBrewer</code><code>shinydashboard</code><code>shinyWidgets</code>
                                    <code>geogsonio</code><code>leaflet</code><code>fontawesome</code>
                                    </p>
                                    <p>This was a project for the Applied Data Science Class at Columbia University.</p>"),
              titlePanel("Contributors : "),
              HTML(
                " <p>Meng, Sharon       |email: zm2380@columbia.edu </p>",
                " <p>Sablani, Rhea      |email: rss2229@columbia.edu </p>",
                " <p>Zha, Yvonne        |email: lz2806@columbia.edu </p>",
                " <p>Zhang, Xile        |email: xz2985@columbia.edu </p>",
                " <p>The inspiration and template for this page can be credited to Fall 2020 Project2 Group1. </p>"
              )
      )
    )))