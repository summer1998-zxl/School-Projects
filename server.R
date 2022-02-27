if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("dplyr")) { install.packages("dplyr")}
library(dplyr)
if (!require("tidyverse")) { install.packages("tidyverse")}
library(tidyverse)
if (!require("DT")) { install.packages("DT")}
library(DT)
if (!require("ggplot2")) { install.packages("ggplot2")}
library(ggplot2)
if (!require("lubridate")) { install.packages("lubridate")}
library(lubridate)
if (!require("plotly")) { install.packages("plotly")}
library(plotly)
if (!require("hrbrthemes")) { install.packages("hrbrthemes")}
library(hrbrthemes)
if (!require("highcharter")) { install.packages("highcharter")}
library(highcharter)
if (!require("RColorBrewer")) { install.packages("RColorBrewer")}
library(RColorBrewer)
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
if (!require("geojsonio")) { install.packages("geojsonio")}
library(geojsonio)
if (!require("readr")) { install.packages("readr")}
library(readr)
if (!require("leaflet")) { install.packages("leaflet")}
library(leaflet)
if (!require("fontawesome")) { install.packages("fontawesome")}
library(fontawesome)

source("global.R")
library(shinydashboard)



shinyServer(function(input, output, session) {
  
  ## homepage box output #############################################


  output$covidtotal <- renderInfoBox({
    infoBox(
      title = tags$p("2021 Covid Total Cases", style = "font-size: 100%;"),
      value = tags$p(prettyNum(1054514, big.mark = ","), style = "font-size: 200%;"),
      icon = icon("fas fa-chart-area"),
      fill = T,
      color = "light-blue")
  })
  
  output$student_temphouse <- renderInfoBox({
    infoBox(
      title = tags$div(
        tags$p("2021 Total number of students", style = "font-size: 100%;"),
        tags$p("in temporary housing", style = "font-size: 100%;")
      ),
      value = tags$p(prettyNum(86915, big.mark = ","), style = "font-size: 200%;"),
      icon = icon("fas fa-bed"),
      fill = T,
      color = "blue")
  })
  
  output$crime_no <- renderInfoBox({
    infoBox(
      title = tags$p("2021 Total crime number of youth", style = "font-size: 100%;"),
      value = tags$p(prettyNum(33727, big.mark = ","), style = "font-size: 200%;"),
      icon = icon("fas fa-exclamation"),
      fill = T,
      color = "aqua")
  })
  
  ## map output #############################################
  output$map <- renderLeaflet({
    #covid cases parameters
    parameter <- if(input$choice == "7 day positive case count") {
      data$people_positive
    } else if(input$choice == "cumulative cases") {
      data2$COVID_CASE_COUNT
    } else if(input$choice == "cumulative deaths"){
      data2$COVID_DEATH_COUNT
    } else{
      crime_count$CRIME_COUNT_2021
    }
    
    #create palette  
    pal <- colorNumeric(
      palette = "Blues",
      domain = parameter)
    
    #create labels
    #Cleaned by emphasizing %Pos, case rate is other option
    labels1 <- paste(
      data$zip, " - ",
      data$modzcta_name, "<br/>",
      "Positive cases in last 7 days: ", data$people_positive,"<br/>",
      "Cumulative cases: ", data2$COVID_CASE_COUNT,"<br/>",
      "Cumulative deaths: ", data2$COVID_DEATH_COUNT,"<br/>",
      "Tested number:",data$people_tested,"<br/>",
      "<b>Infection Rate: ", perp_zipcode[nrow(perp_zipcode),],"%</b><br/>") %>%
      lapply(htmltools::HTML)
    
    labels2 <- paste0(
      str_trim(shape$precinct), "-th", " Precinct",  "<br/>",
      "Phone Number:", crime_count$Phone, "<br/>", 
      crime_count$CRIME_COUNT_2021, " crime cases ", "(","Crime rate:",  crime_count$CRIME_RATE, ")", " throughout 2021:","<br/>", 
      "- Murder: ",crime_count$MURDER, "<br/>", 
      "- Robbery: ",crime_count$ROBBERY,"<br/>", 
      "- Rape: ",crime_count$RAPE,"<br/>", 
      "- Sex crimes: ",crime_count$SEX_CRIMES,"<br/>", 
      "- Dangerous weapons: ",crime_count$DANGEROUS_WEAPONS,"<br/>", 
      "- Dangerous Drugs: ",crime_count$DANGEROUS_DRUGS,"<br/>"
      ) %>%
      lapply(htmltools::HTML)
  
    if (input$choice == "crime"){
      map <- shape %>%
        select(geometry) %>%
        leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
        setView(-73.93, 40.70, zoom = 10) %>%
        addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(parameter),
          weight = 1,
          opacity = .5,
          color = "white",
          dashArray = "2",
          fillOpacity = 0.7,
          highlight = highlightOptions(weight = 1,
                                       color = "yellow",
                                       dashArray = "",
                                       fillOpacity = 0.7,
                                       bringToFront = TRUE),
          label = labels2) %>%
        addLegend(pal = pal,
                  values = ~parameter,
                  opacity = 0.7,
                  title = htmltools::HTML(input$radio),
                  position = "bottomright")
    }
    else{
        map <- geo_data %>%
          select(geometry) %>%
          leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
          setView(-73.93, 40.70, zoom = 10) %>%
          addTiles() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor = ~pal(parameter),
            weight = 1,
            opacity = .5,
            color = "white",
            dashArray = "2",
            fillOpacity = 0.7,
            highlight = highlightOptions(weight = 1,
                                         color = "yellow",
                                         dashArray = "",
                                         fillOpacity = 0.7,
                                         bringToFront = TRUE),
            label = labels1) %>%
          addLegend(pal = pal,
                    values = ~parameter,
                    opacity = 0.7,
                    title = htmltools::HTML(input$radio),
                    position = "bottomright")
      }
    
    

    
    
    
    
  })
  

  #covid vaccination button
  observeEvent(input$covid_vaccination, {
    proxy <- leafletProxy("map", data = covid_vaccination)
    proxy %>% clearControls()
    
    leafletProxy("map", data = covid_vaccination) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        clusterOptions = markerClusterOptions(),
                        label = lapply(
                          lapply(seq(nrow(covid_vaccination)), function(i){
                            paste0('Address: ',covid_vaccination[i, "Location"], '<br/>',
                                   'Zipcode: ',covid_vaccination[i, "Zip_code"], '<br/>',
                                   'Type: ',covid_vaccination[i, "Type"],'<br/>',
                                   'Vaccine offered: ',covid_vaccination[i, "Vaccine_offered"]) }), htmltools::HTML), 
                        icon = awesomeIcons(markerColor= "lightred",
                                            text = fa("syringe")))
  })     
  
  #Flu vaccination Button
  observeEvent(input$flu_vaccination, {
    proxy <- leafletProxy("map", data = flu_vaccination)
    proxy %>% clearControls()
    
    leafletProxy("map", data = flu_vaccination) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        clusterOptions = markerClusterOptions(),
                        label = lapply(
                          lapply(seq(nrow(flu_vaccination)), function(i){
                            paste0('Address: ',flu_vaccination[i, "Address"], '<br/>',
                                   'Zipcode: ',flu_vaccination[i, "ZIP.Code"], '<br/>',
                                   'Vaccine For Children: ',flu_vaccination[i, "Children"],'<br/>',
                                   'Walk-in: ',flu_vaccination[i, "Walk.in"]) }), htmltools::HTML), 
                        icon = awesomeIcons(markerColor= "darkblue",
                                            text = fa("syringe")))
  })
  
  
  #Wifi Spot Button
  observeEvent(input$wifi, {
    proxy <- leafletProxy("map", data = wifi)
    proxy %>% clearControls()
    
    leafletProxy("map", data = wifi) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(~LONGITUDE, ~LATITUDE, 
                        clusterOptions = markerClusterOptions(),
                        label = lapply(
                          lapply(seq(nrow(wifi)), function(i){
                            paste0('Address: ',wifi[i, "ADDRESS"], '<br/>',
                                   'Wifi Status: ',wifi[i, "WIFI.STATUS"], '<br/>',
                                   'Tablet Status: ',wifi[i, "TABLET.STATUS"],'<br/>',
                                   'Phone Status: ',wifi[i, "PHONE.STATUS"]) }), htmltools::HTML), 
                        icon = awesomeIcons(markerColor= "green",
                                            text = fa("wifi")))
  })
  
  
  #Food Center Button
  observeEvent(input$food, {
    proxy <- leafletProxy("map", data = food)
    proxy %>% clearControls()
    
    leafletProxy("map", data = food) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        clusterOptions = markerClusterOptions(),
                        label = lapply(
                          lapply(seq(nrow(food)), function(i){
                            paste0('Address: ',food[i, "Address"], '<br/>',
                                   'Center Name: ',food[i, "Name"], '<br/>',
                                   'Contact Number: ',food[i, "Contact"],'<br/>'
                                   ) }), htmltools::HTML), 
                        icon = awesomeIcons(markerColor= "black",
                                            text = fa("utensils")))
                      
  })
  
  
  #Drop In Center Button
  observeEvent(input$drop_in, {
    proxy <- leafletProxy("map", data = drop_in)
    proxy %>% clearControls()
    
    leafletProxy("map", data = drop_in) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        clusterOptions = markerClusterOptions(),
                        label = lapply(
                          lapply(seq(nrow(drop_in)), function(i){
                            paste0('Address: ',drop_in[i, "Address"], '<br/>',
                                   'Zip Code: ',drop_in[i, "Postcode"], '<br/>',
                                   'Center Name: ',drop_in[i, "Center.Name"],'<br/>'
                            ) }), htmltools::HTML), 
                        icon = awesomeIcons(markerColor= "orange",
                                            text = fa("building")))
  })
  
  #Job Center Button
  observeEvent(input$job, {
    proxy <- leafletProxy("map", data = job)
    proxy %>% clearControls()
    
    
    leafletProxy("map", data = job) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        clusterOptions = markerClusterOptions(),
                        label = lapply(
                          lapply(seq(nrow(job)), function(i){
                            paste0('Address: ',job[i, "Address"], '<br/>',
                                   'AGENCY: ',job[i, "AGENCY"], '<br/>',
                                   'Program: ',job[i, "PROGRAM"],'<br/>',
                                   'Contact Number: ',job[i, "Contact.Number"], '<br/>'
                            ) }), htmltools::HTML), 
                        icon = awesomeIcons(markerColor= "beige",
                                            text = fa("briefcase")))
  })
  
  #Youth Drop=in Button
  observeEvent(input$youth_drop_in, {
    proxy <- leafletProxy("map", data = youth_drop_in)
    proxy %>% clearControls()

    
    leafletProxy("map", data = youth_drop_in) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        clusterOptions = markerClusterOptions(),
                        label = lapply(
                          lapply(seq(nrow(youth_drop_in)), function(i){
                            paste0('Address: ',youth_drop_in[i, "Number.and.Street.Address"], '<br/>',
                                   'Zip Code: ',drop_in[i, "Postcode"], '<br/>',
                                   'Agency: ',youth_drop_in[i, "AGENCY"],'<br/>',
                                   'Contact Number: ',youth_drop_in[i, "Contact.Number"], '<br/>'
                            ) }), htmltools::HTML), 
                        icon = awesomeIcons(markerColor= "purple",
                                            text = fa("bed")))
  })
  
  
  
  
  
  ######### Tab Statistical Analysis ##########
  
  ##############Crime
  
  #Construct the time series map of avg 7 days cases an crimes
  
  y_2<-list(overlaying='y',side='right',title='Average 7 days Cases')
  pcr1<-plot_ly(data=plotdf1,x=~Date,y=~Youth_7_avg,name='Youth Crime Cases',type='scatter',mode='lines',line=list(color='orange'))
  pcr1<-pcr1%>%add_lines(data=plotdf1,x=~Date,y=~Case_7_avg,name='COVID Cases',yaxis='y2',line=list(color='blue'))
  pcr1<-pcr1%>%layout(
    font=list(size=14,color='grey'),
    title=list(text="Average 7 Days Youth Crime Cases and Covid Cases",font=list(size=24,color='grey')),
    paper_bgcolor='transparent',
    xaxis=list(title='Date',showgrid=T),
    yaxis=list(title='Average 7 days Crimes',showgrid=F),
    yaxis2=y_2,
    margin=list(t=80,b=0,r=80,autoexpand=T),
    legend=list(x=0.05,y=0.1,bordercolor='grey',borderwidth=1))
  
  output$pcr1<-renderPlotly(pcr1)
  
  pcr2<-plot_ly(data=plotdf1,x=~Date,y=~All_7_avg,name='All Crime Cases',type='scatter',mode='lines',line=list(color='orange'))
  pcr2<-pcr2%>%add_lines(data=plotdf1,x=~Date,y=~Case_7_avg,name='COVID Cases',yaxis='y2',line=list(color='blue'))
  pcr2<-pcr2%>%layout(
    font=list(size=14,color='grey'),
    title=list(text="Average 7 Days All Crime Cases and Covid Cases",font=list(size=24,color='grey')),
    paper_bgcolor='transparent',
    xaxis=list(title='Date',showgrid=F),
    yaxis=list(title='Average 7 days Crimes',showgrid=F),
    yaxis2=y_2,
    margin=list(t=80,b=0,r=80,autoexpand=T),
    legend=list(x=0.05,y=0.1,bordercolor='grey',borderwidth=1))
  
  output$pcr2<-renderPlotly(pcr2)
  
  #crimedf:144*3,covdf3:36*2
  #'BURGLARY','FELONY ASSAULT','GRAND LARCENY',"ROBBERY"
  #For plotting, manually set Dec 2021 has 200k covid cases
  covdf3[36,2]<-200000
  crimeInput <- reactive({
    switch(input$CrimeType,
           "Burglary"="BURGLARY",
           "Felony Assault"="FELONY ASSAULT",             
           "Grand Larceny"="GRAND LARCENY",            
           "Robbery"="ROBBERY"
    )
  })
  
  output$pcr3<-renderPlotly({
    y_2<-list(overlaying='y',side='right',title='Monthly Crime cases')
    pcr3<-plot_ly(data=covdf3,x=~Date,y=~Count,name='Monthly COVID Cases',type='scatter',mode='lines',line=list(color='red',dash='dash'))
    pcr3<-pcr3%>%add_lines(data=crimedf%>%filter(Crime==crimeInput()),x=~Date,y=~Count,name=str_to_title(crimeInput()),yaxis='y2',line=list(color='blue',dash='dot'))
    pcr3%>%layout(
      font=list(size=14,color='grey'),
      title=list(text="Crime Cases of Special Category and Covid Cases by Month",font=list(size=24,color='grey')),
      paper_bgcolor='transparent',
      xaxis=list(title='Date',showgrid=F),
      yaxis=list(title='COVID Cases by Month',showgrid=F),
      yaxis2=y_2,
      margin=list(t=80,b=0,r=80,autoexpand=T),
      legend=list(x=0.5,y=1,bordercolor='grey',borderwidth=1))
  })
  
  
  
  
  #Summary the crime data and draw bar plots
  crime_data<-plotdf2%>%select(Date,Crime,Borough,Sex,Race)
  crime_data$Sex[crime_data$Sex=='M']<-'Male'
  crime_data$Sex[crime_data$Sex=='F']<-'Female'
  crime_data$Borough<-factor(crime_data$Borough)
  crime_data$Sex<-factor(crime_data$Sex)
  crime_data$Race<-factor(crime_data$Race)
  
  #Processing Input
  boroughInput <- reactive({
    switch(input$Borough,
           "Bronx"="Bronx",
           "Brooklyn"="Brooklyn",             
           "Manhattan"="Manhattan",            
           "Queens"="Queens",              
           "Staten Island"="Staten Island",
           "All"='All')
  })
  
  output$pcr4<-renderPlotly({
    if (boroughInput()!='All'){
      dt3<-crime_data%>%
        filter(Borough==boroughInput())%>%
        select(-Borough)
    }
    else{
      dt3<-crime_data%>%
        select(-Borough)
    }
    plot_ly()%>%
      add_pie(data=as.data.frame(table(dt3$Crime))%>%
                top_n(n=10),labels=~Var1,values=~Freq,hole=0.5,name='Most Frequent Crime',title='Most Frequent Crime',domain=list(row=0,column=0))%>%
      add_pie(data=as.data.frame(table(dt3$Sex)),labels=~Var1,values=~Freq,hole=0.5,name='Sex',title='Sex',domain=list(row=0,column=1))%>%
      add_pie(data=as.data.frame(table(dt3$Race)),labels=~Var1,values=~Freq,hole=0.5,name='Race',title='Race',domain=list(row=1,column=2))%>%
      layout(
        font=list(size=14,color='grey'),
        title=list(text="Pie Chart of Youth Crimes categorized by Features",font=list(size=24,color='grey')),
        showlegend=F,
        grid=list(rows=1,columns=3),
        paper_bgcolor="transparent",
        margin=list(t=50,b=0,autoexpand=T),
        xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
        yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))
  })
  
  output$crime_data<- renderDataTable({
    datatable(
      data = crime_data,
      selection = 'multiple',
      filter = "top",
      rownames = FALSE,
      options = list(scrollX=TRUE,scrollY=TRUE,pageLength = 10)
    )
  })
  
  
  ##############Temporary Housing
  
  #Create secondary data
  dhs$Date<-as.Date(dhs$Date)
  li$Date<-as.Date(li$Date)
  covid$Date<-as.Date(covid$Date)
  dhs_th<-dhs%>%filter(Date>='2019-01-03',Date<'2021-12-01')%>%select(Date,Total.Children.in.Shelter,Total.Individuals.in.Shelter)%>%arrange(Date)
  dhs_th<-dhs_th[!duplicated(dhs_th),]
  covid_th<-covid%>%filter(Date<'2021-12-01')%>%select(Date,Case_7days_avg)
  temp<-data.frame(matrix(rep(0,844),ncol=2))
  names(temp)<-names(covid_th)
  covid_th<-rbind(temp,covid_th)
  li_th<-li_sum
  covid_th$Date<-seq.Date(from=as.Date('2019-01-03'),to=as.Date('2021-11-30'),by='day')
  covid_th$Date<-as.character(covid_th$Date);dhs_th$Date<-as.character(dhs_th$Date);li_th$Date<-as.character(li_th$Date)
  
  #NA operation in li_th
  thdf1<-covid_th%>%left_join(.,li_th)%>%left_join(.,dhs_th)
  thdf1[apply(is.na(thdf1[,c(3,4)]),1,any),c(3,4)]<-0
  #NA operation in dhs_th:mean
  rows<-which(is.na(thdf1[,5]))
  for (row in rows){
    for (col in 5:6){
      thdf1[row,col]<-as.integer((thdf1[row-1,col]+thdf1[row+1,col])/2)
    }
  }
  thdf1$Per.Children.in.Shelter<-thdf1$Total.Children.in.Shelter/thdf1$Total.Individuals.in.Shelter
  thdf1$Date<-as.Date(thdf1$Date)
  
  #Construct the time series of avg 7 days cases,cumulative low income units in NYC and children in shelter
  
  ##covid vs cumsum  
  y_2<-list(overlaying='y',side='right',title='Average 7 days Covid Cases')
  pth1<-plot_ly(data=thdf1,x=~Date,y=~cumsum(Tot),name='Total Units',type='scatter',mode='lines',line=list(color='dark yellow'))
  pth1<-pth1%>%add_lines(data=thdf1,x=~Date,y=~cumsum(LI),name='Low-Income Units',line=list(color='orange'))
  pth1<-pth1%>%add_lines(data=thdf1,x=~Date,y=~Case_7days_avg,name='COVID Cases',yaxis='y2',line=list(color='blue'))
  pth1<-pth1%>%layout(
    font=list(size=14,color='grey'),
    title=list(text="Cumulative Property Units in NYC and Average 7 Days Covid Cases",font=list(size=24,color='grey')),
    paper_bgcolor='transparent',
    xaxis=list(title='Date',showgrid=T),
    yaxis=list(title='Cumulative Property Units',showgrid=F),
    yaxis2=y_2,
    margin=list(t=80,b=0,r=80,autoexpand=T),
    legend=list(x=0.05,y=0.9,bordercolor='grey',borderwidth=1))
  
  output$pth1<-renderPlotly(pth1)
  
  ##covid vs children
  y_2<-list(overlaying='y',side='right',title='Average 7 days Covid Cases')
  pth2<-plot_ly(data=thdf1,x=~Date,y=~Total.Children.in.Shelter,name='Children',type='scatter',mode='lines',line=list(color='dark yellow'))
  pth2<-pth2%>%add_lines(data=thdf1,x=~Date,y=~Case_7days_avg,name='COVID Cases',yaxis='y2',line=list(color='blue'))
  pth2<-pth2%>%layout(
    font=list(size=14,color='grey'),
    title=list(text="Population of Children in Shelter and Average 7 Days Covid Cases",font=list(size=24,color='grey')),
    paper_bgcolor='transparent',
    xaxis=list(title='Date',showgrid=F),
    yaxis=list(title='Population in Shelter',showgrid=F),
    yaxis2=y_2,
    margin=list(t=80,b=0,r=80,autoexpand=T),
    legend=list(x=0.05,y=0.1,bordercolor='grey',borderwidth=1))
  
  output$pth2<-renderPlotly(pth2)
  
  ##low income properties in NYC vs children in shelter
  y_2<-list(overlaying='y',side='right',title='Cumulative Property Units')
  pth3<-plot_ly(data=thdf1,x=~Date,y=~Total.Children.in.Shelter,name='Children',type='scatter',mode='lines',line=list(color='dark yellow'))
  pth3<-pth3%>%add_lines(data=thdf1,x=~Date,y=~cumsum(LI),name='Low-Income Units',yaxis='y2',line=list(color='blue'))
  pth3<-pth3%>%layout(
    font=list(size=14,color='grey'),
    title=list(text="Population of Children in Shelter and Cumulative Property Units in NYC",font=list(size=24,color='grey')),
    paper_bgcolor='transparent',
    xaxis=list(title='Date',showgrid=F),
    yaxis=list(title='Population of Children in Shelter',showgrid=F),
    yaxis2=y_2,
    margin=list(t=80,b=0,r=80,autoexpand=T),
    legend=list(x=0.05,y=0.5,bordercolor='grey',borderwidth=1))
  
  output$pth3<-renderPlotly(pth3)
  
  
  #create secondary data
  sum_covid<-covid%>%
    filter(Year<=2021)%>%
    group_by(Year)%>%
    summarize(Case_count=sum(Daily_Case))
  sum_covid<-rbind(data.frame(Year=2019,Case_count=0),sum_covid)
  
  school_data<-th[,c(1:4,9)]
  school_data$Year<-factor(school_data$Year)
  
  output$covidcase<-renderText({
    case<-sum_covid[sum_covid$Year==as.numeric(input$Year),2]
    paste('There are altogether ',case,' COVID-19 cases in ',input$Year,'.')
  })
  
  output$pth4<-renderPlotly({
    thdf2<-school_data%>%
      filter(Year==as.numeric(input$Year))%>%
      select(-Year)
    thdf21<-thdf2%>%select(-Percentage)%>%arrange(desc(Temp_Housing))%>%top_n(n=10)
    thdf22<-thdf2%>%select(-Temp_Housing)%>%arrange(desc(Percentage))%>%top_n(n=10)
    
    pth4<-plot_ly(data=thdf22,x=~School,y=~Percentage,type='bar',name='By Percentage')
    pth4<-pth4%>%layout(
      font=list(size=14,color='grey'),
      paper_bgcolor='transparent',
      xaxis=list(title='Date',showgrid=F,tickangle=30),
      yaxis=list(title='Percentage of Students in Temporary Housing',showgrid=F),
      margin=list(t=80,b=0,r=80,autoexpand=T),
      legend=list(text="By Percentage",x=0.55,y=1,bordercolor='grey',borderwidth=1)
    )
    
    pth5<-plot_ly(data=thdf21,x=~School,y=~Temp_Housing,type='bar',name='By Population')
    pth5<-pth5%>%layout(
      font=list(size=14,color='grey'),
      title=list(text="10 Schools with Most Students in Temporary Housing",font=list(size=24,color='grey')),
      paper_bgcolor='transparent',
      xaxis=list(title='School',showgrid=F,tickangle=30),
      yaxis=list(title='Population of Students in Temporary Housing',showgrid=F),
      margin=list(t=80,b=0,r=80,autoexpand=T)
    )
    
    subplot(pth4,pth5)
  })
  
  #For filtering schools
  #school_data$School<-factor(school_data$School) #Don't comment out or the barplot will go strange
  
  ###Output the data of schools 
  output$school_data<- renderDataTable({
    datatable(
      data = school_data,
      selection = 'multiple',
      filter = "top",
      rownames = FALSE,
      options = list(scrollX=TRUE,scrollY=TRUE,pageLength = 10)
    )
  })
  
  
  
  
  
  
  
})