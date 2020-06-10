#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(lubridate)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(ggplot2)
library(reshape2)
library(gridExtra) 

#-----------------



ui <- fluidPage(
  
  
  conditionalPanel(condition="input.plot_tabs != 'User guide'",
                   tabsetPanel(id="ui_tab",
                               tabPanel("National and State Data", 
                                        column(7, h4("Click a site"), 
                                               leafletOutput("StateMap", height = "380px"))
                                        ,
                                        column(5, plotOutput("stateplot"))
                               ) 
                               ,
                               tabPanel("US Local Data", 
                                        column(7, h4("Click a site"), 
                                               leafletOutput("LocalMap", height = "380px"))
                                        ,
                                        column(5, plotOutput("localplot"))
                                        
                               )
                               ,
                               tabPanel("Top Countries", column(11, plotOutput("TopCountries", height = "400px"))
                               ),
                               tabPanel("Top US States", column(11, plotOutput("TopStates", height = "400px"))
                               ),
                               tabPanel("Top US Cities", column(11, plotOutput("TopUSCities", height = "400px"))
                               ),
                               
                               tabPanel("Predictive Model & Source",   h4("Link to Coronavirus Predictive Model and\n
                                                                                            Data Source"),
                                        uiOutput("sirModel"),
                                        uiOutput("DataSource"),
                                        uiOutput("github")
                               )
                   ),
                   
                   
                   fluidRow(verbatimTextOutput("map_marker_click"))
  )
)

# Load data

server = function(input, output, session){
  
  reactive_objects=reactiveValues()
  
  reactive_objects.2=reactiveValues()
  
  # all time series and local data
  rawdata = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  rawdata = rawdata[which(rawdata$Country_Region == "US"),]
  
  rawdeaths = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  rawdeaths = rawdeaths[which(rawdeaths$Country_Region == "US"), abs(ncol(rawdata)-ncol(rawdeaths)):ncol(rawdeaths)]
  
  confirmed.ar.us = aggregate(. ~ rawdata$Province_State, rawdata[which(colnames(rawdata)=="X1.22.20"):ncol(rawdata)], FUN = sum)
  
  US.latlon =  aggregate(. ~ rawdata$Province_State, rawdata[,which(colnames(rawdata) %in% c("Lat", "Long_"))], FUN = median)
  
  deaths.ar.us = aggregate(. ~ rawdeaths$Province_State, rawdeaths[which(colnames(rawdeaths)=="X1.22.20"):ncol(rawdeaths)], FUN = sum)
    
  # get rid of nuisance US entry that does not contain data
  #confirmed.ar.us = confirmed.ar.us[which(confirmed.ar.us$Province.State != "US"),]
  #deaths.ar.us = deaths.ar.us[which(deaths.ar.us$Province.State != "US"),]
  
  local.cases = cbind(rawdata$Combined_Key, rawdata$Lat, rawdata$Long_, rawdeaths$Population,
                      rawdata[ncol(rawdata)], rawdeaths[ncol(rawdeaths)] )
  
  colnames(local.cases) = c("Combined_Key", "Lat", "Long_", "Population", "Confirmed", "Deaths")
  #local.cases[which(local.cases$Country_Region == "US"),]
  
  local.cases[which(local.cases$Combined_Key == "Wayne,Michigan,US"),"Long_"] = rawdata[which(rawdata$Combined_Key == "Wayne, Michigan, US"),"Long_"]
  local.cases$Lat[which(local.cases$Combined_Key == "Southwest Utah, Utah, US")] = 37.87777
  rawdata$Lat[which(rawdata$Combined_Key == "Southwest Utah, Utah, US")] = 37.87777
  rawdeaths$Lat[which(rawdeaths$Combined_Key == "Southwest Utah, Utah, US")] = 37.87777
  # why are some local data plots not working (Houston, Tx; Wayne Michigan)
  
  #remove lat/lon of zero
  local.cases = local.cases[which(local.cases$Long_ != 0),]
  
  
  # global time series
  
  
  confirmed = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  
  #fix US column names
  #colnames(confirmed.ar.us) = colnames(confirmed)
  #china
  
  x = confirmed[which(confirmed$Country.Region ==  "China"),]
  confirmed.china = x
  confirmed.china = aggregate(. ~ confirmed.china$Country.Region, confirmed.china[,which(colnames(x)=="X1.22.20"):ncol(x)], FUN = sum)
  # aggregating adds a nuisance column
  #confirmed.china = confirmed.china[,2:ncol(confirmed.china)]
  #GPS
  
  confirmed.china$Province.State = ""
  confirmed.china$Country.Region = "China"
  confirmed.china$Lat = mean(x$Lat)
  confirmed.china$Long = mean(x$Long)
  
  # global total confirmed cases
  confirmed.global = confirmed
  confirmed.global$Province.State = ""
  confirmed.global = aggregate(. ~ confirmed.global$Province.State, confirmed[,which(colnames(confirmed)=="X1.22.20"):ncol(confirmed)], FUN = sum)
  #confirmed.global = confirmed.global[,2:ncol(confirmed.global)]
  #colnames(confirmed.global) = colnames(confirmed)
  confirmed.global$Province.State = ""
  confirmed.global$Country.Region = "Global" # give info for popup label  
  confirmed.global$Lat = 30
  confirmed.global$Long = -30
  
  
  confirmed.ar.us$Province_State = confirmed.ar.us$`rawdata$Province_State`
  #deaths.ar.us$Province_State = deaths.ar.us$`rawdeaths$Province_State`
  
  # match number of columns for US and global data frames
  confirmed.ar.us$Country_Region = "US"
  #deaths.ar.us$Country_Region = "US"
  
  confirmed.ar.us$Lat = US.latlon$Lat
  #deaths.ar.us$Lat = US.latlon$Lat
  
  confirmed.ar.us$Long_ = US.latlon$Long_
  #deaths.ar.us$Long_ = US.latlon$Long_
  
  #confirmed.ar.us = confirmed.ar.us[, 8:ncol(confirmed.ar.us)]
  #deaths.ar.us   = deaths.ar.us[, 8:ncol(deaths.ar.us)]
  
  confirmed.ar.us = confirmed.ar.us[, which(!colnames(confirmed.ar.us) %in% c("Combined_Key"))]
  #deaths.ar.us = deaths.ar.us[, which(!colnames(deaths.ar.us) %in% c("Combined_Key", "Population"))]
  
  #colnames(confirmed.ar.us)[1:ncol(confirmed.global)] = colnames(confirmed.global)[1:ncol(confirmed.global)]
  #colnames(deaths.ar.us)[1:ncol(confirmed.global)] = colnames(confirmed.global)[1:ncol(confirmed.global)]
  
  confirmed.ar.us = confirmed.ar.us[which(confirmed.ar.us$Long != 0), ]
  #deaths.ar.us = deaths.ar.us[which(deaths.ar.us$Long != 0), ]
  
  confirmed.ar.us = confirmed.ar.us[,2:ncol(confirmed.ar.us)] 
  confirmed.china = confirmed.china[,2:ncol(confirmed.china)] 
  confirmed.global = confirmed.global[,2:ncol(confirmed.global)]
  
  
  # combine global and US state case time series
  confirmed = cbind(confirmed[5:ncol(confirmed)], confirmed[1:4])
  colnames(confirmed.ar.us)=colnames(confirmed)
  colnames(confirmed.china)=colnames(confirmed)
  colnames(confirmed.global)=colnames(confirmed)
  
  confirmed = rbind(confirmed, 
                    confirmed.ar.us, 
                    confirmed.china, 
                    confirmed.global)
  
  
  confirmed = cbind(confirmed[, (ncol(confirmed)-3):ncol(confirmed)], confirmed[, 1:(ncol(confirmed)-4)])
  confirmed.ar.us = cbind(confirmed.ar.us[, (ncol(confirmed.ar.us)-3):ncol(confirmed.ar.us)], confirmed.ar.us[, 1:(ncol(confirmed.ar.us)-4)])
  # again for deaths
  deaths = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  
  # fix US column names
  #colnames(deaths.ar.us) = colnames(deaths)
  x = deaths[which(deaths$Country.Region ==  "China"),]
  deaths.china = x
  deaths.china = aggregate(. ~ deaths.china$Country.Region, deaths.china[,which(colnames(x)=="X1.22.20"):ncol(x)], FUN = sum)
  # aggregating adds a nuisance column
  #deaths.china = deaths.china[,2:ncol(deaths.china)]
  #GPS
  
  deaths.china$Province.State = ""
  deaths.china$Country.Region = "China"
  deaths.china$Lat = mean(x$Lat)
  deaths.china$Long = mean(x$Long)
  
  # global total deaths cases
  deaths.global = deaths
  deaths.global$Province.State = ""
  deaths.global = aggregate(. ~ deaths.global$Province.State, deaths[,which(colnames(deaths)=="X1.22.20"):ncol(deaths)], FUN = sum)
  #deaths.global = deaths.global[,2:ncol(deaths.global)]
  #colnames(deaths.global) = colnames(deaths)
  deaths.global$Province.State = ""
  deaths.global$Country.Region = "Global" # give info for popup label  
  deaths.global$Lat = 30
  deaths.global$Long = -30
  
  deaths.ar.us$Province_State = deaths.ar.us$`rawdeaths$Province_State`
  
  # match number of columns for US and global data frames
  deaths.ar.us$Country_Region = "US"
  deaths.ar.us$Lat = US.latlon$Lat
  deaths.ar.us$Long_ = US.latlon$Long_
  deaths.ar.us = deaths.ar.us[, which(!colnames(deaths.ar.us) %in% c("Combined_Key", "Population"))]
  
  #colnames(deaths.ar.us)[1:ncol(deaths.global)] = colnames(deaths.global)[1:ncol(deaths.global)]
  #colnames(deaths.ar.us)[1:ncol(deaths.global)] = colnames(deaths.global)[1:ncol(deaths.global)]
  
  deaths.ar.us = deaths.ar.us[which(deaths.ar.us$Long != 0), ]
  
  deaths.ar.us = deaths.ar.us[,2:ncol(deaths.ar.us)] 
  deaths.china = deaths.china[,2:ncol(deaths.china)] 
  deaths.global = deaths.global[,2:ncol(deaths.global)]
  
  
  # combine global and US state case time series
  deaths = cbind(deaths[5:ncol(deaths)], deaths[1:4])
  colnames(deaths.ar.us)=colnames(deaths)
  colnames(deaths.china)=colnames(deaths)
  colnames(deaths.global)=colnames(deaths)
  
  deaths = rbind(deaths, 
                    deaths.ar.us, 
                    deaths.china, 
                    deaths.global)
  
  deaths = cbind(deaths[, (ncol(deaths)-3):ncol(deaths)], deaths[, 1:(ncol(deaths)-4)])
  
  deaths.ar.us = cbind(deaths.ar.us[, (ncol(deaths.ar.us)-3):ncol(deaths.ar.us)], deaths.ar.us[, 1:(ncol(deaths.ar.us)-4)])
  
  # combine global and US state case time series
  
  recovered = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  
  ##############  
  #State / country map
  ###########################
  # labels for stat/country map
  labs = paste(confirmed$Province.State, confirmed$Country.Region, ": \n", as.character(confirmed[,ncol(confirmed)]), 
               " confirmed,",  "\n", as.character(deaths[,ncol(deaths)]), " deaths,",  "\n")
  
  active = data.frame(cbind(deaths[,ncol(deaths)], confirmed[,ncol(confirmed)]))
  active$cols = c()
  active$cols[which(active[,2] > 0)] = "darkorange"
  active$cols[which(active[,1] > 0)] = "darkred"
  active$cols[which(active$cols == "")] = ""
  
  
  
  output$StateMap = renderLeaflet(leaflet() %>%
                                    addTiles() %>%  # Add default OpenStreetMap map tiles
                                    addCircleMarkers(
                                      lng = confirmed$Long, lat = confirmed$Lat, 
                                      radius = log(as.numeric(confirmed[,ncol(confirmed)])+0.001),
                                      weight = 1, color = "gray",
                                      fillColor = active$cols, fillOpacity = 0.7,
                                      popup=labs))
  
  
  # label = labs
  
  observeEvent(input$StateMap_marker_click, { 
    p <- input$StateMap_marker_click
    reactive_objects$lat = p$lat
    reactive_objects$lng = p$lng
    print(p)
  })
  
  observe({
    req(reactive_objects$lng)
    reactive_objects$n = which(
      round(deaths$Long, 2) == round(reactive_objects$lng, 2) &
        round(deaths$Lat, 2) == round(reactive_objects$lat, 2))
  })
  
  
  output$stateplot=renderPlot({
    
    n = reactive_objects$n
    if(!is.numeric(reactive_objects$n)){n = which(confirmed$Province.State == "New York")}
    if(length(n)>1){n = n[2]}
    
    df = data.frame(as.Date(gsub("X", "", colnames(confirmed[,5:ncol(confirmed)])), 
                            
                            "%m.%d.%y"))
    
    colnames(df) = "date"
    
    df$confirmed = unlist(confirmed[n,5:ncol(confirmed)])
    df$deaths  =    unlist(deaths[n,5:ncol(deaths)])
    
    
    df = melt(df, id = "date")
    df = df[which(!is.na(df$variable )),]
    df = df[which(!is.na(df$value )),]
    
    ggplot(df, aes(x = date, y = value)) +
      geom_area(aes(color = variable, fill = variable), 
                alpha = 0.3, 
                position = position_dodge(0.8)
      )  + geom_point(aes(color = variable, fill = variable)
      )  + scale_color_manual(
        values = c(
          "gold", "darkred"))+scale_fill_manual(
            values = c("gold", "darkred")) + 
      labs(x = "Date", y = "Cases",
                   title =  paste("State Data Selection ",
                                  as.character(confirmed$Province.State[n]),
                                  as.character(confirmed$Country.Region[n]),
                                  sep = " ")) + scale_y_log10() + theme_minimal()
    
    
    
  })
  
  
  
  
  ##############  
  # Local map
  ###########################
  
  local.labels = paste(local.cases$Combined_Key, ":", 
                       local.cases$Confirmed, " confirmed,", 
                       local.cases$Deaths, " deaths")
  
  output$LocalMap = renderLeaflet(leaflet() %>%
                                    addTiles() %>%  # Add default OpenStreetMap map tiles
                                    addCircleMarkers(
                                      lng = local.cases$Long_, lat = local.cases$Lat, 
                                      radius = log(as.numeric(local.cases$Confirmed)+0.001),
                                      weight = 1, color = "gray",
                                      fillColor = "magenta", fillOpacity = 0.7,
                                      popup=local.labels))  
  
  
  
  # label = labs
  
  observeEvent(input$LocalMap_marker_click, { 
    q <- input$LocalMap_marker_click
    reactive_objects.2$lat = q$lat
    reactive_objects.2$lng = q$lng
    print(q)
  })
  
  observe({
    req(reactive_objects.2$lng)
    reactive_objects.2$m = which(
      round(rawdata$Long_, 2) == round(reactive_objects.2$lng, 2) &
        round(rawdata$Lat, 2) == round(reactive_objects.2$lat, 2))
  })
  
  
  output$localplot=renderPlot({
    
    m = reactive_objects.2$m
    if(!is.numeric(reactive_objects.2$m)){
      m = which(rawdata$Combined_Key == "Alexandria, Virginia, US")}
    if(length(m)>1){m = m[1]}
    
    df2 = data.frame(as.Date(gsub("X", "", colnames(rawdata[,12:ncol(rawdata)])), 
                             
                             "%m.%d.%y"))
    
    colnames(df2) = "date"
    
    df2$confirmed = unlist(rawdata[m,12:ncol(rawdata)])
    df2$deaths = unlist(rawdeaths[m,13:ncol(rawdeaths)])
    
    df2 = melt(df2, id = "date")
    df2 = df2[which(!is.na(df2$variable )),]
    df2 = df2[which(!is.na(df2$value )),]
    
    ggplot(df2, aes(x = date, y = value)) +
      geom_area(aes(color = variable, fill = variable), 
                alpha = 0.3, 
                position = position_dodge(0.8)
      )  + geom_point(aes(color = variable, fill = variable)
      )  + scale_color_manual(
        values = c(
          "gold", "darkred"))+scale_fill_manual(
            values = c(
              "gold", "darkred")
          ) + labs(x = "Date", 
                   y = "Cases",
                   title =  paste("Local Data Selection \n ", rawdata$Combined_Key[m],
                                  sep = " ")) + scale_y_log10() + theme_minimal()
    
    
    
  })
  
  ############################
  # Bar plots
  ###############################
  
  
  # Top Countries bar plot
  output$TopCountries=renderPlot({
    
    # Bar graph of most virulent contries to go at bottom of page
    # data 
    # make sure nrow is the same
    cx = confirmed[which(confirmed$Province.State == ""),]
    
    dx = deaths[which(deaths$Province.State == ""),]
    
    mm = min(c(nrow(cx), nrow(dx)))
    
    df3 = data.frame(cx$Country.Region[0:mm])
    colnames(df3) = "country"
    df3$confirmed = cx[0:mm, ncol(cx)]
    df3$deaths = dx[0:mm, ncol(dx)]
    
    # order and sort for max 10 countries
    df3 = df3[rev(order(df3$confirmed))[0:15],]
    
    # structure with melt from reshape2 for ggplot
    df3 = melt(df3, id = "country")
    
    # use ggplot for bar graph based on 
    # http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
    
    ggplot(data=df3, aes(x= reorder(country, -value), y= value, fill= variable)) +
      geom_bar(stat="identity", position=position_dodge())  +
      geom_text(
        aes(label=value), color="black", position=position_dodge(1), 
        hjust = -0.1, angle = 90, size=3) +
      labs(x = "Country", 
           y = "Incidents",
           title =  "Top Countries") + 
      geom_bar(stat="identity", position=position_dodge(), colour="black") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size=12, face="bold", colour="black"),
            axis.title.y = element_text( size=12, face="bold", colour="black")) + 
      scale_fill_manual( values = c( "gold", "darkred")) + 
      ylim(0, 1.2 * na.omit(max(df3$value)))
    
    
    
  })
  
  #------------------------------------------
  # Top States bar plot
  output$TopStates=renderPlot({
    
    # Bar graph of most virulent contries to go at bottom of page
    # data 
    # make sure nrow is the same
    cx = confirmed.ar.us[which(confirmed.ar.us$Province.State != ""),]
    dx = deaths.ar.us[which(deaths.ar.us$Province.State != ""),]
    
    mm = min(c(nrow(cx), nrow(dx)))
    
    df3 = data.frame(cx$Province.State[0:mm])
    colnames(df3) = "state"
    df3$confirmed = cx[0:mm, ncol(cx)]
    df3$deaths = dx[0:mm, ncol(dx)]
    
    df3 = df3[which(!df3$state  %in% 
                      c("United States Virgin Islands", "US", "Puerto Rico")),]
    
    
    # order and sort for max 10 countries
    df3 = df3[rev(order(df3$confirmed))[0:15],]
    
    df3 = df3[which(!df3$state  %in% 
                      c("United States Virgin Islands",
                        "Diamond Princess", "Grand Princess", "Virgin Islands" , "US", "Puerto Rico")),]
    
    # structure with melt from reshape2 for ggplot
    df3 = melt(df3, id = "state")
    
    # use ggplot for bar graph based on 
    # http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
    
    ggplot(data=df3, aes(x= reorder(state, -value), y= value, fill= variable))  + 
      ylim(0, (1.2*max(na.omit(df3$value)))) +
      geom_bar(stat="identity", position=position_dodge(1))  +
      geom_text(aes(label=value), color="black", position=position_dodge(1), 
                hjust = -0.1, angle = 90, size=3) +
      labs(x = "State", 
           y = "Incidents",
           title =  "Top States") + 
      geom_bar(stat="identity", position=position_dodge(1), colour="black") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size=12, 
                                       face="bold", colour="black"),
            axis.title.y = element_text(size=12, 
                                        face="bold", colour="black")) + 
      scale_fill_manual( values = c( "gold", "darkred"))
    
    
    
  })
  
  #------------------------------------------
  # Top Cities bar plot
  output$TopUSCities=renderPlot({
    
    # Bar graph of most virulent contries to go at bottom of page
    # data 
    # make sure nrow is the same
    x = local.cases
    x = x[rev(order(x$Confirmed)),]  
    df3 = data.frame(x$Combined_Key[0:15])
    colnames(df3) = "city"
    df3$confirmed = x$Confirmed[0:15]
    df3$deaths = x$Deaths[0:15]
    df3$city = gsub(", US","" ,df3$city)
    
    # order and sort for max 10 countries
    #df3 = df3[rev(order(df3$confirmed))[0:15],]
    
    # structure with melt from reshape2 for ggplot
    df3 = melt(df3, id = "city")
    
    df3 = na.omit(df3) # 4/25/2020 NYC had NA for deaths
    
    # use ggplot for bar graph based on 
    # http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
    
    ggplot(data=df3, aes(x= reorder(city, -value), y= value, fill= variable)) +
      geom_bar(stat="identity", position=position_dodge())  +
      geom_text(
        aes(label=value), color="black", position=position_dodge(1), 
        hjust = -0.1, angle = 90, size=3) +
      labs(x = "City", 
           y = "Incidents",
           title =  "Top US Cities") + 
      geom_bar(stat="identity", position=position_dodge(), colour="black") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size=12, face="bold", colour="black"),
            axis.title.y = element_text( size=12, face="bold", colour="black")) + 
      scale_fill_manual( values = c( "gold", "darkred")) + 
      ylim(0, 1.3 * na.omit(max(df3$value)))
    
    
  })
  
  
  
  # link to SIR epi model
  url1 <- a("RShiny Predictive Model Link", href = "https://acvanp.shinyapps.io/A-B-epi-model/")
  output$sirModel <- renderUI({
    tagList("Navigate to:", url1)
  })
  
  
  
  # link to data source
  url2 <- a("Data from Johns Hopkins University\n https://github.com/CSSEGISandData/COVID-19", 
            href = "https://github.com/CSSEGISandData/COVID-19")
  output$DataSource <- renderUI({
    tagList("Navigate to:", url2)
  })
  
  
  
  
  url3 <- a("R Code for App and Model https://github.com/acvanp/Epidemic", href = "https://github.com/acvanp/Epidemic")
  output$github <- renderUI({
    tagList("Navigate to:", url3)
  })
  
  
  
  
  }
# Print the map

shinyApp(ui, server)

