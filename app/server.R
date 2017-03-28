packages.used=c("rgeos", "sp", "rgdal", 
                "leaflet", "htmlwidgets", "shiny",
                "ggplot2", "dplyr", "data.table","DT")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}


library(rgeos)
library(sp)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)



#setwd("~/Desktop/Spr2017-proj2-grp2")
setwd("../")


group1 = "<span style='color: #7f0000; font-size: 11pt'><strong>count</strong></span>"
group2 = "<span style='color: #7f0000; font-size: 11pt'><strong>FPD</strong></span>"
group3 = "<span style='color: #7f0000; font-size: 11pt'><strong>Percentage Cash Paying:</strong></span>"
color = list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
             color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
             color3 = c("#F7FCF5","#74C476", "#005A32"))
bin = list(bin1 = c(0,100,1000,10000,100000,1000000,10000000), bin2 = c(0,1,2,3,4,5,6,7))
label = list(label1 = c("<100","100-1000","1000~10,000","10,000~100,000","100,000~1,000,000","1,000,000~10,000,000"),
             label2 = c("0-1","2-3","3-4","4-5","5-6","6","7+"),
             label3 = c("<0.4","0.4~0.6",">0.6"))
title = list(t1 = "Pick Up Numbers", t2 = "Fair Per Distance",t3  = "PercentagePayingCash")




load('output/myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))
dynamicdata = fread("data/pickupDropoff date_hour.csv", header = TRUE, stringsAsFactors=F)
#load('output/count_seperated_2015.RData')

load('output/count_seperated.RData')
load('output/FPD_seperated.RData')
rownames(count_result) = subdat@data$NTACode

subway1 = read.csv("data/sub.csv", header = TRUE, stringsAsFactors = F)
subway2 = read.csv("data/sub2.csv", header = TRUE, stringsAsFactors = F)
payper = read.csv("data/Data_frame_of_summary.csv")




count_result1 <- as.data.frame(count_result[,,1])

sum <- apply(count_result1, 1, sum)
as.matrix(count_result1)
count_result1 <- count_result1/sum 
as.data.frame(count_result1)
count_result1[is.na(count_result1)] <- 0

fit <- kmeans(count_result1, 9)
aggregate(count_result1,by=list(fit$cluster),FUN=mean)
count_result1 <- data.frame(count_result1, fit$cluster)



#head(subdat@data)
shinyServer(function(input, output,session) { 
  
  
  output$map <- renderLeaflet({
    
    #  if (!input$showhr){
    #    subdat@data$count = apply(count_result[, ,ifelse(input$days == "Business Day", 1, 2)], 1, sum)
    #  }
    #  else{
    #    subdat@data$count = count_result[, input$hr_adjust+1, ifelse(input$days == "Business Day", 1, 2)]
    #  }
   
    if (input$days == "All day"){
      count_intermediate = count_result %>% apply(c(1,2), sum)
      FPD_intermediate = FPD_result %>% apply(c(1,2), mean, na.rm = T)
    }else{
      count_intermediate = count_result[ , , (input$days == "Not Business Day") + 1]
      FPD_intermediate = FPD_result[ , , (input$days == "Not Business Day") + 1]
    }
    if (!input$showhr){
      subdat@data$count = count_intermediate %>% apply(1, sum)
      subdat@data$FPD = FPD_intermediate %>% apply(1, mean, na.rm = T)
    }else{
      subdat@data$count = count_intermediate[, input$hr_adjust+1]
      subdat@data$FPD = FPD_intermediate[, input$hr_adjust+1]
    }
    
    ######
    
    blocks_coord = data.frame(center_lng = rep(NA, 195), center_lat = rep(NA, 195)) # Combine borough coord for future marking purpose
    for (i in 1:195){ blocks_coord[i,] = subdat@polygons[[i]]@labpt }    # One more update: add long/lat permanently into myShape@data as

    subdat_top5_intermediate = cbind(subdat@data, blocks_coord)

     if (input$boroSelect == "All"){ # filter borough
       subdat_top5 = subdat_top5_intermediate
     } else {
       subdat_top5 = subdat_top5_intermediate[subdat@data$BoroName == input$boroSelect, ]
     }
    if (!input$showbr){
      subdat_top5 = subdat_top5_intermediate
    }

    subdat_top5 = subdat_top5 %>% 
      subset(select = c("NTACode", "NTAName", "count", "FPD", "center_lng", "center_lat"))

    top5count = subdat_top5[order(subdat_top5$count, decreasing = T),
                            c("NTAName", "count", "center_lng", "center_lat")] %>% head(5) # fetch top-5-rows with the most counts/FPD
    top5FPD = subdat_top5[order(subdat_top5$FPD, decreasing = T),
                          c("NTAName", "FPD", "center_lng", "center_lat")] %>% head(5)

    
    ######
  
    
    #subdat@data$count = count_result[, input$hr_adjust+1,  1]
    
    subdat_data=subdat@data[,c("NTACode", "NTAName", "count", "FPD")]
    subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
    
    # print leaflet
    pal = colorBin(color[[1]], bins = bin[[1]])
    pal_FPD = colorBin(color[[2]], bins = bin[[2]])
    pal2 = colorBin(c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"), 1:10)
    pal3 = colorBin(c("#005A32", "#74C476", "#F7FCF5"), 0:0.125:1)
    
    
    popup1 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Count of pick-ups: </strong><br>', subdat_data$count)
    popup2 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Fair Per Distance: </strong><br>', subdat_data$FPD)
    popup3 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName)
    popup4 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Percentage Paying Cash: </strong><br>', payper$PercentagePaying)
    
    
    greenLeafIcon <- makeIcon(
      iconUrl = "https://cdn1.iconfinder.com/data/icons/weather-19/32/fire-512.png",
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 0, iconAnchorY = 0
      # shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      # shadowWidth = 50, shadowHeight = 64,
      # shadowAnchorX = 4, shadowAnchorY = 62
    )
    redLeafIcon <- makeIcon(
      iconUrl = "https://maxcdn.icons8.com/Share/icon/Finance//usd1600.png",
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 0, iconAnchorY = 0
      # shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      # shadowWidth = 50, shadowHeight = 64,
      # shadowAnchorX = 4, shadowAnchorY = 62
    )
    

    # pic1<-leaflet(subdat) %>%
    #   setView(lat=40.7128, lng=-74.0059, zoom=10) %>%
    #   addProviderTiles('CartoDB.Positron') %>%
    #   addPolygons(fillColor = ~pal(count), color = 'grey', weight = 1, 
    #               popup = popup1, fillOpacity = .6, group = group1) %>%
    #   addPolygons(fillColor = ~pal_FPD(FPD), color = 'grey', weight = 1, 
    #               popup = popup2, fillOpacity = .6, group = group2) %>%
    #   addLayersControl(baseGroups = c(group1,group2), 
    #                    options = layersControlOptions(collapsed = F)) 

    
    pic1<-leaflet(subdat) %>%
      setView(lat=40.7128, lng=-74.0059, zoom=10) %>%
      addProviderTiles('CartoDB.Positron') 

    
    if (input$CF == "count"){
      pic1<-pic1 %>%
        addPolygons(fillColor = ~pal(count), color = 'grey', weight = 1, 
                    popup = popup1, fillOpacity = .6, group = group1) %>%
        addLegend(position = "bottomright",
                  colors = color[[1]],
                  labels = label[[1]],
                  opacity = 0.6,
                  title = title[[1]])
    }
    else if (input$CF == "FPD"){
      pic1<-pic1 %>%
        addPolygons(fillColor = ~pal_FPD(FPD), color = 'grey', weight = 1, 
                    popup = popup2, fillOpacity = .6, group = group2) %>%
        addLegend(position = 'bottomright',
                  colors = color[[2]],
                  labels = label[[2]], ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = title[[2]])
    }
    
    else if(input$CF == "cluster1"){
      pic1<-pic1 %>%
      addPolygons(fillColor = ~pal2(count_result1$fit.cluster), color = 'grey', weight = 1, popup = popup3, fillOpacity = .6)

    }
    
    else if (input$CF == "cash"){
      pic1<-pic1 %>%
        addPolygons(fillColor =  ~pal3(payper$PercentagePayingCash), color = 'grey', weight = 1, 
                    popup = popup4, fillOpacity = .6, group = group3) %>%
        addLegend(position = 'bottomright',
                  colors = color[[3]],
                  labels = label[[3]], ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = title[[3]])
    }

  
###TOP5    
    if (input$top15count == TRUE){
    pic1<-pic1 %>%
    addMarkers(~top5count$center_lng, ~top5count$center_lat, icon = greenLeafIcon)
    }

    else{
      pic1
    }
    
    if (input$top15FPD == TRUE){
      pic1<-pic1 %>%
        addMarkers(~top5FPD$center_lng, ~top5FPD$center_lat, icon = redLeafIcon)
    }

    else{
      pic1
    }

### subway analysis    
    if (input$subway == 1){
      pic1
    }
    else if (input$subway == 2) {
      pic1<-pic1 %>% addMarkers(data = subway1, ~Latitude, ~Longitude, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))  
    }
    else if (input$subway == 3){
    pic1<-pic1 %>% addMarkers(data = subway2, ~L1, ~L2, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))  
      
    }
    

  })
  
  
  
  # observeEvent(input$map_groups,{
  #   map <- leafletProxy("map")%>% clearControls()
  #   if (input$map_groups == group1)
  #   {map<- map %>%
  #     addLegend(position = "bottomright",
  #               colors = color[[1]],
  #               labels = label[[1]],
  #               opacity = 0.6,
  #               title = title[[1]])
  #   }
  #   
  #   else if (input$map_groups == group2)
  #   {map <- map %>%
  #     addLegend(position = 'bottomright',
  #               colors = color[[2]],
  #               labels = label[[2]], ## legend labels (only min and max)
  #               opacity = 0.6,      ##transparency again
  #               title = title[[2]])
  #   }
  # })
  
  
  observe({
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    dattest = data.frame(Longitude = event$lng, Latitude = event$lat)
    coordinates(dattest) <- ~ Longitude + Latitude
    proj4string(dattest) <- CRS("+proj=longlat")
    dattest <- spTransform(dattest, proj4string(myShape1))
    rtest = over(dattest, myShape1)
    
    output$districttimeplot <- renderPlot({
      if (nrow(rtest) == 0) {
        return(NULL)
      }
      if (input$days == "All Day"){
        count_resultNTA = count_result[which(rownames(count_result) == rtest$NTACode),,]
        count_resultNTA = apply(count_resultNTA, 1, sum)
        index <- c(0:23)
        dfcount_resultNTA <- data.frame(index, count_resultNTA)
        ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
          labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
      }
      else if (input$days == "Business Day"){
        count_resultNTA = count_result[which(rownames(count_result) == rtest$NTACode),,1]
        index <- c(0:23)
        dfcount_resultNTA <- data.frame(index, count_resultNTA)
        ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
          labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
      }
      else if (input$days == "Not Business Day") {
        count_resultNTA = count_result[which(rownames(count_result) == rtest$NTACode),,2]
        index <- c(0:23)
        dfcount_resultNTA <- data.frame(index, count_resultNTA)
        ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
          labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
      }
      
    })
  })
    
    output$map2 <- renderLeaflet({
      leaflet() %>%
        setView(lat=40.7128, lng=-74.0059, zoom=11) %>%
        addProviderTiles('CartoDB.Positron') 
    })
    
    drawvalue <- reactive({
      if (input$pd == 'pick up'){
        t <- filter(dynamicdata, pickup_hour == input$hours, pickup_date == "1/1/2015")
        return(t)
      }
      else{
        t <- filter(dynamicdata, dropoff_hour == input$hours, dropoff_date == "1/1/2015")
        return(t)
      }
    })
  
    observe({
      
      radius <-  100
      if (input$pd == 'Pick up')  {
        t <- filter(dynamicdata, pickup_hour == input$hours, pickup_date == input$`choose date`)
        longitudepmax <- max(t$pickup_longitude)
        latitudepmax <- max(t$pickup_latitude)
        longitudepmin <- min(t$pickup_longitude)
        latitudepmin <- min(t$pickup_latitude)
        leafletProxy("map2", data = t) %>%
          clearShapes() %>%  
          addCircles(~pickup_longitude, ~pickup_latitude, radius=radius,
                     stroke=FALSE, fillOpacity=0.8,fillColor = "blue") %>%
          addRectangles(
            lng1=longitudepmax, lat1=latitudepmax,
            lng2=longitudepmin, lat2=latitudepmin,
            fillColor = "transparent"
          )
      }
      else if (input$pd == 'Drop off')  {
        t <- filter(dynamicdata, dropoff_hour == input$hours, dropoff_date == input$`choose date`)
        longitudedmax <- max(t$dropoff_longitude)
        latitudedmax <- max(t$dropoff_latitude)
        longitudedmin <- min(t$dropoff_longitude)
        latitudedmin <- min(t$dropoff_latitude)
        leafletProxy("map2", data = t) %>%
          clearShapes() %>%
          addCircles(~dropoff_longitude, ~dropoff_latitude, radius=radius,
                     stroke=FALSE, fillOpacity=0.8,fillColor = "red") %>%
          addRectangles(
            lng1=longitudedmax, lat1=latitudedmax,
            lng2=longitudedmin, lat2=latitudedmin,
            fillColor = "transparent",
            color = "red"
          ) 
      }
      else if (input$pd == "All"){
        t <- filter(dynamicdata, dropoff_hour == input$hours | pickup_hour == input$hours, 
                    dropoff_date == input$`choose date` | pickup_date == input$`choose date`)
        longitudepmax <- max(t$pickup_longitude)
        latitudepmax <- max(t$pickup_latitude)
        longitudepmin <- min(t$pickup_longitude)
        latitudepmin <- min(t$pickup_latitude)
        longitudedmax <- max(t$dropoff_longitude)
        latitudedmax <- max(t$dropoff_latitude)
        longitudedmin <- min(t$dropoff_longitude)
        latitudedmin <- min(t$dropoff_latitude)
        
        leafletProxy("map2", data = t) %>%
          clearShapes() %>%
          addCircles(~dropoff_longitude, ~dropoff_latitude, radius=radius,
                     stroke=FALSE, fillOpacity=0.8,fillColor = "red") %>%
          addCircles(~pickup_longitude, ~pickup_latitude, radius=radius,
                     stroke=FALSE, fillOpacity=0.8,fillColor = "blue") %>%
          addRectangles(
            lng1=longitudepmax, lat1=latitudepmax,
            lng2=longitudepmin, lat2=latitudepmin,
            fillColor = "transparent"
          ) %>%
          addRectangles(
            lng1=longitudedmax, lat1=latitudedmax,
            lng2=longitudedmin, lat2=latitudedmin,
            fillColor = "transparent",
            color = "red"
          )
      }
      
    })
  
  
  
  
  dataa<-fread("data/rawdata.csv",header = T)
  output$rawtable <- DT::renderDataTable({
    DT::datatable(dataa)
  })
  
})


