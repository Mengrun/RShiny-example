###Task 1 Getting data from forecast.io  
####Description  
#####Input: key, latitude, longtitude  
#####Output: data frame of the hourly weather forecast information of selected location

if(!("stringr" %in% installed.packages())) {
  install.packages("stringr", repos = "http://cran.us.r-project.org")
}
if(!("XML" %in% installed.packages())) {
  install.packages("XML", repos = "http://cran.us.r-project.org")
}
if(!("xml2" %in% installed.packages())) {
  install.packages("xml2", repos = "http://cran.us.r-project.org")
}
if(!("magrittr" %in% installed.packages())) {
  install.packages("magrittr", repos = "http://cran.us.r-project.org")
}
if(!("rvest" %in% installed.packages())) {
  install.packages("rvest", repos = "http://cran.us.r-project.org")
}
if(!("httr" %in% installed.packages())) {
  install.packages("httr", repos = "http://cran.us.r-project.org")
}

library("stringr")
library("XML")
library("xml2")
library("magrittr")
library("rvest")
library("httr")

key = "2ac3029a8c835c47abfe4ef01d1dd349"

get_weather_locs = function(key, lat, long)
{
  url = paste0(
    "https://api.forecast.io/forecast/",key,"/",lat,",",long
  )
  #subset the hourly data from url xml node to get a list
  a = GET(url)
  b = content(a)$hourly
  c = b$data
  summary = b$summary
  icon = b$icon
  name = unique(names(unlist(c)))
  #generate a data frame to store all the data
  #the number of columns is the maximum number among data of each hour
  data = as.data.frame(matrix(NA, ncol = length(name), nrow = length(c)))
  colnames(data) = name
  #fill the data frame
  for(i in 1:length(c)){
    for(j in 1: length(name)){
      if(name[[j]] %in% names(c[[i]]))
        data[i,j] = c[[i]][[name[[j]]]]
    }
  }
  #convert the unix time into EST date and time
  data$time = as.POSIXct(data$time, origin = "1970-01-01") 
  return(data)
}




if(!("maps" %in% installed.packages())) {
  install.packages("maps", repos = "http://cran.us.r-project.org")
}
library("maps")
if(!("dplyr" %in% installed.packages())) {
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
}
library("dplyr")
if(!("shiny" %in% installed.packages())) {
  install.packages("shiny", repos = "http://cran.us.r-project.org")
}
library("shiny")
if(!("ggplot2" %in% installed.packages())) {
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
}
library("ggplot2")
if(!("ggmap" %in% installed.packages())) {
  install.packages("ggmap", repos = "http://cran.us.r-project.org")
}
library("ggmap")

#subset the dataset with setting the population larger than 2500000
worldcity = filter(world.cities, pop > 2500000)

shinyApp(
  ui = fluidPage(
    #set title
    titlePanel(
      "World Cities Weather Forecast Hourly"
    ),
    sidebarPanel(
      #allow user to select a city in my data set
      selectInput("city", label = h4("World Cities"),
                  choices = worldcity$name),
      #allow user to select whether to show the forecast information of next 24 hours or next 48 hours
      checkboxInput(inputId = "next24",
                    label = "Next 24 hours",
                    value = FALSE),
      checkboxInput(inputId = "next48",
                    label = "Next 48 hours",
                    value = FALSE),
      #Once the user select the checkbox above, the panel will show up to allow user to select other information
      conditionalPanel(condition = "input.next24 == true",
                       selectInput("forecast24", label = "Next 24 hours forecast",
                                   choices = c("","precipProbability","humidity","windSpeed"))
      ),
      conditionalPanel(condition = "input.next48 == true",
                       selectInput("forecast48", label = "Next 48 hours forecast",
                                   choices = c("","precipProbability","humidity","windSpeed"))
      )
    ),
    
    mainPanel(
      h4("City Location:"),
      plotOutput("map"),
      strong(verbatimTextOutput("cityinfo")),
      h4("Current temperature:"),
      strong(verbatimTextOutput("current_temperature")),
      h4("Summary:"),
      strong(verbatimTextOutput("summary")),
      h4("Precipitation Probability:"),
      strong(verbatimTextOutput("precipProbability")),
      h4("Humidity:"),
      strong(verbatimTextOutput("humidity")),
      h4("Wind Speed:"),
      strong(verbatimTextOutput("windSpeed")),
      
      hr(),
      #if the user select a checkbox above, the plot will be shown
      conditionalPanel(condition = "input.next24 == true",
                       h4("Next 24 hour"),
                       plotOutput("next24plot")
      ),
      conditionalPanel(condition = "input.next48 == true",
                       h4("Next 48 hour"),
                       plotOutput("next48plot"))
    )  
  ),
  
  server = function(input, output) {
    #subset the information of the city the user select
    ct = reactive(worldcity[which(worldcity$name == input$city),])
    output$cityinfo = renderPrint({cat("Country:",ct()[[2]],"\n",
                                       "Latitude:",ct()[[4]],"\n",
                                       "Longitude:",ct()[[5]],"\n")})
    #get the weather information of the city the user select
    weather = reactive(
      get_weather_locs(key, ct()[[4]],ct()[[5]])
    )
    next24hours = reactive(weather()[2:25,])
    next48hours = reactive(weather()[2:49,])
    #get the map centered on the city the user select
    citymap = reactive(get_map(location = c(left=ct()[[5]]-20,right=ct()[[5]]+20,bottom = ct()[[4]]-8,top=ct()[[4]]+8),maptype = "terrain"))
    output$map = renderPlot(
      ggmap(citymap()) + geom_point(data =ct(), aes(x = long, y = lat), colour = "orange",size = 2)+geom_text(data = ct(),aes(x = long, y = lat,label = name),size = 5,hjust = 0, vjust = 0)
    )
    output$current_temperature = renderPrint(paste0(weather()[1,"temperature"],"F"))
    output$summary = renderPrint(weather()[1,"summary"])
    output$precipProbability = renderPrint(weather()[1,"precipProbability"])
    output$humidity = renderPrint(weather()[1,"humidity"])
    output$windSpeed = renderPrint(weather()[1,"windSpeed"])
    
    #generate the data frame to store time and the information the user may select
    d1 = reactive(data.frame(time = next24hours()[,"time"], forecast = next24hours()[,"temperature"], panel = "a"))
    d2 = reactive(data.frame(time = next24hours()[,"time"], forecast = next24hours()[,"precipProbability"], panel = "b"))
    d3 = reactive(data.frame(time = next24hours()[,"time"], forecast = next24hours()[,"humidity"], panel = "b"))
    d4 = reactive(data.frame(time = next24hours()[,"time"], forecast = next24hours()[,"windSpeed"], panel = "b"))
    
    m1 = reactive(data.frame(time = next48hours()[,"time"], forecast = next48hours()[,"temperature"], panel = "a"))
    m2 = reactive(data.frame(time = next48hours()[,"time"], forecast = next48hours()[,"precipProbability"], panel = "b"))
    m3 = reactive(data.frame(time = next48hours()[,"time"], forecast = next48hours()[,"humidity"], panel = "b"))
    m4 = reactive(data.frame(time = next48hours()[,"time"], forecast = next48hours()[,"windSpeed"], panel = "b"))
    
    
    output$next24plot = renderPlot({
      if(input$next24){
        if(input$forecast24 == ""){
          
          #if the user doesn't select a detailed information in the future, it will only show the plot of the temperature
          ggplot(data = next24hours())+geom_line(aes(x = time, y = temperature),colour = 'red',linetype = 1,size = 0.8,alpha = 0.7)+theme(axis.title=element_text(size=14,face = "bold"))+labs(title = "Temperature in next 24 hours")+theme(plot.title =  element_text(size = 18),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 14))
        }else if(input$forecast24 == "precipProbability"){
          
          #plot the temperature and precipProbability in the same plot
          d = reactive(rbind(d1(),d2()))
          q = ggplot(data = d(),aes(x = time, y = forecast))+labs(title = "Temperature and Precipitation probability forecast in next 24 hours")
          q = q + facet_grid(panel~., scale = "free_y")
          q = q + layer(data = d1(), geom = c("line"),stat = "identity",color = "red",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + layer(data = d2(), geom = c("line"),stat = "identity", color = "blue",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + theme(axis.title=element_text(size=14,face = "bold"),plot.title =  element_text(size = rel(1.5)),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 14))
          q
        }else if(input$forecast24 == "humidity"){
          #plot the temperature and humidity in the same plot
          d = reactive(rbind(d1(),d3()))
          q = ggplot(data = d(),aes(x = time, y = forecast))+labs(title = "Temperature and Humidity forecast in next 24 hours")
          q = q + facet_grid(panel~., scale = "free_y")
          q = q + layer(data = d1(), geom = c("line"),stat = "identity",color = "red",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + layer(data = d3(), geom = c("line"),stat = "identity", color = "pink",linetype = 1,size = 1)
          q = q + theme(axis.title=element_text(size=14,face = "bold"),plot.title =  element_text(size = rel(1.5)),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 14))
          q
        }else if(input$forecast24 == "windSpeed"){
          #plot the temperature and wind speed in the same plot
          d = reactive(rbind(d1(),d4()))
          q = ggplot(data = d(),aes(x = time, y = forecast))+labs(title = "Temperature and Wind speed forecast in next 24 hours")
          q = q + facet_grid(panel~., scale = "free_y")
          q = q + layer(data = d1(), geom = c("line"),stat = "identity",color = "red",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + layer(data = d4(), geom = c("line"),stat = "identity", color = "green",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + theme(axis.title=element_text(size=14,face = "bold"),plot.title =  element_text(size = rel(1.5)),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 14))
          q
        }
      }
    })
    
    ##The code below is the same as it of next 24 hours, but to show the results of next 48 hours
    output$next48plot = renderPlot({
      if(input$next48){
        if(input$forecast48 == ""){
          ggplot(data = next48hours())+geom_line(aes(x = time, y = temperature),colour = 'red',linetype = 1,size = 0.8,alpha = 0.7)+theme(axis.title=element_text(size=14,face = "bold"))+labs(title = "Temperature in next 48 hours")+theme(plot.title =  element_text(size = 18),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 14))
        }else if(input$forecast48 == "precipProbability"){
          d = reactive(rbind(m1(),m2()))
          q = ggplot(data = d(),aes(x = time, y = forecast))+labs(title = "Temperature and Precipitation probability forecast in next 48 hours")
          q = q + facet_grid(panel~., scale = "free_y")
          q = q + layer(data = m1(), geom = c("line"),stat = "identity",color = "red",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + layer(data = m2(), geom = c("line"),stat = "identity", color = "blue",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + theme(axis.title=element_text(size=14,face = "bold"),plot.title =  element_text(size = rel(1.5)),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 14))
          q
        }else if(input$forecast48 == "humidity"){
          d = reactive(rbind(m1(),m3()))
          q = ggplot(data = d(),aes(x = time, y = forecast))+labs(title = "Temperature and Humidity forecast in next 48 hours")
          q = q + facet_grid(panel~., scale = "free_y")
          q = q + layer(data = m1(), geom = c("line"),stat = "identity",color = "red",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + layer(data = m3(), geom = c("line"),stat = "identity", color = "pink",linetype = 1,size = 1)
          q = q + theme(axis.title=element_text(size=14,face = "bold"),plot.title =  element_text(size = rel(1.5)),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 14))
          q
        }else if(input$forecast48 == "windSpeed"){
          d = reactive(rbind(m1(),m4()))
          q = ggplot(data = d(),aes(x = time, y = forecast))+labs(title = "Temperature and Wind speed forecast in next 48 hours")
          q = q + facet_grid(panel~., scale = "free_y")
          q = q + layer(data = m1(), geom = c("line"),stat = "identity",color = "red",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + layer(data = m4(), geom = c("line"),stat = "identity", color = "green",linetype = 1,size = 0.8,alpha = 0.7)
          q = q + theme(axis.title=element_text(size=14,face = "bold"),plot.title =  element_text(size = rel(1.5)),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 14))
          q
        }
      }
    })
  },
  
  options = list(height = 1500)
)
