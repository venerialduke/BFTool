#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

###########################

#Static Files#############

library(xlsx)
library(plyr)
library(lattice)
library(shiny)
library(scatterD3)
#library(googleVis)
#library(googleCharts)
library(dplyr)
library(zoo)
#library(magrittr)


#Test loading data and getting right segmenting, plot scatter plot
popData <- read.csv("Data/PopularityData.csv",header=TRUE)

#Eliminate No-Skin observations
popData <- popData[!(popData$skinName=='NoSkin'),]

#Make "Tier" column
popData$Tier <- sapply(popData$value,function(x) substring(x,1,2))
popData <- popData[popData$Tier %in% c('T1','T2','T3'),]

#Convert Date column
popData$Date <- sapply(popData$dt,function(x) as.Date(x, format='%m/%d/%Y'))

popData$Slot <- sapply(popData$weaponslot, function(x) ifelse(x==0,"Primary","Secondary"))

weaponCategories <- as.list(unique(popData$weaponcategory))


startDate <- as.Date(min(popData$Date))
endDate <- as.Date(max(popData$Date))

myPalette = colorRampPalette(c('blue','purple','red'))(11)
myColorInput <- myPalette
names(myColorInput) <- c(1:11)

# # Define UI for application that draws a histogram
 ui <- fluidPage(
   titlePanel("BF1 Popularity Tool"),
   
   sidebarLayout(
     sidebarPanel(
       helpText("Create demographic maps with 
         information from the 2010 US Census."),
       
       selectInput("Tier", 
                   label = "Choose a weapon Tier",
                   choices = c("T1", "T2",
                               "T3"),
                   selected = "T1",
                   multiple=TRUE),
       
       selectInput("Slot", 
                   label = "Choose a weapon Slot",
                   choices = c("Primary","Secondary"),
                   selected = "Primary",
                   multiple=TRUE),
       
       selectInput("Weapon", 
                   label = "Choose a weapon Category",
                   choices = weaponCategories,
                   selected = "Shotgun",
                   multiple=TRUE),
       
       selectInput("PopularityType", 
                   label = "Evaluate Metrics as Max or Average",
                   choices = c("Average","Max"),
                   selected = "Average",
                   multiple=FALSE),
       
       numericInput('Window',
                    label = 'Set Date Window (Days)',
                    value = 14,
                    min = 1,
                    max = 30,
                    step = 1),
       
       sliderInput('Start',
                   label = 'Start Date',
                   min = startDate,
                   max = endDate,
                   value = startDate,
                   animate=TRUE,
                   timeFormat='%F')
       
       
       #dateRangeInput('dateRange',
      #                label = 'Date Range',
      #                start = Sys.Date() - 28, end = Sys.Date()
      # )
     ),
     
     mainPanel(h2(textOutput("Title"),align="center"),
               scatterD3Output("myScatter",height = "700px"))
     #tableOutput("table1")
   )
 )
 
# # Define server logic required to draw a histogram
 server <- function(input, output) {
   
   mySummary <- reactive({
     
     
     
                          a <-  ddply(popData[(popData$Date >= input$Start) & (popData$Date <= input$Start + input$Window),], .(newItemKey,weaponcategory,weaponname,Tier,Slot,skinName), summarize, itemMax_wa = max(waAvailability),
                                itemMax_ia = max(itemAvailability),itemMax_pop = max(inSlot_popularity_score),
                                itemMean_wa = mean(waAvailability),
                                itemMean_ia = mean(itemAvailability),itemMean_pop = mean(inSlot_popularity_score))
                          return(a)
     
   })
 
   
   #plotData <- mySummary()[(mySummary()$Slot %in% input$Slot) & (mySummary()$weaponname %in% input$Weapon) & (mySummary()$Tier %in% input$Tier) & (mySummary()$itemMean_ia<.9)]
   plotData <- reactive({
             
             b <- subset(mySummary(),(Slot %in% input$Slot) & (weaponcategory %in% input$Weapon) & (Tier %in% input$Tier) & (itemMean_ia<.9))
             b$Weapon_Availability <- switch(input$PopularityType,
                                             "Average"=b$itemMean_wa,
                                             "Max"=b$itemMax_wa)

             b$Skin_Availability <- switch(input$PopularityType,
                                             "Average"=b$itemMean_ia,
                                             "Max"=b$itemMax_ia)
             
             b$Raw_Popularity <- switch(input$PopularityType,
                                             "Average"=b$itemMean_pop,
                                             "Max"=b$itemMax_pop)
             
             
             low = log(min(b$Raw_Popularity))
             high = log(max(b$Raw_Popularity))
             b$Popularity <- sapply(b$Raw_Popularity, function(x) toString(floor(10*(log(x)-low)/(high-low))+1))
             b$Potential <- with(b, as.numeric(Popularity)^2*Weapon_Availability)
             lowp = log(min(b$Potential))
             highp = log(max(b$Potential))
             #b$Potential <- sapply(b$Potential, function(x) floor(500*(log(x)-lowp)/(highp-lowp)))
             
             return(b)
   })
   
   
   output$myScatter <- renderScatterD3({
     
               
               scatterD3(data = plotData(), x = Weapon_Availability, y = Skin_Availability, col_var = Popularity,colors=myColorInput,
               point_opacity = 0.7, x_log=TRUE, y_log=TRUE, hover_size=1.5, size_var = Potential, size_range=c(25,500),lab=newItemKey,
               legend_width=0,labels_size=0,transitions = TRUE,xlab="Weapon Availability",ylab="Skin Availability")
     
     })
   myTitle <- reactive({paste("BF1 Popularity Tool - ",input$Start," - ",input$Start+input$Window)})
   output$Title <- renderText({myTitle()})
 }
# 
# # Run the application 
shinyApp(ui = ui, server = server)




#library(devtools)
#library(httr)
#set_config(config(ssl_verifypeer = 0L))
#if (!require(devtools))
#  install.packages("devtools")
#devtools::install_github("jcheng5/googleCharts")
