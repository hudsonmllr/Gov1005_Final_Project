library(shiny)
library(readr)
library(tidyverse)
library(janitor)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggthemes)
library(shinythemes)

load("Fig1_joined_data_bg.Rdata")
load("Fig1_DOD_data.Rdata")

ui <- fluidPage(
    theme = shinytheme("lumen"),
titlePanel("Comparing Demographics Between Undergrad Students and Enlisted Service"),
    mainPanel(
        tabsetPanel(
    tabPanel("Current Demographic Breakdowns", plotOutput("First_Graph"),
             h2("Discussion"),
             p("This is what I found")
             ),
    
    tabPanel("Historical Diversity Data",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("race", "Choose Racial Demographic",
                                 choices = c("White" , "Black", "Hispanic"), selected = "White")),
                 mainPanel(
                     plotOutput("enr_graph")
                 )),
             h2("Discussion"),
             p("This is what I found")
             ),
    
    tabPanel("About", 
             h2("About the Project"),
             p("The purpose of this project is to compare and analyze the difference in
               racial and gender diversity between the United States Armed Forces and
               US colleges and universities. The military has become more racial diverse
               in last few decades but despite this trend towards diversity, there has
               not been a surge of females to the armed forces. Likewise, US colleges
               and universities have seen higher numbers of women enroll but they still
               lack racial diversity."),
             h2("Methodology"),
             p("This project will use college enrollment data gathered from National 
               Center for Education Statistics and military demographic statistics
               gathered by the Office of the Under Secretary of Defense for Personnel
               and Readiness and published in the Fiscal Year 2017 Population Representation
               in the Military Service. In order to compare the new fields, I will
               first display the racial and gender breakdowns for each field. I will then
               see if those racial or gender statistics can be explained by where the
               majority of applicants for the military or college are located."),
             h2("About Me"),
             p("My name is Hudson Miller and I am pursuing a degree in Economics at
             Harvard College 
             You can reach me at Hudsonmiller@college.harvard.edu.")))))

server <- function(input, output){
    
    output$enr_graph <- renderPlot({
        if(input$race == "White") {
            y_value <- Fig1_joined_data_bg$white
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Undergraduate Students Who Are White \n Fall Term from 1976-2017 (selected years)"
        } 
        else if(input$race == "Black") {
            y_value <- Fig1_joined_data_bg$black
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Undergraduate Students Who Are Black \n Fall Term from 1976-2017 (selected years)"
        } else {
            y_value <- Fig1_joined_data_bg$hispanic
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Undergraduate Students Who Are Hispanic \n Fall Term from 1976-2017 (selected years)"
        }
        
        # Use ggplot to create the framework for the graph. I used geom point to
        # make a scatter plot, geom smooth to add a line of best fit, and scale x
        # and y continuous to avoid the axies being in scientific notation because
        # it is not user friendly.
        
        Fig1_joined_data_bg %>% ggplot(aes(year, y_value, color=group)) +
            geom_plot() +
            theme_minimal() +
            scale_x_continuous(breaks=c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015),
                               labels=c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)) +
            labs(title = Enr_title, x = "Year", y = y_lab, caption = "Source: National Center for Education Statistics")
    })
    
    
    output$branch_graph <- renderPlot({
        if(input$branch == "Army") {
            y_value <- Fig1_joined_data_bg$Army
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Undergraduate Students Who Are White \n Fall Term from 1976-2017 (selected years)"
        } 
        else if(input$race == "Black") {
            y_value <- Fig1_joined_data_bg$black
            y_lab <- "Percentage"
            Enr_title <-
                
                
    
    
    }
    

shinyApp(ui = ui, server = server)
