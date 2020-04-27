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
library(gganimate)


load("Fig1_joined_data_bg.Rdata")
load("Gen_Race_pct_Male.Rdata")
load("Gen_Race_pct_Female.Rdata")


ui <- navbarPage(theme = shinytheme("slate"),
titlePanel("Comparing Demographics Between Undergrad Students and Enlisted Service"),
    mainPanel(
        tabsetPanel(
    tabPanel("CollegeDemographic Breakdowns",
             sidebarLayout(
                 sidebarPanel(
                     h2("The Changing Face of the American University"),
                     p("The data for these two graphs was gather from the
                       National Center for Education Statistics. It shows 
                       that there have large steps forward in increasing
                       the level of diversity in American undergraduate
                       programs")),
                 mainPanel(
                     plotlyOutput("Enr_fml"),
                     plotlyOutput("Enr_ml"))
             )),
    
    tabPanel("Historical Diversity Data",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("race", "Choose Racial Demographic",
                                 choices = c("White" , "Black", "Hispanic"), selected = "White")),
                 mainPanel(
                     plotlyOutput("enr_graph")
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
    
    output$enr_graph <- renderPlotly({
        if(input$race == "White") {
            y_value <- Fig1_joined_data_bg$white
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Particular Groups Who Are White \n From 1976-2017 (selected years)"
        } 
        else if(input$race == "Black") {
            y_value <- Fig1_joined_data_bg$black
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Particular Groups Who Are Black \n From 1976-2017 (selected years)"
        } else {
            y_value <- Fig1_joined_data_bg$hispanic
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Particular Groups Who Are Hispanic \n From 1976-2017 (selected years)"
        }
        
        # Use ggplot to create the framework for the graph. I used geom point to
        # make a scatter plot, geom smooth to add a line of best fit, and scale x
        # and y continuous to avoid the axies being in scientific notation because
        # it is not user friendly.
        
        Fig1_joined_data_bg %>% ggplot(aes(year, y_value, color=group)) +
            geom_point() + geom_line() +
            theme_minimal() +
            scale_x_continuous(breaks=c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015),
                               labels=c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)) +
            labs(title = Enr_title, x = "Year", y = y_lab, caption =
            "Sources: National Center for Education Statistics\n
            Office of The Under Secretary of Defense, Personnel\n
            and Readiness: 2017 Population Report") +
            scale_color_discrete(labels=c("Civilian Population\n(18-24 Years Old)",
                                          "Undergraduate College Students",
                                          "Enlisted Military\n(Non-prior Service Accession)"))
    })
    
    output$Enr_fml <- renderPlotly({
        Gen_Race_pct_Female %>%
            ggplot(aes(year, Percent, fill=Race)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() + labs(x="Year",
                                     title="Female Race Statistices for the Fall Undergraduate Semester")
        
        })
    
    output$Enr_ml <- renderPlotly({
         Gen_Race_pct_Male %>% ggplot(aes(year, Percent, fill=Race)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() + labs(x="Year", title="Male Race Statistices for the Fall Undergraduate Semester")
        
        })
     
    }
    

shinyApp(ui = ui, server = server)


