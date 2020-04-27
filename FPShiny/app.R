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
load("DOD_Gender_Data.Rdata")
load("Fall_Enr_Totals_tidy.Rdata")

ui <- navbarPage(theme = shinytheme("slate") ,
titlePanel("Comparing Demographics Between Undergrad Students and Enlisted Service"),
    mainPanel(
        tabsetPanel(
    tabPanel("College Demographic Breakdowns",
             sidebarLayout(
                 sidebarPanel(
                     h2("The Changing Face of the American University"),
                     p("The data for these two graphs was gather from the
                       National Center for Education Statistics. It shows 
                       that there have large steps forward in increasing
                       the level of diversity in American undergraduate
                       programs")),
                 mainPanel(
                     plotlyOutput("Enr_Gend"),
                     plotlyOutput("Enr_fml"),
                     plotlyOutput("Enr_ml"))
             )),
    tabPanel("Military Gender Statistics",
             sidebarLayout(
                 sidebarPanel(
                     h2("The lack of female representation in the U.S. Armed Forces"),
                     p("Add comment")),
                 mainPanel(
                     plotlyOutput("DOD_Civ_Gen_fig"),
                     plotlyOutput("DOD_Branch_Gen_fig"))
             )),
    tabPanel("Historical Diversity Data",
             sidebarLayout(
                 sidebarPanel(
                     h2("Colleges Are Leading the Charge?"),
                     p("When you look at the data for the percentage of college students
               who are white numbers have decreases at a remarkable rate. While this shows
               a promising shift towards a more diverse student body at American schools,
               the figures for black and hispanic students have not seen as great of a change.
               It is important to note that the figures for people joining the enlisted military
               are much higher for black and hispanic citiziens. Both these groups are
               disproportionately represented in the United States Armed Forces. I have included
               a regression line for each group that shows the trend line based on the given data.
               This line helps us understand how those variables may behave in the future."),
                     selectInput("race", "Choose Racial Demographic",
                                 choices = c("White" , "Black", "Hispanic"), selected = "White")),
                 mainPanel(
                     plotlyOutput("enr_graph")
                 ))),
    tabPanel("Predicting Future Shifts",
             sidebarLayout(
                 sidebarPanel(
                     h2("How will the demographic makeup of America's military and
                        universities continue to shift?"),
                     p("The projects are derived from running basic regressions
                       on the current data of the two groups")),
                 mainPanel()
             )),
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
             p("My name is Hudson Miller and before coming to Harvard College, I spent
             5 and a half years in the Marine Corps. I am a First Year and I intend to concentrate in Economics
             You can reach me at Hudsonmiller@college.harvard.edu.")))))

server <- function(input, output){
    
    output$Enr_Gend <- renderPlotly({
        Fall_Enr_Totals_tidy %>%
            ggplot(aes(Year, Percent)) +
            geom_hline(yintercept=50, linetype="dashed", color = "blue") +
            scale_y_continuous(labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                               breaks= c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
            geom_bar(stat = "identity", color = "#FF6666") +
            theme_economist_white() + labs(x="Year",
                                           title="Percentage of Student Enrolling in Fall Undergraduate Semester Who Are Female ")
        
    })
    
    output$Enr_fml <- renderPlotly({
        Gen_Race_pct_Female %>%
            ggplot(aes(Year, Percent, fill=Race)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() + labs(x="Year",
                                           title="Female Race Statistices for the Fall Undergraduate Semester")
        
    })
    
    output$Enr_ml <- renderPlotly({
        Gen_Race_pct_Male %>% ggplot(aes(Year, Percent, fill=Race)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() + labs(x="Year",
                                           title="Male Race Statistices for the Fall Undergraduate Semester")
        
    })
    
    output$DOD_Civ_Gen_fig <- renderPlotly({
        DOD_Gender_Data %>%
            filter(Group == "DOD" | Group == "Civilian") %>%
            ggplot(aes(Year,Percentage,fill=Group)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() + labs(x="Year",
                                           title="Female Representation in the Military",
                                           subtitle = "NPS accessions compared to 18-24 year-old civilian population")
    })
    
    output$DOD_Branch_Gen_fig <- renderPlotly({
        DOD_Gender_Data %>%
            filter(!Group == "DOD" & !Group == "Civilian") %>%
            ggplot(aes(Year,Percentage,fill=Group)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() + labs(x="Year",
                                           title="Female Representation in the Military",
                                           subtitle = "NPS accessions compared to 18-24 year-old civilian population")
    })
    
   
    
    output$enr_graph <- renderPlotly({
        if(input$race == "White") {
            Pct <- Fig1_joined_data_bg$white
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Particular Groups Who Are White \n From 1976-2017 (selected years)"
        } 
        
        else if(input$race == "Black") {
            Pct <- Fig1_joined_data_bg$black
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Particular Groups Who Are Black \n From 1976-2017 (selected years)"
        } else {
            Pct <- Fig1_joined_data_bg$hispanic
            y_lab <- "Percentage"
            Enr_title <- "Percentage of Particular Groups Who Are Hispanic \n From 1976-2017 (selected years)"
        }
        
        # Use ggplot to create the framework for the graph. I used geom point to
        # make a scatter plot, geom smooth to add a line of best fit, and scale x
        # and y continuous to avoid the axies being in scientific notation because
        # it is not user friendly.
        
        Fig1_joined_data_bg %>% ggplot(aes(year, Pct, color=group)) +
            geom_point() + geom_line() +
            theme_minimal() +
            geom_smooth(method = "lm") +
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
    
  
     
    }
    

shinyApp(ui = ui, server = server)


