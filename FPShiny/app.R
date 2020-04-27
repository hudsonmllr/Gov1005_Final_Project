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
titlePanel(
           tags$head(tags$link(rel = "shortcut icon",
                               href ="https://vignette.wikia.nocookie.net/marvelcinematicuniverse/images/9/9c/Harvard_shield_wreath.png/revision/latest?cb=20190302143211")),
           "Comparing Demographics Between Undergrad Students and Enlisted Service"),
    mainPanel(
        tabsetPanel(
    tabPanel("College Demographic Breakdowns",
             sidebarLayout(
                 sidebarPanel(
                     h2("The Changing Face of the American University"),
                     p("The data for these two graphs was gather from the
                       National Center for Education Statistics. There has been
                       a large increase in the share of women who going to
                       college in America. This has been a slow process but women
                       are now outnumber the men at American universities.
                       There is still much that needs to be done to increase the
                       racial diversity on college campuses"),
                     br(),
                     p("It is important to note that these figures are only looking
                       at white, black, and hispanic students and do not take into account
                       international students and other minority groups. I focused on these
                       3 racial groups due to the lack of statistics for smaller minoritiy
                       groups in previous years.")),
                 mainPanel(
                     h3("Students Enrolling in Fall Undergraduate Semester Who Are Female"),
                     plotlyOutput("Enr_Gend"),
                     br(),
                     h3("Breakdown of Female Students for the Fall Undergraduate Semester"),
                     plotlyOutput("Enr_fml"),
                     br(),
                     h3("Breakdown of Male Students for the Fall Undergraduate Semester"),
                     plotlyOutput("Enr_ml"))
             )),
    tabPanel("Military Gender Statistics",
             sidebarLayout(
                 sidebarPanel(
                     h2("The lack of female representation in the U.S. Armed Forces"),
                     p("Though there has been an increase in the representation of women on college
                       campuses, the same cannot be said for the U.S. Military. Despite slight increases 
                       in the female population, the military has a long way to go and is far from the
                       gender representation in the country as a whole. The military has remained 
                       a male dominated profession"),
                     br(),
                     h2("Next Steps"),
                     p("There have been steps to encourage more women to participate. One major change
                       was allowing women to serve in combat roles that had previously been restricted
                       to male service members. Another major shift is the increase in the number of
                       maternity days that I female service member recieves. These two changes have
                       led to slught increases in the number of women joining the military, but there
                       is still a lot of work to do.")),
                 mainPanel(
                     plotlyOutput("DOD_Civ_Gen_fig"),
                     br(),
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
               disproportionately represented in the United States Armed Forces."),
               br(),
                    p("I have included
               a regression line for each group that shows the trend line based on the given data.
               This line helps us understand how those variables may behave in the future."),
                     selectInput("race", "Choose Racial Demographic",
                                 choices = c("White" , "Black", "Hispanic"), selected = "White")),
                 mainPanel(
                     plotlyOutput("enr_graph")
                 ))),
    
    tabPanel("About", 
             h1("About the Project"),
             p("The purpose of this project is to compare and analyze the difference in
               racial and gender diversity between the United States Armed Forces and
               American undergraduate universities. Both groups have become more diverse
               over recent years, but I want to exam how each group has changes compared
               to the other and the American population as a whole. I wanted to see if
               trends could be found and if there were policy changes that could be
               shared or adopted in order to learn from eachother's successes and failures."),
             h1("Data"),
             p("This project uses college enrollment data gathered from National 
               Center for Education Statistics and military demographic statistics
               gathered by the Office of the Under Secretary of Defense for Personnel
               and Readiness and published in the Fiscal Year 2017 Population Representation
               in the Military Service. In order to compare the new fields, I will
               first display the racial and gender breakdowns for each field. I will then
               see if those racial or gender statistics can be explained by where the
               majority of applicants for the military or college are located."),
             tags$a(href="https://prhome.defense.gov/Portals/52/Documents/MRA_Docs/MPP/AP/poprep/2017/Appendix%20D%20-%20(Historical%20Component).pdf", "Military Data"),
             br(),
             tags$a(href="https://nces.ed.gov/programs/digest/d18/tables/dt18_306.10.asp", "NCES Data"),
             br(),
             h1("About Me"),
             p("My name is Hudson Miller and before coming to Harvard College, I spent
             5 and a half years in the Marine Corps. I am a First Year and I intend
             to concentrate in Economics. You can reach me at 
               Hudsonmiller@college.harvard.edu."),
             tags$a(href="https://github.com/hudsonmllr", "My Github"),
             br(),
             tags$a(href="https://www.linkedin.com/in/hudson-miller", "LinkedIn")))))

server <- function(input, output){
    
    # I first loaded the 2  gender graphs for the firt slide. They are both ran with
    # renderPlotly so that they are more interactive for the user.
    
    output$Enr_Gend <- renderPlotly({
        Fall_Enr_Totals_tidy %>%
            ggplot(aes(Year, Percent)) +
            geom_bar(stat = "identity", color = "blue") +
            
            # I wanted to add in the 50 percent line to put the amount in perspective.
            
            geom_hline(yintercept=50, linetype="dashed", color = "blue") +
            scale_y_continuous(labels = c(0, 10, 20, 30, 40, 50),
                               breaks= c(0, 10, 20, 30, 40, 50)) +
            theme_economist_white() +
            labs(x="Year")
    })
    
    output$Enr_fml <- renderPlotly({
        Gen_Race_pct_Female %>%
            ggplot(aes(Year, Percent, fill=Race)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() +
            labs(x="Year")
    })
    
    output$Enr_ml <- renderPlotly({
        Gen_Race_pct_Male %>%
            ggplot(aes(Year, Percent, fill=Race)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() +
            labs(x="Year")
    })
    
    # I had to use position dodge for these graphs and split them up because they became too messy to understand.
    # I am still considering splitting them up and possibly including a drop down to select which branch to observe.
    # But I do like being able to compare them to eachother on the same plot.
    
    output$DOD_Civ_Gen_fig <- renderPlotly({
        DOD_Gender_Data %>%
            filter(Group == "DOD" | Group == "Civilian") %>%
            ggplot(aes(Year,Percentage,fill=Group)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() +
            labs(x="Year",
                 title="Female Representation in the Military",
                 subtitle = "NPS accessions compared to 18-24 year-old civilian population")
    })
    
    output$DOD_Branch_Gen_fig <- renderPlotly({
        DOD_Gender_Data %>%
            filter(!Group == "DOD" & !Group == "Civilian") %>%
            ggplot(aes(Year,Percentage,fill=Group)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_economist_white() +
            labs(x="Year",
                 title="Female Representation in the Military by Brach",
                 subtitle = "NPS accessions compared to 18-24 year-old civilian population") +
            scale_fill_manual(values=c("grey", "green", "blue", "brown"))
    })

    # For this graph I had a lot of trouble with the various arguments. 
    # But this function takes in the selection of the user and then uses that
    # to build a plot using the plotting function that I created. 
    
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
        
        # The code above, turns the selection of the user into an input for the ggplot function below.
        
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


