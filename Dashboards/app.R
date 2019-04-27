
## McCourt School of Public Policy - Georgetown University ####3
## Intro to Data Science - Spring 2019 ##
## Team: R against the Machine #

# Steffi, Diya, and Rafael ##

# Loading require libraries #
require(recipes)
require(tidyverse)
require(pdp)
require(utf8)
require("skimr")
require(foreign)
require(readr)
library(haven)
require(scales)
require(shiny)
require(shinydashboard)
require(shinythemes)

# Loading our main dataset, the 2016 American Survey of Refugees # 
ASRraw <- read_dta("2016-ASR_Public_Use_File.dta") ##Loading main dataset


#### Data wrangling ##


### EMPLOYMENT AND EDUCATION ###

## Rafa: I create a new dataframe (ASR_educ) with the variables pertaining to education and employment ##

ASR_educ <-
  ASRraw %>%
  select (hhid, qn1h, qn1l, qn2a, qn2b, qn3a, qn3b, qn4a, qn4b, qn4c, qn4e, 
          qn4j,qn5a, qn5b,qn5c, qn6a, qn8a, qn8b, qn10a, qn10b, qn11a, qn18c, qn20, qn24a, qn25a, qn25b, qn25c)%>%
  rename ("nationality"="qn1h") %>%  #"What is this person's country of citizenship?
  rename ("refugee"="qn1l") %>%  #"Is this person a refugee who entered between 2011 and 2016?
  rename("yearseduc"="qn2a") %>%   #Years of education
  mutate(yearseduc = ifelse(yearseduc>90, NA, yearseduc)) %>% 
  rename("highcert"="qn2b") %>%     #Highest degree obtained before coming to the U.S.?
  rename("job_type"="qn3a") %>%     #Area of work before coming to the U.S.?
  rename("job_act"="qn3b") %>%      #Type of activity before coming to the U.S.?
  rename("eng_arrival"="qn4a") %>%   #On arrival, how well did the person speak English?
  rename("eng_current"="qn4b") %>%   #How well does the person speak English now?
  rename("eng_edu_pre"="qn4c") %>%   #Language instruction before coming to the U.S.? 
  rename("eng_training"="qn4e") %>%  #English training in the past 12 months?
  rename("eng_training_current"="qn4j") %>%  #Currently enrolled in English language training
  rename("work"="qn5a") %>%  #Person worked at a job anytime last week?
  rename("multiplejobs"="qn5b") %>%  #Person worked more than 1 job anytime last week?
  rename("jobs"="qn5c") %>%  #Number of jobs
  rename("workhours"="qn6a") %>%  #Number of hours  
  rename("pretaxinc"="qn8a") %>%  #PreTax income
  rename("incomebasis"="qn8b") %>%  #On what basis is pre-tax income compiled?
  rename("pretaxinc2"="qn10a") %>%  #Earnings from second job
  rename("incomebasis2"="qn10b") %>%  #On what basis is pre-tax income compiled?
  rename("everworked"="qn11a") %>%  #Has this person worked since arrival to the US?
  rename("incometotal"="qn18c") %>% #Income from all sources
  
  ## Recode some values as missing
  mutate(incometotal= ifelse(incometotal>500000, NA, incometotal)) %>% 
  rename("employer"="qn20") %>% #Is the person working for private, federal, state, local...
  rename("training"="qn24a") %>% #Attended job training in past 12 months?
  rename("school"="qn25a") %>% #Attended school or university in past 12 months?
  rename("schoolfordegree"="qn25b") %>% #"Attending school for degree or certificate?"
  rename("degree"="qn25c") %>%   #"What degree is person attempting to earn?"
  
  mutate(nationality=as_factor(nationality)) %>%
  mutate(job_type=as_factor(job_type)) %>%
  mutate(job_act=as_factor(job_act)) %>% 
  mutate(eng_arrival=as_factor(eng_arrival)) %>% 
  mutate(eng_current=as_factor(eng_current)) %>% 
  mutate(eng_edu_pre=as_factor(eng_edu_pre)) %>%
  mutate(eng_training_current=as_factor(eng_training_current)) %>% 
  mutate(eng_training=as_factor(eng_training)) %>%
  mutate(highcert=as_factor(highcert)) %>% 
  mutate(work=as_factor(work)) %>% 
  mutate(multiplejobs=as_factor(multiplejobs)) %>% 
  mutate(everworked=as_factor(everworked)) %>% 
  mutate(employer=as_factor(employer)) %>% 
  mutate(training=as_factor(training)) %>% 
  mutate(school=as_factor(school)) %>%
  mutate(schoolfordegree=as_factor(schoolfordegree)) %>% 
  mutate(degree=as_factor(degree))%>% 
  mutate(refugee=as_factor(refugee)) %>% 
  
  ## We want only people who responded to the survey that they were refugees
  filter(refugee=="Yes")




##### BUILDING THE USER INTERFACE FOR THE DASHBOARD #######


ui <- dashboardPage(skin="black",
                    dashboardHeader(title="ASR Interactive Dashboard", titleWidth = 350), 
                    dashboardSidebar(width = 350,
                                     sidebarMenu(id="sidebarmenu",
                                       menuItem("Home", tabName = "dashboard", icon=icon("home")),
                                       
                                       
                                       menuItem("Widgets", tabName = "widgets"),
                                       
       #### Steffi, Diya: Follow this example to include more items and subitems. Keep track of the , and ()
      ####  See how the name of each item is then used later to create an action, either uiOutput or plotOutput
      ## which is then coded on the server side
       
                                       menuItem("Employment and Education", startExpanded = TRUE,
                                                menuSubItem("Schooling", tabName = "schooling", icon=icon("graduation-cap")),
                                                menuSubItem("Employment", tabName = "english_arrival", icon=icon("building"))),
      
                                       menuItem("About the project", tabName = "about", icon=icon("info-circle"))
                                       
                                       
                                       
                                     ),
                                     textOutput("res")
                                     
  
                                     
                                     
                    ),
                    dashboardBody(
                      
                      conditionalPanel(
                        condition = "input.sidebarmenu == 'schooling'",
                        selectInput(
                          "breaks", "Breaks",
                          names(ASR_educ))),
                      
                      tabItems(
                        tabItem("dashboard", uiOutput("home")),
                        tabItem("widgets", "Widgets tab content"),
                        tabItem("schooling", plotOutput("school", )),
                        tabItem("english_arrival", plotOutput("english_arrival")),
                        
                        uiOutput("text2")),
                
                        
                        fluidRow(
                        
                        hr(),
                        tags$footer("Refugee Dashboard - All Rights Reserved", align="center"))
                      )
                    )


server <- function(input, output, session) {

  ### This is the home screen ###  
  
  output$home <- renderUI({
    
    fluidPage(
      
      
      
      tags$h1("What does the data tell us about America's refugees?"),
      
      tags$img(src="refugee_cover.jpg", width=500, height=300),
      
      hr(),
      
      
      strong(
    "We propose exploring the relationship between the
      success of refugees resettled in the United States and 
    the characteristics of the communities that receive them. 
    For this purpose, we are interested in analyzing data from 
    the most recent Annual Survey of Refugees (ASR 2016) 
    as well as datasets from the United States Census Bureau. 
    The ASR data would allow us to paint a picture of the refugees’ 
    progress in the country in terms of variables such as English language 
    learning and labor force participation, 
    thus identifying patterns of success or challenges
    for specific communities across the country.")
    )    
  }) 
  
  
  ### Steffi, Diya: Use this example to code for your graph based on the tab names you specified above.  
  
  output$english_arrival <- renderPlot({
    #On arrival, how well did the person speak English?
    ggplot(data=subset(ASR_educ, !is.na(eng_arrival)), aes(x = as.factor(eng_arrival))) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), width=.5, fill = "steelblue") +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", hjust=-.1) +
      scale_y_continuous(labels = percent) +
      coord_flip()+
      labs(title = "On arrival, how well did the person speak English?", x ="Proficiency", y = "Percent")
  })
  
  

  output$employer <- renderPlot({
    ggplot(data=subset(ASR_educ, !is.na(employer)), aes(x = as.factor(employer))) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), width=.5, fill = "steelblue") +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", hjust=-.1) +
      scale_y_continuous(labels = percent) +
      coord_flip()+
      labs(title = "Who is the main employer?", x = "Industry", y = "Percent")
  })   
  
  output$school <- renderPlot({
    ggplot(data=subset(ASR_educ, !is.na(eval(as.name(input$breaks)))), aes(x = as.factor(eval(as.name(input$breaks))))) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), width=.5, fill = "steelblue") +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", hjust=-.1) +
      scale_y_continuous(labels = percent) +
      coord_flip()+
      labs(title = eval(as.character(input$breaks)), x=eval(as.character(input$breaks)), y = "Percent")
  })
  
  output$text2 <- renderUI({
    
    if (input$breaks=="school") {
      
      fluidPage(
      
        br(),
        
        strong("We can see how the following description presents an important measure of refugee welfare")
      )}
    
    ##### INSERT NEW CATEGORIES FOR TEXT HERE #######
    
    
    
    
    
  })
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)

## Code to deploy:

rsconnect::deployApp("Dashboards/")


