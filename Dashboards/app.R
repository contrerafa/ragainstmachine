## McCourt School of Public Policy - Georgetown University ####
## Intro to Data Science - Spring 2019 ##
## Team: R against the Machine #

# Steffi, Diya, and Rafael ##
#Last update: 8:39pm

# Loading require libraries #
require(recipes)
require(tidyverse)
require(pdp)
require(utf8)
require(skimr)
require(foreign)
require(readr)
library(haven)
require(scales)
require(shiny)
require(shinydashboard)
require(shinythemes)
require(ggplot2)
require(maps)

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


options_education<-c("Years of education prior to arrival", "Highest certificate before arriving to the U.S", "Attended training in past 12 months", "Attended school or university in past 12 months",
                    "If in school, what degree are you pursuing?") 
                    

ASR_educ2 <-
  ASR_educ %>% 
  filter (!is.na(yearseduc)) %>% 
  filter (!is.na(nationality)) %>% 
  filter (nationality!=c("United States","Other", "None", "Refused"))



### BENEFITS ###

#### Steffi: I create a new dataframe (ASR_ben) with the variables in which I am interested (benefits) ##
ASR_ben <-
  ASRraw %>%
  select (qn30a, qn31a, qn32a, qn33a, qn34a)%>%
  rename ("food"="qn30a") %>%  
  rename ("tanf"="qn31a") %>% 
  rename ("rca"="qn32a") %>% 
  rename ("ssi"="qn33a") %>% 
  rename ("ga"="qn34a")

####Demographics###
## Diya: I create a new dataframe with the variables of interest to demographics ##

df <- ASRraw
data <- data.frame("age"=df$qn1d,"gender"=df$qn1f,"country of birth"=df$qn1g,"State originally resettle"=df$qn1k,"country of citizenship"=df$qn1h,"ethnic origin"=df$qn1i)
name <- c("age","gender","C_O_B","S_O_R","C_O_C","ethic_origin")
names(data)<-name


#group age data
data <- data %>% mutate(age_group =
                          case_when(
                            age <= 15 ~ "0-15",
                            age <= 30 ~ "16-30",
                            age <= 45 ~ "31-45",
                            age <= 60 ~ "46-60",
                            age <= 75 ~ "61-75") 
)
#drop unreasonable data
data_filt <- data[data$age<=75,]
#The age and gender distribution for refugees


##### BUILDING THE USER INTERFACE FOR THE DASHBOARD #######

ui <- dashboardPage(skin="black",
                    dashboardHeader(title="ASR Interactive Dashboard", titleWidth = 350), 
                    dashboardSidebar(width = 350,sidebarMenu(id="feeder", 
                                                             
                                       menuItem("Home", tabName = "dashboard", icon=icon("home")),
                                       
       #### Steffi, Diya: Follow this example to include more items and subitems. Keep track of the , and ()
      ####  See how the name of each item is then used later to create an action, either uiOutput or plotOutput
      ## which is then coded on the server side
       
      
                                    menuItem("Demographics Overview", startExpanded = TRUE,
                                    menuSubItem("Age and Gender", tabName = "age_gender", icon=icon("user-check")),
                                    menuSubItem("Country of Origin", tabName = "country_origin", icon=icon("passport")),
                                    menuSubItem("State of Resettlement", tabName = "state_resettle", icon=icon("plane-arrival"))),
                                    
      
                                       menuItem("Employment and Education", startExpanded = TRUE,
                                                menuSubItem("Education & Training", tabName = "education", icon=icon("graduation-cap")),
                                                menuSubItem("Employment", tabName = "employment", icon=icon("briefcase"))),
      
                                     menuItem("Benefits", startExpanded = TRUE,
                                     menuSubItem("SNAP", tabName = "food", icon=icon("apple-alt")),
                                     menuSubItem("TANF", tabName = "tanf", icon=icon("money-check-alt")),
                                     menuSubItem("RCA", tabName = "rca", icon=icon("money-check-alt")),
                                     menuSubItem("SSI", tabName = "ssi", icon=icon("money-check-alt")),
                                     menuSubItem("GA", tabName = "ga", icon=icon("money-check-alt"))),
                                      
                                    menuItem("About the project", tabName = "about", icon=icon("info-circle"))
                                     )
                    ),
      
                    dashboardBody(
                      
                      conditionalPanel(
                        condition = "input.feeder == 'education'",
                        selectInput("edu_outcomes", "Select the education outcome of interest:", 
                                    c("Highest certificate before arriving to the U.S" = "highcert",
                                      "Years of education prior to arrival" = "yearseduc",
                                      "Has this person attended school or university in past 12 months?"="school",
                                      "What degree is the person attempting to earn?"="degree",
                                      "On arrival, how well did the person speak English?" = "eng_arrival",
                                      "How well does the person speak English now?" = "eng_current",
                                      "Did the person receive language instruction before coming to the U.S.?" ="eng_edu_pre",
                                      "Is the person currently enrolled in English language training?" ="eng_training_current"))),
                                  
                                    conditionalPanel(
                                      condition = "input.feeder == 'employment'",
                                      selectInput("job_outcomes", "Select the employment outcome of interest:", 
                                                  c("Did the person work at a job anytime last week?" = "work",
                                                    "Has this person worked since arrival to the US?" = "everworked",
                                                    "For what kind of employer is this person working?" = "employer",
                                                    "Has this person attended job training in past 12 months?" = "training",
                                                    "What was this person's income from all sources?"="incometotal")                
                                    
                                    
                                    )),
                      
                      tabItems(
                        tabItem("dashboard", uiOutput("home")),
                        tabItem("education", plotOutput("education")),
                        tabItem("employment", plotOutput("employment")),
                        tabItem("food", plotOutput("food")),
                        tabItem("tanf", plotOutput("tanf")),
                        tabItem("rca", plotOutput("rca")),
                        tabItem("ssi", plotOutput("ssi")),
                        tabItem("ga", plotOutput("ga")),
                        tabItem("age_gender", plotOutput("age_gender")),
                        tabItem("country_origin", plotOutput("country_origin")),
                        tabItem("state_resettle", plotOutput("state_resettle")),
                        
                        uiOutput("text2")),
                
                        
                        fluidRow(
                        
                        hr(),
                        tags$footer("Refugee Dashboard - All Rights Reserved", align="center"))
                      )
                    )

#### BUILDING THE SERVER SIDE OF THINGS ####

server <- function(input, output, session){

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
        the most recent Annual Survey of Refugees (ASR 2016) to paint a picture of the refugees’ 
        progress in the country in terms of variables such as English language 
        learning and labor force participation, 
        thus identifying patterns of success or challenges
        for specific communities across the country.")
      )    
  }) 
  
output$age_gender <- renderPlot({
ggplot(data_filt, aes(x = age,fill=age_group)) +
geom_histogram(aes(y=..density..))+
facet_wrap(.~gender,scales="free")})



 # The country of origin distribution


output$country_origin <- renderPlot({ 
   
world_map = map_data("world")
cobnumber <- data %>% group_by(C_O_B) %>% summarize(count = n())
colnames(cobnumber)[colnames(cobnumber)=="C_O_B"] <- "region"
C_O_B.map <- right_join(cobnumber, world_map, by = "region")
   
ggplot(data = C_O_B.map, aes(x = long, y = lat, group = group)) +
 geom_polygon(aes(fill = C_O_B.map$count))}) 
    
regionnumber <- data %>% group_by(S_O_R) %>% summarize(count = n())
colnames(regionnumber)[colnames(regionnumber)=="S_O_R"] <- "area"
regionnumber$area[regionnumber$area==1] <- "Northeast"
regionnumber$area[regionnumber$area==2] <- "South"
regionnumber$area[regionnumber$area==3] <- "North Central"
regionnumber$area[regionnumber$area==4] <- "West"  
 
 #The state of resettlement distribution

output$state_resettle <- renderPlot({ 
ggplot2_states <- map_data("state")
ggplot2_states$region <- str_to_title(ggplot2_states$region)
region_data <- data.frame(region=state.name,area = state.region)
ggplot2_statesdata <- inner_join(ggplot2_states,region_data,"region")
ggplot2_statesdata <- inner_join(ggplot2_statesdata,regionnumber,"area")

g1 <- ggplot(data = ggplot2_statesdata,
             mapping = aes(x = long, y = lat, group = group,fill=count))+
  scale_fill_gradient(low = "#330000", high = "#FFFFFF") +
  geom_polygon(color="gray90",size=0.1)
g1})
     
  
  output$employment <- renderPlot({
    
    graph<-
      ggplot(data=subset(ASR_educ, !is.na(eval(as.name(input$job_outcomes)))), aes(x = as.factor(eval(as.name(input$job_outcomes))))) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), width=.5, fill = "steelblue") +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", hjust=-.1) +
      scale_y_continuous(labels = percent) +
      coord_flip()
    
  
  if (input$job_outcomes=="work") {
    graph <- graph+labs(title ="Did the person work at a job anytime last week?",
                        x="Worked", 
                        y = "Percent",
                        caption = "We can see how the following description presents an important measure of refugee welfare")+
      theme(
        plot.caption = element_text(size=16, hjust=0.5)
      )
    print(graph)
  }
  
    if (input$job_outcomes=="everworked") {
      graph <- graph+labs(title ="Has this person worked since their arrival to the US?",
                          x="Worked", 
                          y = "Percent",
                          caption = "We can see how the following description presents an important measure of refugee welfare")+
        theme(
          plot.caption = element_text(size=16, hjust=0.5)
        )
      print(graph)
    }  
    
    
  if (input$job_outcomes=="employer") {
    graph <- graph+labs(title = "For what kind of employer is this person working?",
                        x="Employer", 
                        y = "Percent",
                        caption = "We can see how the following description presents an important measure of refugee welfare")+
      theme(
        plot.caption = element_text(size=16, hjust=0.5)
      )
    print(graph)
  }

  if (input$job_outcomes=="training") {
    graph <- graph+labs(title = "Has this person attended job training in past 12 months?",
                        x="Training", 
                        y = "Percent",
                        caption = "We can see how the following description presents an important measure of refugee welfare")+
      theme(
        plot.caption = element_text(size=16, hjust=0.5)
      )
    print(graph)
  }
  
  if (input$job_outcomes=="incometotal") {
    graph <-ggplot(ASR_educ2, aes(incometotal))+geom_density(aes(fill="tomato3"), alpha=0.5) + 
      labs(title="Density plot", 
           subtitle="Reported income from all sources",
           caption = "We can see how the following description presents an important measure of refugee welfare",
           x="Income total (in US$)")+
      scale_y_continuous() +
      theme(plot.caption = element_text(size=16, hjust=0.5), axis.title.y=element_blank())
    print(graph)
  }
  
  }) 
  
  
  output$education <- renderPlot({
  
    graph<-
      ggplot(data=subset(ASR_educ, !is.na(eval(as.name(input$edu_outcomes)))), aes(x = as.factor(eval(as.name(input$edu_outcomes))))) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), width=.5, fill = "steelblue") +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", hjust=-.1) +
      scale_y_continuous(labels = percent) +
      coord_flip()
      
    if (input$edu_outcomes=="yearseduc") {
  
      graph <- ggplot(ASR_educ, aes(yearseduc))+
        geom_density(aes(fill="tomato3"), alpha=0.5) + 
        labs(title="Years of education prior to arrival", 
             subtitle="Years of education reported",
             caption="We can see how the following description presents an important measure of refugee welfare",
             x="Years of education")+
        theme_classic()+theme(plot.caption = element_text(size=16, hjust=0.5))
      print(graph)
    }

    if (input$edu_outcomes=="highcert") {
     graph <- graph+labs(title ="What was the highest certificate you obtained prior to arriving to the U.S?",
                         x="Certificate", 
                         y = "Percent",
                         caption = "We can see how the following description presents an important measure of refugee welfare")+
       theme(
         plot.caption = element_text(size=16, hjust=0.5)
       )
     print(graph)
    }
     
    if (input$edu_outcomes=="school") {
      graph <- graph+labs(title ="Has this person attended school or university in past 12 months?",
                          x="Attended school", 
                          y = "Percent",
                          caption = "We can see how the following description presents an important measure of refugee welfare")+
        theme(
          plot.caption = element_text(size=16, hjust=0.5)
        )
      print(graph)
    }
    

    if (input$edu_outcomes=="degree") {
      graph <- graph+labs(title ="What degree is the person attempting to earn?",
                          x="English proficiency", 
                          y = "Percent",
                          caption = "We can see how the following description presents an important measure of refugee welfare")+
        theme(
          plot.caption = element_text(size=16, hjust=0.5)
        )
      print(graph)
    }
    
    
    if (input$edu_outcomes=="eng_arrival") {
      graph <- graph+labs(title ="On arrival, how well did the person speak English?",
                          x="English proficiency", 
                          y = "Percent",
                          caption = "We can see how the following description presents an important measure of refugee welfare")+
        theme(
          plot.caption = element_text(size=16, hjust=0.5)
        )
      print(graph)
    }
    
    if (input$edu_outcomes=="eng_current") {
      graph <- graph+labs(title ="How well does the person speak English now?",
                          x="English proficiency", 
                          y = "Percent",
                          caption = "We can see how the following description presents an important measure of refugee welfare")+
        theme(
          plot.caption = element_text(size=16, hjust=0.5)
        )
      print(graph)
    }
    
    if (input$edu_outcomes=="eng_edu_pre") {
      graph <- graph+labs(title ="Did the person receive language instruction before coming to the U.S.?",
                          x="Received training", 
                          y = "Percent",
                          caption = "We can see how the following description presents an important measure of refugee welfare")+
        theme(
          plot.caption = element_text(size=16, hjust=0.5)
        )
      print(graph)
    }
    
    if (input$edu_outcomes=="eng_training_current") {
      graph <- graph+labs(title = "Is the person currently enrolled in English language training?",
                          x="Enrolled", 
                          y = "Percent",
                          caption = "We can see how the following description presents an important measure of refugee welfare")+
        theme(
          plot.caption = element_text(size=16, hjust=0.5)
        )
      print(graph)
    }
    
    
  })

  output$text2 <- renderUI({
    if (input$edu_outcomes=="school") {
      fluidPage(
        br(),
        strong("We can see how the following description presents an important measure of refugee welfare"))}
  })

    output$rca <- renderPlot({
      ggplot(ASR_ben, aes(x = as.factor(rca))) +
        labs(title = "RCA") +
        geom_bar(width=.5, fill = "steelblue") +
        coord_flip() +
        scale_x_discrete(labels=c("1" = "No", "2" = "Yes",
                                  "8" = "Don't know", "9" = "Refused")) +
        xlab("") +
        ylab("")
    })
    
    output$ssi <- renderPlot({
      ggplot(ASR_ben, aes(x = as.factor(ssi))) +
        labs(title = "SSI") +
        geom_bar(width=.5, fill = "steelblue") +
        coord_flip() +
        scale_x_discrete(labels=c("1" = "No", "2" = "Yes",
                                  "8" = "Don't know", "9" = "Refused")) + 
        xlab("") +
        ylab("")
    })
    
    output$ga <- renderPlot({
      ggplot(ASR_ben, aes(x = as.factor(ga))) +
        xlim("No", "Yes") +
        labs(title = "In the past 12 months, have you received cash assistance through the General Assistance (GA) Program?") +
        geom_bar(width=.5, fill = "steelblue") +
        coord_flip() +
        scale_x_discrete(labels=c("1" = "No", "2" = "Yes",
                                  "8" = "Don't know", "9" = "Refused")) +
        xlab("") +
        ylab("")
    })


  output$food <- renderPlot({
    ggplot(ASR_ben, aes(x = as.factor(food))) +
      labs(title = "In the past 12 months, have you received food stamps?") +
      geom_bar(position='dodge', width=.5, fill = "steelblue") +
      coord_flip() +
      scale_x_discrete(labels=c("1" = "No", "2" = "Yes",
                                "8" = "Don't know", "9" = "Refused")) +
      xlab("") +
      ylab("")
  })
  
  output$tanf <- renderPlot({
    ggplot(ASR_ben, aes(x = as.factor(tanf))) +
      labs(title = "In the past 12 months, have you received cash assistance through the Temporary Assistance to Needy Families (TANF) Program?") +
      geom_bar(width=.5, fill = "steelblue") +
      coord_flip() +
      scale_x_discrete(labels=c("1" = "No", "2" = "Yes",
                                "8" = "Don't know", "9" = "Refused")) +
      xlab("") +
      ylab("")
  })
  
  output$rca <- renderPlot({
    ggplot(ASR_ben, aes(x = as.factor(rca))) +
      labs(title = "In the past 12 months, have you received assistance through the Refugee Cash Assistance (RCA) program?") +
      geom_bar(width=.5, fill = "steelblue") +
      coord_flip() +
      scale_x_discrete(labels=c("1" = "No", "2" = "Yes",
                                "8" = "Don't know", "9" = "Refused")) +
      xlab("") +
      ylab("")
  })
  
  output$ssi <- renderPlot({
    ggplot(ASR_ben, aes(x = as.factor(ssi))) +
      labs(title = "In the past 12 months, have you received Supplemental Security Income (SSI)?") +
      geom_bar(width=.5, fill = "steelblue") +
      coord_flip() +
      scale_x_discrete(labels=c("1" = "No", "2" = "Yes",
                                "8" = "Don't know", "9" = "Refused")) + 
      xlab("") +
      ylab("")
  })
  
  output$ga <- renderPlot({
    ggplot(ASR_ben, aes(x = as.factor(ga))) +
      labs(title = "In the past 12 months, have one you received income from General Assistance (GA)?") +
      geom_bar(width=.5, fill = "steelblue") +
      coord_flip() +
      scale_x_discrete(labels=c("1" = "No", "2" = "Yes",
                                "8" = "Don't know", "9" = "Refused")) +
      xlab("") +
      ylab("")
  }) 
}


shinyApp(ui = ui, server = server)


## Code to deploy:
#rsconnect::deployApp("Dashboards/")

 
