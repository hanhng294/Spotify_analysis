library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggdark)
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyverse)
library('dplyr')
library("readxl")
library('ggplot2')
library(png)
#setwd('C:/Users/thuch/OneDrive/Desktop/DATA5002/assignment2')
spotify<- read.csv("Spotify_data.csv")
spotify['pod_variety_satisfaction']<-spotify %>%select(pod_variety_satisfaction)%>%mutate_all(~case_when(. == "Very Dissatisfied" ~ 1, 
                                                                                                         . == "Dissatisfied" ~ 2, 
                                                                                                         . == "Ok" ~ 3,
                                                                                                         .=='Satisfied'~4,
                                                                                                         .=='Very Satisfied'~5))
prem_willing <- spotify[spotify['premium_sub_willingness']=='Yes',]
gender_w<- prem_willing %>%
  group_by(Gender) %>%
  summarise(count =n()*100/nrow(prem_willing))
fav_pod_genre<-spotify %>%
  group_by(fav_pod_genre) %>%
  summarise(count =n())%>%arrange(desc(count))
pod5<-fav_pod_genre[2:6,]['fav_pod_genre']
pod_top5<-spotify%>%
  filter(fav_pod_genre %in% c('Comedy','Lifestyle and Health','Health and Fitness','Sports','Food and cooking'))%>%
  group_by(fav_pod_genre,pod_variety_satisfaction)%>%
  summarise(count =n())%>%arrange(desc(count))
fav_mus_genre<-spotify %>%
  group_by(fav_music_genre) %>%
  summarise(count =n())%>%arrange(desc(count))

mus5<-list(fav_mus_genre[1:5,]['fav_music_genre'])
music_top5<-spotify%>%
  filter(fav_music_genre %in% c('Melody','classical','Pop','Rap','Electronic/Dance'))%>%
  group_by(fav_music_genre,music_recc_rating)%>%
  summarise(count =n())%>%arrange(desc(count))

ui <-  shinyUI(
  fluidPage(
    setBackgroundColor(
      color = c("black", "black"),
      gradient = "radial",
      direction = c("top", "left")
    ),
  tags$style(type = 'text/css','a{color:black;}','h1,h3{color:green}','.box{background-color:green;color:white;}','.box-title{font-weight:bold;font-size :40px;color:white}','p{color:white;font-size:17px;}',
        '.navbar {
               text-align:center ; 
               color:black;
               font-family: Arial;
               font-size: 20px;
               background-color:green;
               }','.navbar-default .navbar-brand {
                         color: black; font-style:bold;}','.navbar-default .navbar-nav li a{color: black; font-weight:bold;}'),
            
            navbarPage(title="App",
                          tabPanel("User-Characteristics",
                                   fluidRow(
                                     imageOutput("spotify",height = "40px"),
                                     h1("User Characteristics")),
                                   column(6,
                                   fluidRow(align = "center",
                                     box(title = paste(round(nrow(spotify[spotify['spotify_subscription_plan']=='Premium (paid subscription)',])*100/nrow(spotify)),'%'),
                                         p ("Premium Plan users"), width = 5),
                                     box(title = paste(round(nrow(spotify[spotify['premium_sub_willingness']=='Yes',])*100/nrow(spotify)),'%'),
                                         p ("Willing for Premium"), width = 5),
                                     tags$script(HTML("$('.box').css('border', '2px solid black';"))),
                                   fluidRow(
                                     br(),
                                     column(6,
                                            tags$style("#userinfo-label {color:white;font-size: 17px;}"),
                                            selectInput("userinfo", label = "Select User Characteristic", choices = c('Spotify Usage Period'='spotify_usage_period','Age'='Age','Gender')))
                                   ),
                                   fluidRow(
                                     br(),
                                     column(11,plotlyOutput("user_plot"))
                                   )
                                   ),
                                   
                                   column(6,
                                     fluidRow(
                                        h3("Potential Premium User"),
                                        br(),
                                         column(6,imageOutput("willinggen")),
                                         column(6,plotlyOutput("willingage"))
                                     ),
                                     fluidRow(
                                       column(6,
                                              tags$style("#mp-label {color:white;font-size: 17px;}"),
                                              selectInput("mp", label = "Select Music or Podcast", choices = c("Music"='fav_music_genre',"Podcast"='fav_pod_genre'))),
                                       column(11,plotlyOutput("spotify_plot"))
                                     )
                                   )

                          ),
                          
                          tabPanel("User-Preference",
                                   fluidRow(
                                     imageOutput("spotify2",height = "40px"),
                                     h1("User Preference and Rating")
                                   ),
                                   br(),
                                   column(6,
                                   fluidRow(
                                     br(),
                                     column(6,p("Music Recommendation Ratings:")),
                                     column(6,imageOutput("photo",height = "80px"))),
                                   fluidRow(
                                     column(6,p("Podcast Variety Ratings: ")),
                                     column(6,imageOutput("scale",height = "80px"))
                                     ),
                                   fluidRow(
                                     column(6,
                                            tags$style("#mp2-label {color:white;font-size: 17px;}"),
                                            selectInput("mp2", label = "Select Music or Podcast", choices = c('Podcast'='pod_top5','Music' ='music_top5')))
                                   ),
                                   fluidRow(
                                     column(12,
                                     br(),
                                     plotlyOutput("podmusicrating"))
                                   )
                                   ),
                                   column(6,align = "center",
                                   fluidRow(
                                     box(title = nrow(spotify[spotify['pod_lis_frequency']=='Daily',]),
                                         p("Daily listeners"),width = 5),
                                     box(title = paste(round(nrow(spotify[spotify['pod_variety_satisfaction']>3,])*100/nrow(spotify)),'%'),
                                         p("Satisfied with Variety"), width = 5),
                                     tags$script(HTML("$('.box').css('border', '2px solid black');"))
                                     ),
                                   fluidRow(
                                     br(),
                                     br(),
                                     column(10,plotOutput("PMGenre"))
                                   ),
                                   fluidRow(
                                     column(12,plotlyOutput("PMpref"))
                                   )
                                   )
                                )
                       )
            )
  )

server <- function(input, output, session) {
  output$premiumprop <- renderPrint({round(nrow(spotify[spotify['spotify_subscription_plan']=='Premium (paid subscription)',])*100/nrow(spotify))})
  output$willingprop <- renderPrint({round(nrow(spotify[spotify['premium_sub_willingness']=='Yes',])*100/nrow(spotify))})
  u_title<- reactive({
    if(input$userinfo=="spotify_usage_period"){
      "Usage Period"
    }
    else{
      input$userinfo
    }
  })
  output$user_plot<- renderPlotly(
    ggplotly(height =700,width = 600,ggplot(spotify, aes_string(x = 'preffered_premium_plan', fill = input$userinfo)) + geom_bar(position = "dodge")+ labs(title = paste("Premium Plan Preference by",u_title()))+ dark_theme_gray() +theme(axis.text.x = element_text(angle = 45), axis.title.y = element_blank()))
  )

  output$spotify_plot <-renderPlotly({
    ggplotly(width = 600,ggplot(spotify, aes_string(y = input$mp, fill = input$mp)) + geom_bar()+labs(title = "Genre Distribution") +dark_theme_gray()+ theme(legend.position = "none") +theme(axis.title = element_blank()))
  })
  output$willinggen <- renderImage(
    list(
      src = "plot1.png",
      contentType = "image/png",
      width = 300
    ))
  
  #renderPlot(
  #ggplot(gender_w, aes(x = "", y = count, fill = Gender)) + geom_col() + coord_polar(theta = "y",start = 0)+geom_text(aes(label = paste(round(count,2),'%')), position = position_stack(vjust = 0.5))+dark_theme_gray()+theme(panel.background = element_rect(fill = "black"),panel.grid = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(), axis.text = element_blank()) +labs(title = "By Gender"),height = 300)
  
  output$willingage <- renderPlotly(
    ggplotly(height = 300,width = 400,ggplot(spotify[spotify['premium_sub_willingness']=='Yes',], aes_string(x = "Age",fill = 'Age')) + geom_bar()+labs(title = "By Age")+dark_theme_gray()+theme(axis.title.y = element_blank(), legend.position = "none" ))
  )
  
  pieplot<- reactive({
    if(input$mp2=="pod_top5"){
      "plot2_1.png"
    }
    else{
      "plot2_2.png"
    }
  })
  
  output$PMGenre <- renderImage(
    list(
      src = pieplot(),
      contentType = "image/png",
      width = 500
    ))
    
    #renderPlot(
    #ggplot(get(input$mp2)%>%group_by_at(vars(colnames(get(input$mp2))[1]))%>%summarise(sum = sum(count)*100/nrow(spotify)), aes_string(x = 3, y = "sum", fill = colnames(get(input$mp2))[1])) + geom_col(color = "black") + coord_polar(theta = "y",start = 0)+ xlim(c(0.2, 3.5))+geom_text(aes(label = paste(round(sum,2),'%')), position = position_stack(vjust = 0.5))+dark_theme_gray()+theme(panel.background = element_rect(fill = "black"),plot.background = element_rect(fill = "black"),legend.background = element_rect(fill = "black"),panel.grid = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(), axis.text = element_blank()),width = 400, height = 300)
  
    format<- reactive({
    if(input$mp2=="pod_top5"){
      "preffered_pod_duration"
    }
    else{
      "music_time_slot"
    }
  })
  title<- reactive({
    if(input$mp2=="pod_top5"){
      "Preferred Podcast Duration"
    }
    else{
      "Preferred Music Time Slot"
    }
  })
  output$PMpref <- renderPlotly(
    ggplotly(height = 350,ggplot(spotify, aes_string(y = format(), fill = format()))+geom_bar(position = "dodge",color = "black")+labs(title = title())+dark_theme_gray()+theme(axis.title = element_blank()))
  )
  output$podmusicrating<-renderPlotly({
    ggplotly(height = 600,ggplot(get(input$mp2), aes_string(x = colnames(get(input$mp2))[2],y = "count",col = colnames(get(input$mp2))[1]))+geom_line()+labs(title = "Ratings for Top 5")+dark_theme_gray())
  })
  output$photo <- renderImage(
    list(
      src = "stars.png",
      contentType = "image/png",
      width = 150
    ))
  output$scale <- renderImage(
    list(
      src = "scale.png",
      contentType = "image/png",
      width = 180
    ))
  output$spotify <- renderImage(
    list(
      src = "spotify.png",
      contentType = "image/png",
      width = 150
    ))
  output$spotify2 <- renderImage(
    list(
      src = "spotify.png",
      contentType = "image/png",
      width = 150
    ))
}

shinyApp(ui, server)
