#Installing Packages
# install.packages('shinydashboard')
# install.packages('highcharter')
# install.packages('dplyr')
# install.packages('shiny')
# install.packages('waterfalls')
# install.packages('ggplot2')
require(highcharter)
require(dplyr)
library(waterfalls)
library(shiny)
library(shinydashboard)
library(ggplot2)
#install.packages("partykit")
library(partykit)
#install.packages("party")
library('party')
#install.packages("CHAID", repos="http://R-Forge.R-project.org") 
library(CHAID)

setwd("C:/Users/shruti.sairaj/Desktop/Shruti/Feb/Modelling_Deepdive/RShiny/13thMar")
#getwd()


region_values = unique(concepts$Region)
driver_values=unique(M_Scores$driver)
market_values  = unique(concepts$Market_number[concepts$Region == region_values[1]])

ui<-
  dashboardPage(
    
    dashboardHeader(title="Deep Dive"),
    dashboardSidebar(
      #id="tabs",
      sidebarMenu( id="tabs",
                   #menuItem("Instructions" , tabName = "Dashboard" ),
                   menuItem("Regional Level Summary" , tabName = "Region" ),
                   menuItem("Market Scores",  tabName = "Market_scores" ),
                   menuItem("Market Level Summary",  tabName = "Market" ),
                   menuItem("Store Level Summary",  tabName = "Store"),
                   menuItem("Openends",  tabName = "Openend")),
      title = "Controls",
      selectInput(inputId = "region",
                  label = "Region:",
                  choices = region_values,selected=0),
      selectInput(inputId = "tree",
                  label = "No of Trees:",
                  choices = c(3, 4, 5, 6),
                  selected = 3)
      
    ),
    
    dashboardBody(
      tabItems(
        tabItem( tabName="Region",
                 
                 fluidRow(
                   
                   (plotOutput("regionplot"))
                 )
        ),
        tabItem( tabName="Market_scores",
                 selectInput(inputId = "Driver",
                             label = "Select Driver:",
                             choices= as.vector(unique(M_Scores$driver)),selected=0),
                 
                 fluidRow(
                   
                   (dataTableOutput("market_driver_scores_responses")))
        ),				   
        tabItem( tabName="Market",
                 
                 selectInput(inputId = "Markets",
                             label = "Market:",
                             choices = unique(concepts$Market_number[concepts$Region == region_values[1]])),
                 
                 fluidRow(
                   
                   (plotOutput("rmplot"))))
        ,
        tabItem( tabName="Store",
                 selectInput(inputId = "store",
                             label = "Storenum:",
                             choices = unique(concepts$storenum[concepts$Market_number == market_values[1]])),
                 fluidRow(
                   
                   (plotOutput("msplot")))
        ),
        tabItem( tabName="Openend",
                 selectInput(inputId = "Con",
                             label = "Concepts:",
                             choices = as.vector(unique(as.character(openend$Concept)))),
                 selectInput(inputId = "Sto",
                             label = "store:",
                             choices = as.vector(unique(as.character(openend$storenum)))),
                 fluidRow(
                   
                   (dataTableOutput("openendresult")))
        ))
      
    ))



server <- function(input, output, session){
  
  #Reading the Rawdata
  concepts<-read.csv("Rawdata_2.csv")
  openend <- read.csv("Chaid_Rawdata.csv")
  M_Scores <- read.csv("Market_scores.csv")
  
  
  #concepts <- read.csv("Rawdata_2.csv")
  
  observeEvent(
    {
      input$region  ####code will run for changes in any variable that you pass here
    },
    {
      updateSelectInput(
        session,
        inputId = "Markets",
        label = "Market:",
        choices = unique(concepts$Market_number[concepts$Region == input$region]),
        selected = 0
        
      )}
  )
  
  observeEvent(
    {
      input$Markets  ####code will run for changes in any variable that you pass here
    },
    {
      updateSelectInput(
        session,
        inputId = "store",
        label = "Storenum:",
        choices = sort(unique(concepts$storenum[concepts$Market_number == input$Markets])),
        selected = 0
        
      )}
  )
  
  
  #Converting the Columns to Factors to run chaid model
  concepts$PD_Flag<-factor(concepts$PD_Flag)
  concepts$Market_number<-factor(concepts$Market_number)
  concepts$storenum<-factor(concepts$storenum)
  concepts$ga_bucket<-factor(concepts$ga_bucket)
  concepts$concept_bag<-factor(concepts$concept_bag)
  concepts$concept_cart<-factor(concepts$concept_cart)
  concepts$concept_thanks<-factor(concepts$concept_thanks)
  concepts$concept_trip<-factor(concepts$concept_trip)
  concepts$concept_price<-factor(concepts$concept_price)
  concepts$concept_order<-factor(concepts$concept_order)
  concepts$concept_app<-factor(concepts$concept_app)
  concepts$concept_large<-factor(concepts$concept_large)
  concepts$concept_easy<-factor(concepts$concept_easy)
  concepts$concept_rude<-factor(concepts$concept_rude)
  concepts$concept_selection<-factor(concepts$concept_selection)
  concepts$concept_slow<-factor(concepts$concept_slow)
  concepts$concept_registration<-factor(concepts$concept_registration)
  concepts$concept_items<-factor(concepts$concept_items)
  concepts$concept_groceries<-factor(concepts$concept_groceries)
  concepts$concept_self_checkout<-factor(concepts$concept_self_checkout)
  concepts$concept_checkout_lines<-factor(concepts$concept_checkout_lines)
  concepts$concept_scan<-factor(concepts$concept_scan)
  concepts$concept_long<-factor(concepts$concept_long)
  concepts$concept_low<-factor(concepts$concept_low)
  concepts$concept_walmart_pay<-factor(concepts$concept_walmart_pay)
  concepts$concept_place<-factor(concepts$concept_place)
  concepts$concept_workers<-factor(concepts$concept_workers)
  concepts$concept_product<-factor(concepts$concept_product)
  concepts$concept_manager<-factor(concepts$concept_manager)
  concepts$concept_efficient<-factor(concepts$concept_efficient)
  concepts$concept_staff<-factor(concepts$concept_staff)
  concepts$concept_aisles<-factor(concepts$concept_aisles)
  concepts$concept_people<-factor(concepts$concept_people)
  concepts$concept_help<-factor(concepts$concept_help)
  concepts$concept_fast<-factor(concepts$concept_fast)
  concepts$concept_open<-factor(concepts$concept_open)
  concepts$concept_return<-factor(concepts$concept_return)
  concepts$concept_friendly<-factor(concepts$concept_friendly)
  concepts$concept_customer_service<-factor(concepts$concept_customer_service)
  concepts$concept_available<-factor(concepts$concept_available)
  concepts$concept_check_out<-factor(concepts$concept_check_out)
  concepts$concept_stocked<-factor(concepts$concept_stocked)
  concepts$concept_stock<-factor(concepts$concept_stock)
  concepts$concept_day<-factor(concepts$concept_day)
  concepts$concept_pay<-factor(concepts$concept_pay)
  concepts$concept_check<-factor(concepts$concept_check)
  concepts$concept_list<-factor(concepts$concept_list)
  concepts$concept_shelf<-factor(concepts$concept_shelf)
  concepts$concept_convenient<-factor(concepts$concept_convenient)
  concepts$concept_person<-factor(concepts$concept_person)
  concepts$concept_receipt<-factor(concepts$concept_receipt)
  concepts$concept_courteous<-factor(concepts$concept_courteous)
  concepts$concept_easy_to_access<-factor(concepts$concept_easy_to_access)
  concepts$concept_not_working<-factor(concepts$concept_not_working)
  concepts$concept_shelves<-factor(concepts$concept_shelves)
  concepts$concept_well_stocked<-factor(concepts$concept_well_stocked)
  concepts$concept_able_to_find<-factor(concepts$concept_able_to_find)
  concepts$concept_busy<-factor(concepts$concept_busy)
  concepts$concept_lines<-factor(concepts$concept_lines)
  concepts$concept_wait_time<-factor(concepts$concept_wait_time)
  concepts$concept_out_of<-factor(concepts$concept_out_of)
  concepts$concept_purchase<-factor(concepts$concept_purchase)
  concepts$concept_clean<-factor(concepts$concept_clean)
  concepts$concept_functioning<-factor(concepts$concept_functioning)
  concepts$concept_too_long<-factor(concepts$concept_too_long)
  concepts$concept_service<-factor(concepts$concept_service)
  concepts$concept_timely<-factor(concepts$concept_timely)
  concepts$concept_short<-factor(concepts$concept_short)
  concepts$concept_out_of_stock<-factor(concepts$concept_out_of_stock)
  concepts$concept_location<-factor(concepts$concept_location)
  concepts$concept_time<-factor(concepts$concept_time)
  
  #Filtering the data for the Region chosen in the Region Filter
  C<- reactive(concepts[which(concepts$Region==input$region),])
  
  #Setting the control parameters for chaid
  control <- reactive(chaid_control(minbucket = 1, minsplit = 1,minprob=0.01,alpha2=1, alpha4 = .98,maxheight=input$tree) )
  
  
  #Chaid Function
  concepts_y<-reactive({
    chaid(C()$PD_Flag~C()$concept_bag+C()$concept_cart+C()$concept_thanks+C()$concept_trip+C()$concept_price+C()$concept_order+C()$concept_app+C()$concept_large+C()$concept_easy+C()$concept_rude+C()$concept_selection+C()$concept_slow+C()$concept_registration+C()$concept_items+C()$concept_groceries+C()$concept_self_checkout+C()$concept_checkout_lines+C()$concept_scan+C()$concept_long+C()$concept_low+C()$concept_walmart_pay+C()$concept_place+C()$concept_workers+C()$concept_product+C()$concept_manager+C()$concept_efficient+C()$concept_staff+C()$concept_aisles+C()$concept_people+C()$concept_help+C()$concept_fast+C()$concept_open+C()$concept_return+C()$concept_friendly+C()$concept_customer_service+C()$concept_available+C()$concept_check_out+C()$concept_stocked+C()$concept_stock+C()$concept_day+C()$concept_pay+C()$concept_check+C()$concept_list+C()$concept_shelf+C()$concept_convenient+C()$concept_person+C()$concept_receipt+C()$concept_courteous+C()$concept_easy_to_access+C()$concept_not_working+C()$concept_shelves+C()$concept_well_stocked+C()$concept_able_to_find+C()$concept_busy+C()$concept_lines+C()$concept_wait_time+C()$concept_out_of+C()$concept_purchase+C()$concept_clean+C()$concept_functioning+C()$concept_too_long+C()$concept_service+C()$concept_timely+C()$concept_short+C()$concept_out_of_stock+C()$concept_location+C()$concept_time,control=control(),data = C())
  })
  
  #Plotting the output of chaid
  output$regionplot <- renderPlot({
    
    plot(concepts_y())
  })
  
  Markets_scores<- reactive(M_Scores[which(M_Scores$region_description==input$region),])
  
  Markets_scores_driver<- reactive(Markets_scores()[which(Markets_scores()$driver==input$Driver),])
  
  output$market_driver_scores_responses<- renderDataTable(Markets_scores_driver())
  
  C2<- reactive(concepts[which(concepts$Market_number==input$Markets),])
  
  #Setting the control parameters for chaid
  control_rm<- reactive(chaid_control(minbucket = 1, minsplit = 1,minprob=0.0001,alpha2=1, alpha4 = 0.98,maxheight=input$tree))
  
  #Chaid Function
  concepts_z<-reactive({
    
    chaid(C2()$PD_Flag~C2()$concept_bag+C2()$concept_cart+C2()$concept_thanks+C2()$concept_trip+C2()$concept_price+C2()$concept_order+C2()$concept_app+C2()$concept_large+C2()$concept_easy+C2()$concept_rude+C2()$concept_selection+C2()$concept_slow+C2()$concept_registration+C2()$concept_items+C2()$concept_groceries+C2()$concept_self_checkout+C2()$concept_checkout_lines+C2()$concept_scan+C2()$concept_long+C2()$concept_low+C2()$concept_walmart_pay+C2()$concept_place+C2()$concept_workers+C2()$concept_product+C2()$concept_manager+C2()$concept_efficient+C2()$concept_staff+C2()$concept_aisles+C2()$concept_people+C2()$concept_help+C2()$concept_fast+C2()$concept_open+C2()$concept_return+C2()$concept_friendly+C2()$concept_customer_service+C2()$concept_available+C2()$concept_check_out+C2()$concept_stocked+C2()$concept_stock+C2()$concept_day+C2()$concept_pay+C2()$concept_check+C2()$concept_list+C2()$concept_shelf+C2()$concept_convenient+C2()$concept_person+C2()$concept_receipt+C2()$concept_courteous+C2()$concept_easy_to_access+C2()$concept_not_working+C2()$concept_shelves+C2()$concept_well_stocked+C2()$concept_able_to_find+C2()$concept_busy+C2()$concept_lines+C2()$concept_wait_time+C2()$concept_out_of+C2()$concept_purchase+C2()$concept_clean+C2()$concept_functioning+C2()$concept_too_long+C2()$concept_service+C2()$concept_timely+C2()$concept_short+C2()$concept_out_of_stock+C2()$concept_location+C2()$concept_time,control=control_rm(),data = C2())
    
    
  })
  
  
  #Plotting the output of chaid
  output$rmplot <- renderPlot({
    
    plot(concepts_z())
  })
  
  
  C1<- reactive(concepts[which(concepts$storenum==input$store),])
  control_ms <- reactive(chaid_control(minbucket = 1, minsplit = 1,minprob=0.01,alpha2=0.5, alpha4 = 0.5,maxheight=input$tree) )
  concepts_x<-reactive({
    
    chaid(C1()$PD_Flag~C1()$concept_bag+C1()$concept_cart+C1()$concept_thanks+C1()$concept_trip+C1()$concept_price+C1()$concept_order+C1()$concept_app+C1()$concept_large+C1()$concept_easy+C1()$concept_rude+C1()$concept_selection+C1()$concept_slow+C1()$concept_registration+C1()$concept_items+C1()$concept_groceries+C1()$concept_self_checkout+C1()$concept_checkout_lines+C1()$concept_scan+C1()$concept_long+C1()$concept_low+C1()$concept_walmart_pay+C1()$concept_place+C1()$concept_workers+C1()$concept_product+C1()$concept_manager+C1()$concept_efficient+C1()$concept_staff+C1()$concept_aisles+C1()$concept_people+C1()$concept_help+C1()$concept_fast+C1()$concept_open+C1()$concept_return+C1()$concept_friendly+C1()$concept_customer_service+C1()$concept_available+C1()$concept_check_out+C1()$concept_stocked+C1()$concept_stock+C1()$concept_day+C1()$concept_pay+C1()$concept_check+C1()$concept_list+C1()$concept_shelf+C1()$concept_convenient+C1()$concept_person+C1()$concept_receipt+C1()$concept_courteous+C1()$concept_easy_to_access+C1()$concept_not_working+C1()$concept_shelves+C1()$concept_well_stocked+C1()$concept_able_to_find+C1()$concept_busy+C1()$concept_lines+C1()$concept_wait_time+C1()$concept_out_of+C1()$concept_purchase+C1()$concept_clean+C1()$concept_functioning+C1()$concept_too_long+C1()$concept_service+C1()$concept_timely+C1()$concept_short+C1()$concept_out_of_stock+C1()$concept_location+C1()$concept_time,control=control_ms(),data = C1())
  }
  )
  
  output$msplot <- renderPlot({
    
    plot(concepts_x())
  })
  
  #c2<-reactive(openend[which(openend$Region==input$region),])
  
  #c3<-reactive(openend[which(c2()$Market==input$Markets),])
  
  c4<-reactive(openend[which(openend$Concept==input$Con),])
  
  c5<-reactive(openend[which(c4()$storenum==input$Sto),])
  
  
  output$openendresult<- renderDataTable(c5())
  
  
}

shinyApp(ui, server)

