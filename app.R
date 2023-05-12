library(shiny)
library(shinythemes)

library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(forcats)
library(scales)
library(DT)


# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Airbnb data Paris"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    numericInput("num", label = "1.Nber of Apartments owned", value = 1, min =1, max=200),
    
    br(),
    br(),
    br(),
    
    #Input: Date selector for the visit frequency over time
    selectizeInput(inputId = "agglo",
                   label="2.Select the area",
                   choices = unique(Q$agglo),
                   selected = "Paris",
                   multiple = FALSE
    ),
    
  
    
    selectInput("zipcode", label = "Quarter", 
                choices = sort(unique(Q$zipcode)), 
                selected = 1),
    br(),
    
    
    dateRangeInput(inputId="date_visit",
                   label = "Visits period",
                   start = min(Q$new_date), 
                   end = max(Q$new_date)),
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),br(),br(),br(),br(),br(),br(),br(),br(),
    sliderInput(inputId = "new_price",
                label = "4.Select a renting price:",
                min = 0,
                max = 500,
                value = 100),
    br(),
    br()
    
    
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    h2("1. Number of apartments per owner"),
    textOutput("selected_var"),
    verbatimTextOutput("item_e"),
    
    
    br(),
    br(),
    
    h2("2. Visit frequency of the different quarters by year"),
    plotOutput("plot_a"),
    br(),
    br(),
    p(
      "In this visual we can select the data from Paris or from the suburb (Select the area) to see the average price of the listing featuring that amenity. "
    ),
    br(),
    br(), 
    
    h2("3. Price vs apartment features"),
    #plotOutput("plot_c"),
    br(),
    plotOutput("plot_d"),
    
    br(),
    p(
      "For instance, in Paris, if a listing features air conditionning, the average price will be around 175€ whereas it will be around 80€ in the suburbs. We can see the difference by selecting the area as in the previous visual. "
    ),
    
    plotOutput("plot_g"),
    br(),
    p(
      "In this second visual, we can see the most common/rare amenities by zipcode, sorted in ascending order. No surprise, a kitchen is always present, but a pool and/or Washer dryer are rarer in the listing, probably because of the dryer, since the single amenity washer is more commonly present alongside the Kitchen. "
    ),
    
    h2("4. Distribution of the renting price per city quarter"),
 # plotOutput("selected_period"),
 plotOutput("plot_b"),
 plotOutput("plot_f"),
 br(),
 br(),
 p(
   "With this visual, we can see the range of the rent for each quarter, depending of the maximum renting price selected on the slider, as well as the most affordable/expensive quarters for this price."
 ),
    
 br(),
 br(),
 
 br()
 
 
 
  )
)


# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  #output$dateRangeText  <- renderText({
    #paste("input$dateRange is", 
    #      paste(as.character(input$dateRange), collapse = " to ")
   # )
  #})
  d <- reactive({
    filtered <-
      Q %>%
      filter( agglo == input$agglo,
              zipcode == input$zipcode,
              new_date %in% seq(input$date_visit[1], input$date_visit[2], by = 1))
  }) 
  
  a <- reactive({
    filtered <- mysum %>% filter(L.agglo == input$agglo)
  })
  
  table_filter <- reactive({
    G_agg %>%
      filter(Group.1 == input$num)
  })
  
  f <- reactive({
    filtered <- L2 %>% filter(L.agglo == input$agglo, zipcode)
  })

  g <- reactive({
    filtered <- Q3  %>% filter(L.zipcode==input$zipcode)
    
  })
  
  #plot a
  output$plot_a <- renderPlot({
    ggplot(d(), aes(x=new_date))+geom_histogram(position="identity", alpha=0.5, color = "#d3ebcd", fill = "#377d71")+xlab(input$agglo)+ylab("Visit frequency")
  })
  
  #plot b
  output$plot_b <- renderPlot({
    b_L2 <- filter(L1, new_price<=input$new_price)
    ggplot(data=b_L2, aes(x=fct_reorder(zipcode,new_price), y=new_price, fill=new_price))+geom_boxplot()+coord_flip()
  })
  

  #plot d
  output$plot_d <- renderPlot({
    my_groups = group_by(Q3,Amenities, L.agglo)
    mysum = summarise(my_groups,
                      price_mean = mean(L.new_price,na.rm=TRUE),
                      price_sd = sd(L.new_price, na.rm=TRUE))
    ggplot(a()) +
      geom_point(aes(x=Amenities,y=price_mean))+geom_smooth(mapping=aes(x=Amenities, y=price_mean),method="lm")+facet_wrap(~L.agglo, nrow=2)+guides(x = guide_axis(angle = 90))
  })
  
  #plot e
  output$selected_var <- renderText({
    paste("The number of owners who have",input$num," apartments is:")
  })
  
  output$item_e <- renderText({
    dataset <- table_filter()
    dataset$x
  })
  
#plot f
  
  output$plot_f <- renderPlot({
    
    ggplot(data=L2, aes(x=fct_reorder(zipcode,new_price), y=new_price, fill=new_price))+
    geom_bar(stat="identity", fill= "#54c3f3")+
    coord_flip()+
    theme_bw()+ ylab("Average renting price")+
    xlab("Quarter")+
    geom_text(aes(label=round(new_price, digits=0)), vjust=0.5, hjust=1.5, color="white", size=3.5)
    
  })
  
#plot_g
  output$plot_g <- renderPlot({
    ggplot(g(), aes(y=fct_infreq(Amenities))) + geom_bar(color="white",fill="#e69f00",width = 0.75, position=position_dodge(0.8))+
      geom_text(stat="count",aes(label=..count..),vjust=-1, size=3.5)+theme_gray()
  })
  
}

#Create Shiny object
shinyApp(ui = ui, server = server)