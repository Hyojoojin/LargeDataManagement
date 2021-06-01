library(shiny)
year2018  = fread("2018.csv", header = TRUE, 
                        stringsAsFactors = FALSE, data.table = F)
year2019  = fread("2019.csv", header = TRUE, 
                        stringsAsFactors = FALSE, data.table = F)
year2020  = fread("2020.csv", header = TRUE, 
                        stringsAsFactors = FALSE, data.table = F)
year2021  = fread("2021.csv", header = TRUE, 
                        stringsAsFactors = FALSE, data.table = F)



ui <- fluidPage(
  titlePanel("Happiness Rank Comparison"), 
  
  sidebarLayout(
    sidebarPanel(helpText("Select the range of ranks to show in each year:"),
                 sliderInput("range1",
                             label = "Number of Ranks 2018:",
                             min = 1, max = 90, value = c(1, 90)),
                 sliderInput("range2",
                             label = "Number of Ranks 2019:",
                             min = 1, max = 90, value = c(1, 90)),
                 sliderInput("range3",
                             label = "Number of Ranks 2020:",
                             min = 1, max = 90, value = c(1, 90)),
                 sliderInput("range4",
                             label = "Number of Ranks 2021:",
                             min = 1, max = 90, value = c(1, 90))
                 ),
    mainPanel(h2("Happiness Rank"),
              plotOutput("selected_var"))
  )
)


server <- function(input, output) {
  Input2018 <- reactive({
    year2018[order(year2018$rank)[input$range1[1]:input$range1[2]],]
  })
  
  Input2019 <- reactive({
    year2019[order(year2019$rank)[input$range2[1]:input$range2[2]],]
  })
  
  Input2020 <- reactive({
    year2020[order(year2020$rank)[input$range3[1]:input$range3[2]],]
  })
  
  Input2021 <- reactive({
    year2021[order(year2021$rank)[input$range4[1]:input$range4[2]],]
  })
  output$selected_var <- renderPlot({
    p1<-ggplot(Input2018(), aes(reorder(country,rank),rank, fill = region)) + xlab("Country")+ylab("Rank")+
      geom_col()+ coord_flip()+ labs(fill = "Regions")+theme_bw()+ggtitle("2018")+
      theme(plot.title = element_text(size = 15, face = "bold"))+
      scale_fill_brewer(palette = "PuRd")
    p2<-ggplot(Input2019(), aes(reorder(country,rank),rank, fill = region)) + xlab("Country")+ylab("Rank")+
      geom_col()+ coord_flip()+ labs(fill = "Regions")+theme_bw()+ggtitle("2019")+
      theme(plot.title = element_text(size = 15, face = "bold"))+
      scale_fill_brewer(palette = "GnBu")
    p3<-ggplot(Input2020(), aes(reorder(country,rank),rank, fill = region)) + xlab("Country")+ylab("Rank")+
      geom_col()+ coord_flip()+ labs(fill = "Regions")+ theme_bw()+ggtitle("2020")+
      theme(plot.title = element_text(size = 15, face = "bold"))+
      scale_fill_brewer(palette = "BuPu")
    p4<-ggplot(Input2021(), aes(reorder(country,rank),rank, fill = region)) + xlab("Country")+ylab("Rank")+
      geom_col()+ coord_flip()+ labs(fill = "Regions")+ theme_bw()+ggtitle("2021")+
      theme(plot.title = element_text(size = 15, face = "bold"))+
      scale_fill_brewer(palette = "BuGn")
    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  })
}

shinyApp(ui = ui, server = server)