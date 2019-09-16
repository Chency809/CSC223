# server.R

ui <- fluidPage(
  titlePanel("NBA Players' salaries from 2008 to 2018"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create scatter plot for NBA data."),
      
      selectInput("xvar", 
                  label = "Choose a x variable to display",
                  choices = c("MIN", "PTS","FGM", "FGA","FG%",
                              "3PM","3PA","3P%","FTM","FTA","FT%",
                              "REB","AST","STL","BLK","TO","DD2","TD3",
                              "PER","RK"),
                  selected = "PTS"),
      
      selectInput("yvar", 
                  label = "Player's salary",
                  choices = c("SALARY"),
                  selected = "SALARY"),
    
      selectInput("yearvar", 
                label = "Choose each individual year",
                choices = c("2008-2009","2009-2010", "2010-2011","2011-2012", 
                            "2012-2013","2013-2014","2014-2015","2015-2016",
                            "2016-2017","2017-2018","2018-2019"),
                selected = "2008-2009")),
    
    mainPanel(plotOutput("plot"))
  ))


library(ggplot2)
Final_Dataset$MIN <- as.numeric(Final_Dataset$MIN)
Final_Dataset$SALARY <- as.numeric(Final_Dataset$SALARY)
Final_Dataset$PTS <- as.numeric(Final_Dataset$PTS)
Final_Dataset$FGM <- as.numeric(Final_Dataset$FGM)
Final_Dataset$FGA <- as.numeric(Final_Dataset$FGA)
Final_Dataset$`FG%` <- as.numeric(Final_Dataset$`FG%`)
Final_Dataset$`3PM` <- as.numeric(Final_Dataset$`3PM`)
Final_Dataset$`3PA` <- as.numeric(Final_Dataset$`3PA`)
Final_Dataset$`3P%` <- as.numeric(Final_Dataset$`3P%`)
Final_Dataset$FTM <- as.numeric(Final_Dataset$FTM)
Final_Dataset$FTA <- as.numeric(Final_Dataset$FTA)
Final_Dataset$`FT%` <- as.numeric(Final_Dataset$`FT%`)
Final_Dataset$REB <- as.numeric(Final_Dataset$REB)
Final_Dataset$AST <- as.numeric(Final_Dataset$AST)
Final_Dataset$STL <- as.numeric(Final_Dataset$STL)
Final_Dataset$BLK <- as.numeric(Final_Dataset$BLK)
Final_Dataset$TO <- as.numeric(Final_Dataset$TO)
Final_Dataset$DD2 <- as.numeric(Final_Dataset$DD2)
Final_Dataset$TD3 <- as.numeric(Final_Dataset$TD3)
Final_Dataset$PER <- as.numeric(Final_Dataset$PER)
Final_Dataset$RK <- as.numeric(Final_Dataset$RK)
Final_Dataset$Duration <- as.character(Final_Dataset$Duration)

server <- function(input, output) {
  output$plot <- renderPlot({
    Final_Dataset <- filter(Final_Dataset, Duration == input$yearvar)
    
    xdata <- switch(input$xvar, 
                    "MIN" = Final_Dataset$MIN,
                    "PTS" = Final_Dataset$PTS,
                    "FGM" = Final_Dataset$FGM, 
                    "FGA" = Final_Dataset$FGA,
                    "FG%" = Final_Dataset$`FG%`,
                    "3PM" = Final_Dataset$`3PM`,
                    "3PA" = Final_Dataset$`3PA`,
                    "3P%" = Final_Dataset$`3P%`,
                    "FTM" = Final_Dataset$FTM,
                    "FTA" = Final_Dataset$FTA,
                    "FT%" = Final_Dataset$`FT%`,
                    "REB" = Final_Dataset$REB,
                    "AST" = Final_Dataset$AST,
                    "STL" = Final_Dataset$STL,
                    "BLK" = Final_Dataset$BLK,
                    "TO" = Final_Dataset$TO,
                    "DD2" = Final_Dataset$DD2,
                    "TD3" = Final_Dataset$TD3,
                    "PER" = Final_Dataset$PER,
                    "RK" = Final_Dataset$RK)
    
    ydata <- switch(input$yvar, 
                    "SALARY" = Final_Dataset$SALARY)
    
    ggplot(Final_Dataset, aes(x = xdata, y = ydata)) +
      geom_point()
  })
}
shinyApp(ui, server)
