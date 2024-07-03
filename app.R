#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
nfl_draft <- read_csv("nfl_draft.csv")

nfl_draft<- nfl_draft|>
  filter(tm !="RAM",tm != "RAI",tm !="PHO")|>
  mutate(division = case_when(
    tm %in% c("NYG", "PHI", "WAS", "DAL") ~ "NFC East",
    tm %in% c("GNB", "CHI", "MIN", "DET") ~ "NFC North",
    tm %in% c("TAM", "NOR", "CAR", "ATL") ~ "NFC South",
    tm %in% c("SEA", "SFO", "ARI", "STL") ~ "NFC West",
    tm %in% c("NWE", "BUF", "MIA", "NYJ") ~ "AFC East",
    tm %in% c("BAL", "PIT", "CLE", "CIN") ~ "AFC North",
    tm %in% c("HOU", "IND", "TEN", "JAX") ~ "AFC South",
    tm %in% c("KAN", "OAK", "SDG", "DEN") ~ "AFC West",
  ))


plot_draft <- function(nfl_draft){
  ggplot(nfl_draft)+
    geom_bar(aes(pb,tm, fill=division),width=.95, stat="identity")+
    theme(panel.grid.major.y=element_blank(),
          axis.ticks=element_blank())+
    scale_x_continuous(expand = c(0, 0, 0.1, 0.1)) +
    scale_fill_manual(values=c("#84E1BB", "#9160E1","#6F71E0","#C69DE0","#F4B793","#DEDBB0","#9BC9FA","#DB7E82"))+
    labs(x="Number of Pro Bowl Selections", y= "NFL Team", fill="Division")+
    ggtitle("Pro Bowl Selections Drafted by Each Team")
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NFL Draft Pick vs Pro Bowl Appearance"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          pickerInput("year","Year", unique(nfl_draft$year), options=list(`actions-box`=TRUE), selected = c("1985":"2015"), multiple=TRUE),
          pickerInput("rnd", "Round", unique(nfl_draft$rnd),options=list(`actions-box`=TRUE), nfl_draft$rnd[1], multiple=TRUE),
          h5("This bar graph shows how many Pro Bowl Selections each team has drafted from 1985 to 2015. You can adjust the year to see which teams had a good draft that year and you can adjust the round the pick was drafted to see if picks in higher rounds are more likely to go to the Pro Bowl"),
          h6("The Pro Bowl is an annual selection of the best players from each conference")), 

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("line_plot")
        )
    )
)

 
# Define server logic required to draw a histogram
server<-function(input,output){
  observe(
  output$line_plot <-renderPlot({
    nfl_draft|>
      filter(year %in% input$year,
             rnd %in% input$rnd)|>
      plot_draft()
  })
  )
}

# Run the application 
shinyApp(ui,server)
