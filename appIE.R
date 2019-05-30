library(shiny)
library(datasets)
library(magrittr) 
library(dplyr)    
library(ggplot2)
library(shinyWidgets)

cost = read.csv("costComparison.csv",header=T)

ui <- fluidPage(setBackgroundColor("#222222"),
                tags$head(tags$style(
                  HTML('
                       #sidebar {
                       background-color: #222222;
                       },
                       
                       body, label, input, button, select { 
                       font-family: "Arial";
                       }'))),
  tags$head(tags$style(
    type="text/css",
    "#img img {max-width: 100%; width: 100%; height: auto}"
                  )),
  
  sidebarLayout(
    sidebarPanel(id="sidebar",setBackgroundColor("#222222"),
                 
                 radioButtons("rbreason",  HTML("<h3 style='color:white;'>Reasons for hospital care:</h3>"),
                              choiceNames = list(
                                HTML("<p style='color:white;'>Admit for Renal Dialysis</p>"),
                                HTML("<p style='color:white;'>Rehabilitation</p>"),
                                HTML("<p style='color:white;'>Dementia and Other Chronic Disturbances of Cerebral Function</p>"),
                                HTML("<p style='color:white;'>Respiratory Infections/Inflammations</p>"),
                                HTML("<p style='color:white;'>Other Factors Influencing Health Status</p>"),
                                HTML("<p style='color:white;'>Kidney and Urinary Tract Infections</p>"),
                                HTML("<p style='color:white;'>Injuries</p>"),
                                HTML("<p style='color:white;'>Heart Failure and Shock</p>"),
                                HTML("<p style='color:white;';label title='The reflux of stomach contents into the oesophagus (tube leading to the stomach)'>Oesophagitis, Gastroenteritis and Misc Digestive System Disorders Age >9</p>"), 
                                HTML("<p style='color:white;'>Stroke</p>"),
                                HTML("<p style='color:white;'>Other hip and femur procedures</p>"),
                                HTML("<p style='color:white;'> Chronic obstructive airways disease</p>"),
                                HTML("<p style='color:white;'>Syncope and collapse</p>"),
                                HTML("<p style='color:white;';label title='Serious disturbance in mental abilities that results in confused thinking and reduced awareness of surroundings.'>Delirium</p>"),
                                HTML("<p style='color:white;';label title='Septicaemia is an infection caused by large amounts of bacteria entering the bloodstream.'>Septicaemia</p>"), 
                                HTML("<p style='color:white;'>Chest pain</p>"), 
                                HTML("<p style='color:white;'>Other disorders of the nervous system</p>"),
                                HTML("<p style='color:white;'> Hip revision or replacement</p>"),
                                HTML("<p style='color:white;'> Non-surgical spinal disorders</p>"),
                                HTML("<p style='color:white;';label title='Cellulitis is a bacterial infection of the skin that tends to occur on the lower legs and in areas that are damaged or inflamed'>Cellulitis</p>"), 
                                HTML("<p style='color:white;'>All</p>")
                              ),
                              choiceValues = list("Admit for Renal Dialysis","Rehabilitation","Dementia and Other Chronic Disturbances of Cerebral Function",
                                                  "Respiratory Infections/Inflammations","Other Factors Influencing Health Status",
                                                  "Kidney and Urinary Tract Infections",
                                                  
                                                  "Injuries","Heart Failure and Shock","Oesophagitis, Gastroenteritis and Misc Digestive System Disorders Age >9","Stroke",
                                                  "Other hip and femur procedures","Chronic obstructive airways disease","Syncope and collapse","Delirium","Septicaemia",
                                                  "Chest pain", "Other disorders of the nervous system", "Hip revision or replacement","Non-surgical spinal disorders",
                                                  "Cellulitis","Show all reasons"
                              ),selected="Admit for Renal Dialysis"
                 )
    ),
    
    mainPanel(
      br(),
      HTML("<h1 style='color:white'; align='center'>Average hospital care costs comparison between people with and without dementia</h1>"),
      br(),
      br(),
      br(),
      textOutput('text1'),
      tags$head(tags$style("#text1{color: #ADD8E6;
                           font-size: 30px;
                           font-style: italic;  
                           text-align: center
                           }"
      )
      ),
      
      br(),
      br(),
      
      plotOutput("costplot")

      )
      )
    )

server <- function(input, output) {
  
  output$text1 <- renderText({ paste(input$rbreason) }) 
  output$costplot <- renderPlot({
    
    #Filter the data based on the radio button selection.
    
    if(input$rbreason == "Show all reasons")
    {
      ggplot(data=cost, aes(x=Reason, y=cost, fill=status,label=cost)) +
        geom_bar(stat="identity", position=position_dodge(), size = 1) +   geom_text(aes(label = cost, y = cost,fontface = "bold"),check_overlap = TRUE,size=4)  +
        coord_flip()  
    }
    else
    {
      filteredcost <-
        cost %>%
        filter(
          Reason== input$rbreason
        )
      ggplot(data=filteredcost, aes(x=Reason, y=cost, fill=status,label=cost)) +
        geom_bar(stat="identity", position=position_dodge(), size = 1) +   geom_text(aes(label = cost, y = cost,fontface = "bold"),check_overlap = TRUE,size=5, position=position_dodge(width=0.9), vjust=-0.25)  
    }})
}


shinyApp(ui = ui, server = server)






