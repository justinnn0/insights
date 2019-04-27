library(shiny)
library(datasets)
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)    # alternative, this also loads %>%
library(ggplot2)
library(shinyWidgets)
dementiaData = read.csv("State-Territory-Dementia.csv",header = T)
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
                 
                 radioButtons("rbState",  HTML("<h3 style='color:white;'>State or region:</h3>"),
                              choiceNames = list(
                                HTML('<img id="img1" src="NSW.png" height=80 px width=80 px"><label style="color:white">NSW</label></img>'),
                                HTML('<img id="img2" src="VIC.png" height=80 px width=80 px"><label style="color:white">VIC</label></img>'),
                                HTML('<img id="img3" src="QLD.png" height=80 px width=80 px"><label style="color:white">QLD</label></img>'),
                                HTML('<img id="img4" src="WA.png" height=80 px width=80 px"><label style="color:white">WA</label></img>'),
                                HTML('<img id="img5" src="SA.png" height=80 px width=80 px"><label style="color:white">SA</label></img>'),
                                HTML('<img id="img6" src="TAS.png" height=80 px width=80 px"><label style="color:white">TAS</label></img>'),
                                HTML('<img id="img7" src="ACT.png" height=80 px width=80 px"><label style="color:white">ACT</label></img>'),
                                HTML('<img id="img8" src="NT.png" height=80 px width=80 px"><label style="color:white">NT</label></img>'),
                                HTML('<img id="img9" src="allAu.png" height=80 px width=80 px"><label style="color:white">All</label></img>')
                                
                                
                              ),
                              choiceValues = list(
                                "NSW", "Vic", "Qld","WA", "SA", "Tas","ACT","NT","Show all states"
                              ),selected="NSW"
                 ),
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
                                HTML("<p style='color:white;'>Oesophagitis, Gastroenteritis and Misc Digestive System Disorders Age >9</p>"), 
                                HTML("<p style='color:white;'>Stroke</p>"),
                                HTML("<p style='color:white;'>Other hip and femur procedures</p>"),
                                HTML("<p style='color:white;'> Chronic obstructive airways disease</p>"),
                                HTML("<p style='color:white;'>Syncope and collapse</p>"),
                                HTML("<p style='color:white;'>Delirium</p>"),
                                
                                HTML("<p style='color:white;'>Septicaemia</p>"),
                                HTML("<p style='color:white;'>Chest pain</p>"), 
                                HTML("<p style='color:white;'>Other disorders of the nervous system</p>"),
                                HTML("<p style='color:white;'> Hip revision or replacement</p>"),
                                HTML("<p style='color:white;'> Non-surgical spinal disorders</p>"),
                                HTML("<p style='color:white;'>Cellulitis</p>"),
                                HTML("<p style='color:white;'>All</p>")
                                
                                
                                #img(id="img9",src = "English.png", height = 40, width=80),
                                #img(id="img10",src = "Italian.png", height = 40, width=80),
                                #img(id="img11",src = "Chinese.png", height = 40, width=80),
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
      HTML("<h1 style='color:white'; align='center'>Number of people with dementia in Australia</h1>"),
      #headerPanel("Number of people with dementia in Australia"), 
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
     
      
      plotOutput("coolplot"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
    
      
      br(),
      HTML("<h1 style='color:white'; align='center'>Average hospital care costs comparison between people with and without dementia</h1>"),
      br(),
      
      br(),
      br(),
      
      br(),
      br(),
      
      plotOutput("costplot")
      
      
      
      
    )
  )
  
  
  
  
  
  #headerPanel("Average costs comparison between people with and without dementia"), 
  
  
  
  
                )

server <- function(input, output) {
  output$coolplot <- renderPlot(
    
    
    
    {
      if(input$rbState == "Show all states")
      {
        ggplot(dementiaData,aes(Year,Number,colour=State)) + 
          geom_line() + geom_text(aes(label = Number, y = Number,fontface = "bold"),check_overlap = TRUE,size=4,color="Black")
      }
      else
      {
        filtered <-
          dementiaData %>%
          filter(
            State == input$rbState
            
          )
        
        
        ggplot(filtered,aes(Year,Number,label=Number)) + 
          geom_point(aes(color=State)) + facet_wrap(~State) + geom_smooth(method = "lm", formula = y ~ x, size = 1) +  geom_text(aes(label = Number, y = Number,fontface = "bold"), ,check_overlap = TRUE,size=4)
        
        
        
      }})
  
  
  
  output$costplot <- renderPlot({
    
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





