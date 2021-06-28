if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")
library(shiny)
library(shinyjs)

# Definition de l'interface utilisateur de l'application 
ui <- shinyUI(fluidPage(
  useShinyjs(),
  # titre application
  titlePanel("Géographie des commentaires Airbnb à Paris, Lyon et Bordeaux"),
  
  sidebarLayout(
    #Composants de la région gauche de l'application 
    sidebarPanel(

      selectInput(inputId = "ville", 
                  label = "Ville", 
                  choice = c("Paris","Bordeaux","Lyon")),
      
      selectInput(inputId = "mot", 
                  label = "Mot", 
                  choice = c("Bruit","Moustique",'Restaurant',"Voiture","Velo","Loin","Cher")),
      
      radioButtons("visuBtn", "Je souhaite visualiser...", 
                   choices = c("une carte dynamique" = "dynamique", 
                               "une carte statique" = "statique"))
    ),
  
    mainPanel(
      # conditionalPanel(
      #   condition = "input.visuBtn == 'dynamique'",
        leafletOutput("leaflet"),
      # ),
      # conditionalPanel(
      #   condition = "input.visuBtn == 'statique'",
        plotOutput("carte")
      # )
      
      
    )
  )))


server <- function(input, output, session){
  
  output$carte <- renderPlot({
    carte_statique(str_to_lower(input$ville),str_to_lower(input$mot))
  }) 

  output$leaflet <- renderLeaflet({
    carte_dynamique(str_to_lower(input$ville),str_to_lower(input$mot))
    
  })
    
  observeEvent(input$visuBtn,{
    req(input$visuBtn)
    print(input$visuBtn)
    if(input$visuBtn == "dynamique"){
      hide("carte")
      show("leaflet")
    }else{
      hide("leaflet")
      show("carte")
    }
  })
  
}

shinyApp(ui, server)

