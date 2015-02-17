library(shiny)
library(forecast)



source("brand.R")
source("campagne.R")



shinyUI(pageWithSidebar(
  headerPanel(span("NetBooster Prédiction",style="color:pink")),
  
  sidebarPanel(

    dateRangeInput("dates", label = h5("Date"),start="2012-09-02",end="2014-09-22",min="2012-09-01",max="2014-09-23")
    ,
    
    radioButtons("Type", "Type:", choices=c("Brand","Generic")),
    
    
    br(),
    selectInput("model", "Choissez un modèle", choices=list("var","arma"), selected = "var")
    ,
    uiOutput("P"),
    uiOutput("Q"),
    uiOutput("T"),
    
    radioButtons("dataset", "Choisissez une donnée:", 
                 choices = c("Clics","Impressions","CTR","CPC.moy.","Taux.de.conversion.des.clics","Valeur.de.conv..totale")),
    
    
    
    br(),
    sliderInput("obs", "Nombre d'observations à voir:", min=1,max=280,value=100,step=1),
    
    
    
    br(),
    br(),
    helpText(h3("Modifier:" )),
    br(),
    
    sliderInput("modcpc","CPC.moy. ",min=-100, max=100,value=0,step=10),
    br(),
    sliderInput("modctr","CTR ",min=-100, max=100,value=0,step=10),
    br(),
    sliderInput("modimp","Impressions ",min=-100, max=100,value=0,step=10),
    br(),
    sliderInput("modtaux","Taux.de.conversion.des.clics ",min=-100, max=100,value=0,step=10),
    br(),
    
    selectInput("Cam", label = strong("Choisissez campagne:"),rownames(ro)),
    br(),
    checkboxInput(inputId = "perdu",
                  label = strong("Impressions perdues 100%"),
                  value = FALSE),
    br(),
    uiOutput("Per")
    
    
    
  ),
  
  
  mainPanel(
    tabsetPanel(
      
      
      tabPanel("Model Select",plotOutput("Modelplot"),plotOutput("Modelplot1"),
               conditionalPanel("input.model=='var'", verbatimTextOutput("sum1")),
               conditionalPanel("input.model=='var'", verbatimTextOutput("sum5"))
      ),
      
      
      tabPanel("Model Stats",conditionalPanel("input.model=='var'", verbatimTextOutput("sum")),
               conditionalPanel("input.model=='arma'", verbatimTextOutput("sum2")),
               conditionalPanel("input.model=='var'", verbatimTextOutput("sum3")),
               conditionalPanel("input.model=='arma'", plotOutput("sum4"))),
      
      
      tabPanel("PrÃ©diction",conditionalPanel("input.model=='var'",dataTableOutput("view")),
               conditionalPanel("input.model=='arma'",dataTableOutput("view1"))),
      
      tabPanel("RÃ©sumÃ© de prÃ©diction",conditionalPanel("input.model=='var'",verbatimTextOutput("summary")),
               conditionalPanel("input.model=='arma'",verbatimTextOutput("summary1"))),
      
      tabPanel("Graphique",conditionalPanel("input.model=='var'",plotOutput("plot")),
               conditionalPanel("input.model=='arma'",plotOutput("plot1"))),
      
      
      tabPanel("Final",conditionalPanel("input.model=='var'",plotOutput("final")),
               conditionalPanel("input.model=='arma'",plotOutput("final1"))),
      
      tabPanel("Budget",conditionalPanel("input.model=='var'",plotOutput("budget")),
               conditionalPanel("input.model=='arma'",plotOutput("budget1"))),
      
      
      
      
      
      
      tabPanel("Donnée",dataTableOutput("look")),
      tabPanel("Campagne",plotOutput("cc"),plotOutput("cc1"),plotOutput("cc2"),plotOutput("cc3")),
      tabPanel("Mois",plotOutput("mois"))
      
    )   
    
    
  )
  
))
