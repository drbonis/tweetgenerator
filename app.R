#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rjson)
source("main.R")

Queen_uk <<- build_model("queen_uk")
IQVia_global <<- build_model("iqvia_global")
ConfunciusQuote <<- build_model("confuciusquote")
realDonaldTrump <<- build_model("realdonaldtrump")
iberia <<- build_model("iberia")
ifilosofia <<- build_model("ifilosofia")
bolsamania <<- build_model("bolsamania")
empleoytrabajo <<- build_model("empleoytrabajo")
perezreverte <<- build_model("perezreverte")
dlacalle <<- build_model("dlacalle")
juliomayol <<- build_model("juliomayol")
manyez <<- build_model("manyez")
rcofinof <<- build_model("rcofinof")
fllordachs <<- build_model("fllordachs")
marca <<- build_model("marca")
medecofamilia <<- build_model("medecofamilia")
roberrimbaud <<- build_model("roberrimbaud")
rubiu5 <<- build_model("rubiu5")



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Tweet generator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "twitteraccount",
                    label = "Choose account to imitate:",
                    choices = c("Queen_uk", 
                                "IQVia_global", 
                                "JustinBieber",
                                "ConfunciusQuote",
                                "realDonaldTrump",
                                "iberia",
                                "ifilosofia",
                                "bolsamania",
                                "empleoytrabajo",
                                "perezreverte",
                                "dlacalle",
                                "juliomayol",
                                "manyez",
                                "rcofinof",
                                "fllordachs",
                                "marca",
                                "medecofamilia",
                                "roberrimbaud",
                                "rubiu5"
                                ))

      ),

      
      # Show a plot of the generated distribution
      mainPanel(
         htmlOutput("image"),
         HTML(paste0("<h3>",textOutput("newTweet1"),"</h3>"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$image <- renderUI(img(src=paste0('https://avatars.io/twitter/',input$twitteraccount),
                               width='80px'
                               ))
  
  output$newTweet1 <- 
    renderText({
      build_random_tweet(input$twitteraccount,20,20)[[1]]
    })
  output$newTweet2 <- 
    renderText({
      build_random_tweet(input$twitteraccount,20,20)[[1]]
    })
  output$newTweet3 <- 
    renderText({
      build_random_tweet(input$twitteraccount,20,20)[[1]]
    })
  output$newTweet4 <- 
    renderText({
      build_random_tweet(input$twitteraccount,20,20)[[1]]
    })
  output$newTweet5 <- 
    renderText({
      build_random_tweet(input$twitteraccount,20,20)[[1]]
    })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

