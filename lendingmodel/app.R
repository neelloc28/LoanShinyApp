#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#In the server side, where you have the bulk of the R code, start with a static graph. 
#Use an observation from the data and create the CP profile, paying no attention to the values from the ui part. 
#Once you have your static graph working, use the data from the ui for the prediction,
#but create the graph for a specific variable, rather than from the variable chosen in the ui.
#If you make it this far, that's great!
#Don't worry too much about the "choosing the variable" piece - that's something we could go over together on Friday or next Monday

library(shiny)
library(tidyverse)
library(tidymodels)
library(stacks)
library(modeldata)

#load in model
final_model <- readRDS("lending_final_stack.rds")

#read in some of the data
data("lending_club")

#lump states based on region
regions <- read.csv("statesregions.csv")

lending_club1 <- lending_club %>%
    mutate(State.Code = addr_state) %>%
    left_join(regions) %>%
    select(-addr_state, -State, -State.Code, -Division) %>%
    mutate(Region = as.factor(Region))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predict Probability of Loan Repayment"),

    sliderInput(inputId = "annual_income", label = "Annual Income of Borrower:", min = min(lending_club1$annual_inc), max = max(lending_club1$annual_inc), value = min(lending_club$annual_inc)),
    
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        obs1 <- lending_club1 %>%
            slice(1) #takes first observation from lending club dataset
        
        
        min_inc <- min(lending_club1$annual_inc)
        max_inc <- max(lending_club1$annual_inc)
        
        obs1_many <- obs1 %>% 
            #there is probably a better way to do this
            sample_n(size = 50, replace = TRUE) %>% 
            select(-annual_inc) %>% 
            mutate(annual_inc= seq(min_inc, max_inc, length.out = 50))
        
        obs1_many %>% 
            select(annual_inc) %>% 
            bind_cols(
                predict(final_model,
                        new_data = obs1_many, type = "prob")
            ) %>% 
            ggplot(aes(x = annual_inc,
                       y = .pred_good)) +
            geom_line() +
            labs(y = "Predicted Price (log 10)")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
