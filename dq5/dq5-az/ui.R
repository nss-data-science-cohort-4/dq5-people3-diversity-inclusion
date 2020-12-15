#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel('Gender Distribution in Davidson'),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            plotOutput('generation'),
            plotOutput('decade')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("gender")
        )
    )
))
