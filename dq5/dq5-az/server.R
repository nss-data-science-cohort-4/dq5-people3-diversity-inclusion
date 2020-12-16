#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$gender <- renderPlotly({
        gender_gg <- gender_Davidson %>%
            plot_ly(labels= ~variable, values= ~estimate, marker = list(colors = c('#F6DDB6','#F0C37F')))
        gender_gg<- gender_gg %>%
            add_pie(hole=0.5)
        gender_gg <- gender_gg %>%
            layout(title='Male and Female Population',
                   xaxis=list(showgrid=FALSE, zeroline= FALSE, showticklabels= FALSE),
                   yaxis= list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))

    })
    output$generation <- renderPlotly({
     gen_gg
    })

    output$decade <- renderPlotly({
        decade_gg
    })

})
