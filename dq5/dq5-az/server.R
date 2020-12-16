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
        gen_gg <- plot_ly(gen_Davidsonmf, x=~generation, y=~women_gen, type='bar', name='Female', color = I('#9C877B'))
        gen_gg <- gen_gg %>% add_trace(y=~men_gen, name='Male', color= I('#DCC5A8'))
        gen_gg <- gen_gg %>%  layout (yaxis=list(title='Population'),
                                      xaxis=list(title='Age grouping'),
                                      barmode='stack')
    })

    output$decade <- renderPlotly({
        decade_gg <- plot_ly(dec_Davidson, x=~labels, y=~women, type='bar', name='Female',color = I('#9C877B'))
        decade_gg <- decade_gg %>% add_trace(y=~men, name='Male',color= I('#DCC5A8'))
        decade_gg <- decade_gg %>%  layout (yaxis=list(title='Population'),
                                            xaxis=list(title='Age grouping'),
                                            barmode='stack')
    })

})
