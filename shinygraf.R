library(shiny)
library(tidyverse)
tb <-
  read.csv('C:\\Users\\bruno\\Downloads\\projeto notas - Página1.csv')
tb <- tb %>%
  mutate(notas_cut = cut(notas, c(0, 4, 7, 10),include.lowest = T))
ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    selectInput('periodo', 'qual periodo?', unique(tb$periodo), 1),
    selectInput(
      'mat1',
      'materia',
      '',
      ''
    )
  ),
  mainPanel(plotOutput('p1'),
            plotOutput('p1_extra'))
))

server <- function(input, output,session) {
  observe({
    x <- tb%>% dplyr::filter(periodo==tb$periodo)%>% pull(materia)%>%unique()
    updateSelectInput(session,'mat1','materia',x,x[1])
  })
  
  output$p1 <- renderPlot({
    req(input$mat1)
    x <- tb %>%
      dplyr::filter(materia == input$mat1)
    
    x %>%
      count(notas_cut,periodo) %>%
      mutate(periodo = as.character(periodo))%>%
      
      ggplot(aes(x = periodo, y = n, fill = notas_cut)) +
      #geom_histogram(boundary = 0, binwidth = 1, color = 'black')+
      geom_bar(stat = 'identity',position = 'stack') 
      # scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
      # labs(title = 'media final de matrizes', x = 'notas', y = 'frequencia') +
      # scale_fill_manual('notas_cut', values = c('blue', 'red', 'green')) +
      # geom_vline(aes(xintercept = mean(notas), color = 'media'), size = 1.5) +
      # scale_color_manual('', values = c('black')) +
      # theme_minimal()
    
  })
  
  
}

shinyApp(ui, server)
