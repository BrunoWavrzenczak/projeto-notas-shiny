shinyServer(function(input, output, session) {

    observe({
        req(input$periodo)
        if (input$periodo == 2) {
            shiny::updateSelectInput(session, "materia", "materia", c('calculo-1', 'GA', 'est.computacional'), 'calculo-1')
        } else if (input$periodo == 1) {
            shiny::updateSelectInput(session, "materia", "materia", c('est.básica', 'matriz', 'função'), 'est.básica')
        }
    })

    data <- reactive({
        req(input$periodo, input$materia)

        data <- tb |>
            dplyr::filter(
                periodo == input$periodo &
                materia == input$materia
            )
        
        data
    })

    # ==============================================================================
    output$p1 <- renderPlot({
        req(data())

        validate(
            need(nrow(dplyr::filter(data(),prova == 1)) != 0, "Sem informação")
        )

        data() |>
            dplyr::filter(prova == "1") |> 
            ggplot(aes(x = notas)) +
            geom_histogram(
                color = 'black', binwidth = 1, boundary = 0,
                fill = c(rep('coral1', 4), rep('3', 3), rep('4', 3))
            ) +
            labs(title = paste('p1', input$materia), x = 'notas', y = 'frequencia') +
            scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
            scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
            geom_vline(aes(xintercept = mean(notas), color = 'média'), size = 1.5) +
            scale_color_manual('', values = 'black') +
            theme_minimal()
    })

    # # ==============================================================================
    output$p2 <- renderPlot({
        req(data())

        validate(
            need(nrow(dplyr::filter(data(),prova == 2)) != 0, "Sem informação")
        )

        data() |>
            dplyr::filter(prova == '2') |> 
            ggplot(aes(x = notas)) +
            geom_histogram(color = 'black', binwidth = 1, boundary = 0,
                fill = c(rep('coral1', 4), rep('3', 3), rep('4', 3))) +
            labs(title = paste('p2', input$materia), x = 'notas', y = 'frequencia') +
            scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
            scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
            geom_vline(aes(xintercept = mean(notas), color = 'média'), size = 1.5) +
            scale_color_manual('', values = 'black') +
            theme_minimal()
    })

    # ==============================================================================
    output$p3 <- renderPlot({
      req(data())

      validate(
        need(nrow(dplyr::filter(data(),prova == 3)) != 0, 'Sem informação')
      )

      data() |>
        dplyr::filter(prova == '3') |>
        ggplot(aes(x = notas)) +
        geom_histogram(color = 'black', binwidth = 1, boundary = 0,
                       fill = c(rep('coral1', 4), rep('3', 3), rep('4', 3))) +
        labs(title = paste('p3', input$materia), x = 'notas', y = 'frequencia') +
        scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
        scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
        geom_vline(aes(xintercept = mean(notas), color = 'média'), size = 1.5) +
        scale_color_manual('', values = 'black') +
        theme_minimal()
     })
     # ==============================================================================

    output$bar <- renderPlot({
      req(data())
      
      validate(
        need(nrow(dplyr::filter(data(),prova == 'final')) != 0, 'Sem informação')
      )

      data() |>
        dplyr::filter(prova == 'final') |>
        ggplot(
          aes(x = factor(intervalo, levels = c('[0,4]', '(4,7]', '(7,10]')),
              fill = intervalo)) +
        geom_bar() +
        scale_fill_manual('', values = c('coral1', '3', '4'),
                          breaks = c('[0,4]', '(4,7]', '(7,10]'),
                          labels = c('reprovaram', 'final', 'passaram')) +
        labs(x = '', y = '') +
        scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5))+
        theme_minimal()

     })
})