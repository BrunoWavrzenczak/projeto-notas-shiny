shinyServer(function(input, output, session) {

    output$materias <- renderUI({
        switch(input$periodo,
            '1' = selectInput('mat1', 'matéria', c('est.básica', 'matriz', 'função'), 'est.básica'),
            '2' = selectInput('mat2', 'matéria', c('calculo-1', 'GA', 'est.computacional'), 'calculo-1'))
    })

    # ==============================================================================

    output$p1 <- renderPlot({
        switch(input$periodo,
            '1' = ggplot(dplyr::filter(tb, periodo == input$periodo,
                materia == input$mat1,
                prova == 'final'), aes(x = notas)) +
                geom_histogram(color = 'black', binwidth = 1, boundary = 0,
                    fill = c(rep('coral1', 4), rep('3', 3), rep('4', 3))) +
                labs(title = paste('média final de', input$mat1), x = 'notas', y = 'frequencia') +
                scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
                scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
                geom_vline(aes(xintercept = mean(notas), color = 'média'), size = 1.5) +
                scale_color_manual('', values = 'black') +
                theme_minimal(),

            ggplot(dplyr::filter(tb, periodo == input$periodo,
                materia == input$mat2,
                prova == 1), aes(x = notas)) +
                geom_histogram(color = 'black', binwidth = 1, boundary = 0,
                    fill = c(rep('coral1', 4), rep('3', 3), rep('4', 3))) +
                labs(title = paste('p1', input$mat2), x = 'notas', y = 'frequencia') +
                scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
                scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
                geom_vline(aes(xintercept = mean(notas), color = 'média'), size = 1.5) +
                scale_color_manual('', values = 'black') +
                theme_minimal())


    })


    # ==============================================================================
    output$p2 <- renderPlot({
        switch(input$periodo,
            '1' = NULL,

            ggplot(dplyr::filter(tb, periodo == input$periodo,
                materia == input$mat2,
                prova == 2), aes(x = notas)) +
                geom_histogram(color = 'black', binwidth = 1, boundary = 0,
                    fill = c(rep('coral1', 4), rep('3', 3), rep('4', 3))) +
                labs(title = paste('p2', input$mat2), x = 'notas', y = 'frequencia') +
                scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
                scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
                geom_vline(aes(xintercept = mean(notas), color = 'média'), size = 1.5) +
                scale_color_manual('', values = 'black') +
                theme_minimal())

    })

    # ==============================================================================
    output$p3 <- renderPlot({
        switch(input$periodo,
            '1' = NULL,

            '2' = ggplot(dplyr::filter(tb, periodo == input$periodo,
                materia == input$mat2,
                prova == 3), aes(x = notas)) +
                geom_histogram(color = 'black', binwidth = 1, boundary = 0,
                    fill = c(rep('coral1', 4), rep('3', 3), rep('4', 3))) +
                labs(title = paste('p3', input$mat2), x = 'notas', y = 'frequencia') +
                scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
                scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
                geom_vline(aes(xintercept = mean(notas), color = 'média'), size = 1.5) +
                scale_color_manual('', values = 'black') +
                theme_minimal())
    })
    # ==============================================================================

    output$bar <- renderPlot({
        switch(input$periodo,

            '1' = ggplot(
                dplyr::filter(tb, periodo == input$periodo,
                    materia == input$mat1, prova == 'final'),
                aes(x = factor(intervalo, levels = c('[0,4]', '(4,7]', '(7,10]')),
                    fill = intervalo)) +
                geom_bar() +
                scale_fill_manual('', values = c('coral1', '3', '4'),
                    breaks = c('[0,4]', '(4,7]', '(7,10]'),
                    labels = c('reprovaram', 'final', 'passaram')) +
                labs(x = '', y = '') +
                scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)),

            '2' = ggplot(
                dplyr::filter(tb, periodo == input$periodo,
                    materia == input$mat2, prova == 'final'),
                aes(x = factor(intervalo, levels = c('[0,4]', '(4,7]', '(7,10]')),
                    fill = intervalo)) +
                geom_bar() +
                scale_fill_manual('', values = c('coral1', '3', '4'),
                    breaks = c('[0,4]', '(4,7]', '(7,10]'),
                    labels = c('reprovaram', 'final', 'passaram')) +
                labs(x = '', y = '') +
                scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5))

        )

    })
})