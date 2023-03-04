dashboardPage(
    skin = 'green',
    dashboardHeader(title = 'Notas GRR2022'),
    dashboardSidebar(
        sidebarMenu(
            menuItem('Provas', tabName = 'provas', icon = icon('book'))
        )
    ),

    dashboardBody(
        tabItems(
            tabItem('provas', fluidPage(
                fluidRow(
                    column(6, box(selectInput('periodo', 'período', choices = c('1', '2'), '1'), width = 12)),
                    column(6, box(selectInput('materia', 'matéria', c('est.básica', 'matriz', 'função'), 'est.básica'), width = 12))),

                fluidRow(
                    column(6, box(plotOutput('p1'), width = 12)),
                    column(6, box(plotOutput('p2'), width = 12))),

                fluidRow(
                    column(6, box(plotOutput('p3'), width = 12)),
                    column(6, box(plotOutput('bar'), width = 12)))
            ))

        )
    )
)