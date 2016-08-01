## test app
# Web app for demo datatable structure and capabilities

library(shiny)
library(DT)
library(nycflights13)

ui <- fluidPage(
    title = "DT Demo",
    h2("Data Table Options Demo"),
    fluidRow(
        column(6,
            p("this app demonstrates two modes of navigation in Data Table"),
            tags$li("scrolling"),
            tags$li("navigation"),
            p("and the UX implications of server-side/client-side setting and dataset size")
                
        )
    ),
    fluidRow(
        column(3,
               radioButtons(
                   'rdb_nav',
                   label = "Navigation mode",
                   choices = c("scrolling", "pagination")
               )

        ),
        column(3,
               radioButtons(
                   "rdb_rows",
                   "Size of dataset",
                   choices = c("small (200 rows)" = 200, 
                               "medium (2000 rows)" = 2000,
                               "large (20000 rows)" = 20000)
               )
        )
    ),
    fluidRow(
        column(6,
            
            h4('DataTable 1: Server-side processing'),
            tags$ul(
                tags$li("Data Mechanism: full dataset stays on server, data gradually loaded into web browser along with user navigation"),
                tags$li("Response Time: Stable - suitable for medium/large datasets"),
                tags$li("Button Effect: performs partial data copy/download")
            ),
            actionButton('btn_refresh1', "Refresh DT1"),
            hr(),
            wellPanel(

                DT::dataTableOutput("DT1", width = "100%")
            )

        ),
        
        column(6,
            h4('DataTable 2: full data loaded on client side (browser)'),
            tags$ul(
                tags$li("Data Mechanism: full dataset loaded into web browser in one go"),
                tags$li("Response time: Varialbe according to dataset size - suitable for small datasets"),
                tags$li("Button Effect: performs full dataset copy/download")
            ),
            actionButton('btn_refresh2', "Refresh DT2"),
            hr(),
            wellPanel(

                DT::dataTableOutput("DT2", width = "100%")
            )

        )
    )

)


server <- function(input, output, session) {
    df1 <- eventReactive(input$btn_refresh1, {
        weather[1:input$rdb_rows, 1:8]
    })
    df2 <- eventReactive(input$btn_refresh2, {
        weather[1:input$rdb_rows, 1:8]
    })
    
    # user choices
    lst_choice1 <-
        eventReactive(input$btn_refresh1, {
        list(
            extensions = switch(
                input$rdb_nav,
                scrolling = c('Scroller', 'Buttons'),
                pagination = 'Buttons'
            ),
            options = switch(
                input$rdb_nav,
                scrolling = list(
                    # Scroller
                    deferRender = TRUE,
                    scrollY = 300,
                    # vertical scroller height 300px
                    scroller = TRUE,
                    # Buttons
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                ),
                pagination = list(
                    # Buttons
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                )
            )
        )
    })

    # user choices
    lst_choice2 <-
        eventReactive(input$btn_refresh2, {
            list(
                extensions = switch(
                    input$rdb_nav,
                    scrolling = c('Scroller', 'Buttons'),
                    pagination = 'Buttons'
                ),
                options = switch(
                    input$rdb_nav,
                    scrolling = list(
                        # Scroller
                        deferRender = TRUE,
                        scrollY = 300,
                        # vertical scroller height 300px
                        scroller = TRUE,
                        # Buttons
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel')
                    ),
                    pagination = list(
                        # Buttons
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel')
                    )
                )
            )
        })
    output$DT1 <- DT::renderDataTable({
        datatable(
            data = df1(),
            rownames = FALSE,
            filter = 'none',            # none/top/bottom
            extensions = lst_choice1()$extensions,
            options = lst_choice1()$options
        )
    }, server = T)
    
    output$DT2 <- DT::renderDataTable({

        datatable(
            data = df2(),
            rownames = FALSE,
            filter = 'none',            # none/top/bottom
            extensions = lst_choice2()$extensions,
            options = lst_choice2()$options
        )
    }, server = F)

    
}

shinyApp(ui, server)
