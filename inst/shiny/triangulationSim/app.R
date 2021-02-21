

library(shiny)
library(shinyWidgets)
library(progressr)

# Define UI for application that draws a histogram
ui <- fixedPage(

    titlePanel("Triangulation sim"),
    # theme = bslib::bs_theme(version = 4),

    # useShinyjs(),

    setBackgroundColor(
        color = c("#FFFFFF"),
        gradient = "linear",
        direction = "bottom"
    ),

    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #A9A9A9;}")),
        tags$style(
            HTML(
                ".shiny-notification {
                        position:fixed;
                        top: calc(20%);
                        left: calc(40%);
                    }"
            )
        )
    ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            splitLayout(
                numericInput(
                    "size",
                    "Voter sample size",
                    1000,
                    min = 10,
                    max = 100000,
                    step = 10
                ),
                numericInput(
                    "center",
                    "Ideology center",
                    0,
                    min = -10,
                    max = 10,
                    step = 1
                )
            ),
            splitLayout(
                numericInput(
                    "deviation",
                    "Ideology std. dev.",
                    3,
                    min = -10,
                    max = 10,
                    step = 1
                ),
                numericInput(
                    "LeftPosition",
                    "Left pos. start",
                    1,
                    min = -10,
                    max = 10,
                    step = 1
                )
            ),
            splitLayout(
                numericInput(
                    "RightPosition",
                    "Right pos. start",
                    -2,
                    min = -10,
                    max = 10,
                    step = 1
                ),
                numericInput(
                    "ElectionGravity",
                    "Election gravity",
                    0.1,
                    min = 0,
                    max = 1,
                    step = 0.1
                )
            ),
            splitLayout(
                numericInput(
                    "LeftWinGravity",
                    "Left win gravity",
                    0.05,
                    min = 0.01,
                    max = 1,
                    step = 0.01
                ),
                numericInput(
                    "RightWinGravity",
                    "Right win gravity",
                    0.05,
                    min = 0.01,
                    max = 1,
                    step = 0.01
                )
            ),
            splitLayout(
                numericInput(
                    "randomness",
                    "Randomness",
                    0.1,
                    min = 0,
                    max = 1,
                    step = 0.01
                ),
                numericInput(
                    "Shift",
                    "Shift",
                    0,
                    min = 0,
                    max = 1,
                    step = 0.01
                )
            ),
            splitLayout(
                numericInput(
                    "TriangulationShift",
                    "Triangulation shift",
                    0.1,
                    min = 0,
                    max = 1,
                    step = 0.01
                ),
                numericInput(
                    "elections",
                    "elections",
                    20,
                    min = 1,
                    max = 100,
                    step = 1
                )
            ),
            div(
                style="display: inline-block;vertical-align:top; width: 125px;",
                numericInput(
                    "trials",
                    "trials",
                    1,
                    min = 0,
                    max = 100,
                    step = 1
                )
            ),
            br(),
            actionButton(
                "startSim",
                "START"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(
        input$startSim,
        {
            output$distPlot <- renderPlot(
                {
                    withProgress(
                        triangulationSim::triangulation_sim(
                            size = isolate(input$size),
                            center = isolate(input$center),
                            deviation = isolate(input$deviation),
                            LeftPosition = isolate(input$LeftPosition),
                            RightPosition = isolate(input$RightPosition),
                            ElectionGravity = isolate(input$ElectionGravity),
                            LeftWinGravity = isolate(input$LeftWinGravity),
                            RightWinGravity = isolate(input$RightWinGravity),
                            randomness = isolate(input$randomness),
                            Shift = isolate(input$Shift),
                            TriangulationShift = isolate(input$TriangulationShift),
                            elections = isolate(input$elections),
                            trials = isolate(input$trials)
                        ),
                        message = "Simulating voter ideology..."
                    )
                }
            )
        }
    )

}


# Run the application
shinyApp(ui = ui, server = server)
