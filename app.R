library(shiny)
rx1 <- c("Wow - that's pretty warm! To stay cool, you'll need to drink three glasses of lemonade, two cold beers or one giant frozen margarita.")
rx2 <- c("Oh my - that's hot! To stay cool, you'll need to drink five glasses of lemonade, three cold beers or two giant frozen margaritas.")
rx3 <- c("Ouch - you're burning up! To stay cool, you'll need to drink ten glasses of lemonade, six cold beers or three giant frozen margaritas.")
rx4 <- c("Where are you, on the surface of a nearby star or something? Sorry, but science doesn't yet know how to make a big enough frozen margarita to save anyone from conditions like this.")
ui <- fluidPage(
        headerPanel("Heat Index Calculator and 
                     Recommended Fluid Intake Compensation Guide"),
        helpText("Use the sliders below to set temperature (in degrees Centigrade)
                 and relative humidity percentage. The app will generate the 
                 associated heat index and recommended fluid intake options to stay cool."),
        sliderInput(inputId = "tempc", label = "Set Temperature (Centigrade)",
                    min = 27, max = 43, value = 30, step = 1),
        sliderInput(inputId = "rhum", label = "Set Relative Humidity %", 
                    min = 40, max = 100, value = 50, step = 5),
        mainPanel(   
                textOutput("newIndex"),
                tags$head(tags$style("#newIndex{color: red;
                                     font-size: 24px;
                                     font-style: bold;
                                     }")),
                helpText(""),
                textOutput("newRX"),
                tags$head(tags$style("#newRX{color: blue;
                                     font-size: 20px;
                                     font-style: italic;
                                     }")),
                helpText(""),
                helpText("If you're looking for something to read while you're sweating, 
                         here's how the heat index is calculated:"),
                helpText("First, convert temperature from Centigrade to Fahrenheit 
                         using the formula Fahrenheit = Celsius * 9/5 + 32.
                         For temperature Fahrenheit = T and relative humidity = rh,
                         the formula for heat index is as follows:
                         -42.379 + (2.04901523 * T) + (10.14333127 * rh) + 
                         (-.22475541 * T * rh) + (-6.83783 * 10^-3 * T^2) +
                         (-5.481717 * 10^-2 * rh^2) + (1.22874 * 10^-3 * T^2 * rh) + 
                         (8.5282 * 10^-4 * T * rh^2) + (-1.99 * 10^-6 * T^2 * rh^2).")
)
)

server <- function(input, output) {
        output$newIndex <- renderText( { 
                t <- 9/5 * input$tempc + 32
                rh <- input$rhum
                indexf <- round((-42.379 + (2.04901523*t) + (10.14333127*rh) + 
                        (-.22475541*t*rh) + (-6.83783*10^-3*t^2) +
                        (-5.481717*10^-2*rh^2) + (1.22874*10^-3*t^2*rh) +
                        (8.5282*10^-4*t*rh^2) + (-1.99*10^-6*t^2*rh^2)),0)
                indexc <- round(((indexf - 32) * 5/9), 0)
                ifelse(indexc < 60, paste("At ", input$tempc, 
                " degrees Centigrade with ", input$rhum, 
                " percent relative humidity, the heat index is ", indexc, "."), 
                paste("At ", input$tempc, 
                      " degrees Centigrade with ", input$rhum, 
                      " percent relative humidity, heat stroke is imminent.
                      Don't waste these last few precious moments of your life
                      worrying about the stupid heat index."))
                })
                output$newRX <- renderText({
                        t <- 9/5 * input$tempc + 32
                        rh <- input$rhum
                        indexf <- round((-42.379 + (2.04901523*t) + (10.14333127*rh) + 
                                (-.22475541*t*rh) + (-6.83783*10^-3*t^2) +
                                (-5.481717*10^-2*rh^2) + (1.22874*10^-3*t^2*rh) +
                                (8.5282*10^-4*t*rh^2) + (-1.99*10^-6*t^2*rh^2)),0)
                        indexc <- round(((indexf - 32) * 5/9), 0)
                        paste("Recommended Fluid Intake Compensation:")
                        ifelse(indexc < 39, rx1, 
                               ifelse(indexc >= 39 & indexc < 50, rx2, 
                                      ifelse(indexc >= 50 & indexc < 60 , rx3, rx4)))
                })
}

shinyApp(ui = ui, server = server)
