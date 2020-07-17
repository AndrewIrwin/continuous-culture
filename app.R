#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# functions for ODE model

library(deSolve)
library(ggplot2)
library(tidyr)

# system of equations

# dR/dt = d(R0 - R) - rho(Q)X
# dQ/dt = rho(Q) - mu(Q)
# dX/dt = X (mu(Q) - d)
# mu(Q) = mumax ( 1 - Qmin/Q)
# rho(Q) = Vmax R / (Km + R)
# conservation law: d/dt(R + XQ) = 0 = dR/dt + XdQ/dt + QdX/dt 

culture.chemostat <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
        rho = Vmax*R/(Km+R)
        mu = mumax*(1-Qmin/Q)
        dR = d*(R0 - R) - rho*X
        dQ = rho - mu
        dX = X*(mu-d)
        list(c(dR, dQ, dX))  
    })
}
# sample integration
# params <- c(mumax = 1.0, Vmax = 2.0, Km = 1.0, Qmin = 0.1, R0 = 2.0, d = 0.5)
# state <- c(R = 1.0, Q = 1.0, X = 1.0)

# culture.chemostat(0, state, params)
# times <- seq(0, 25, length = 100)
# out <- ode(y = state, times = times, func = culture.chemostat, parms = params)

plotTS <- function(out) {
  out %>% as_tibble() %>%
    mutate(log10X = log10(X)) %>%
    select(-X) %>%
    pivot_longer(R:log10X) %>%
    ggplot(aes(x=time, y = value )) + 
    geom_line() + 
    facet_grid( name ~ . , scale="free_y") + 
    theme_bw()
}

# functions for shiny app

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Continuous culture"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mumax", "mumax:", min = 0, max = 2, value = 1.00, step = 0.05),
            sliderInput("d", "d:", min = 0, max = 2, value = 0.50, step = 0.05),
            sliderInput("Vmax", "Vmax:", min = 0, max = 2, value = 2.00, step = 0.05),
            sliderInput("Km", "Km:", min = 0, max = 2, value = 1.00, step = 0.05),
            sliderInput("Qmin", "Qmin:", min = 0, max = 2, value = 0.10, step = 0.05),
            sliderInput("R0", "R0:", min = 0, max = 2, value = 2.00, step = 0.05),
            sliderInput("R", "Q:", min = 0, max = 2, value = 1.00, step = 0.05),
            sliderInput("Q", "Q:", min = 0, max = 2, value = 1.00, step = 0.05),
            sliderInput("X", "X:", min = 0, max = 2, value = 1.00, step = 0.05)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("tsPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$tsPlot <- renderPlot({
        # set parameters
        params <- c(mumax = input$mumax, Vmax = input$Vmax, Km = input$Km, Qmin = input$Qmin, R0 = input$R0, d = input$d)
        state <- c(R = input$R, Q = input$Q, X = input$X)
        
        # generate model output
        times <- seq(0, 25, length = 100)
        out <- ode(y = state, times = times, func = culture.chemostat, parms = params)
        
        # plot time series
        plotTS(out)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
