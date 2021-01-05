library(shiny)
library(tidyverse)
library(Rcpp)
library(shinydashboard)


c <- 4190           #cieplo wlasciwe
ro <- 1000          #gestosc
Q <- 0.000033333    #przeplyw wody
V <- 0.05           #objetosc zbiornika
R <- 10             #opor grzalki
t <- 30000          #liczba cykli
Tp <- 0.1           #okres probkowania
sim_cycles <- t/Tp  #liczba krokow symulacji

cppFunction('void f(int sim_cycles,NumericVector &e, NumericVector &temp, NumericVector &u, const double T, const double& kp, const double& Tp, const double& Ti, int T0, const double& u_max, const double& u_min, double& e_sum, const double& Td){
    for(int i=0 ; i<sim_cycles ; ++i) {
      e[i] = T - temp[i-1];
      e_sum += e[i];
      u[i] = kp * (e[i] + (Tp / Ti) * e_sum + (Td / Tp) * (e[i] - e[i-1]));
      temp[i] = u[i] * (T - T0) / (u_max - u_min) + T0;
    }
}')


#reactive
#Td Ti kp UG


ui <- dashboardPage(
  dashboardHeader(title = "Bilans energii"),
  dashboardSidebar(),
  
  dashboardBody(
    fluidRow(
      box(
        sliderInput(inputId="suwak",label="costam", value = 1, min = 1, max = 10)
      ),
      
      box(
        plotOutput('suw')
      )
    )
  )
)



server <- function(input, output) {
  output$suw <- renderPlot({ ggplot() + geom_histogram(aes(rnorm(input$suwak))) })
}

shinyApp(ui = ui, server = server)



