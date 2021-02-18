library(shiny)
library(tidyverse)
library(Rcpp)
library(shinydashboard)
library(DT)

c <- 4190           #cieplo wlasciwe
ro <- 1000          #gestosc
Q <- 0.000033333    #przeplyw wody
V <- 0.05           #objetosc zbiornika
R <- 10             #opor grzalki
t <- 3000           #liczba cykli
Tp <- 0.1           #okres probkowania
sim_cycles <- t/Tp  #liczba krokow symulacji
time_step <- 1:(sim_cycles)*Tp
time_step2 <- 1:(sim_cycles+2)*Tp
u_min <- 0
u_max <- 50
UGmin <- 0
UGmax <- 350
#e_sum <- 0

cppFunction('void f(int sim_cycles,NumericVector &e, NumericVector &temp, NumericVector &u, const double T, const double& kp, const double& Tp, const double& Ti, int T0, double& e_sum, const double& Td, NumericVector &k1, NumericVector &k2, const double &Q, const double& SM, const double& T_cz){
    for(int i=0 ; i<sim_cycles ; ++i) {
      e[i] = T - temp[i-1];
      e_sum += e[i];
      u[i] = kp * (e[i] + (Tp / Ti) * e_sum + (Td / Tp) * (e[i] - e[i-1]));
      k1[i] = -(temp[i] - T0) / Q;
      k2[i] = 2*u[i]/SM;
      temp[i] = ( - ( (k1[i] * Q) + (temp[i-1] - T0) + k2[i]*(u[i] - u[i-1]) )/T_cz );
     
    }
}')

# temp[i] = u[i] * (T - T0) / (u_max - u_min) + T0;
#reactive
#Td Ti kp UG

ui <- dashboardPage(
  dashboardHeader(title = "Bilans energii"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Regulator PID", tabName="dashboard", icon = icon("dashboard")),
      menuItem("Wykresy", tabName="wykresy", icon = icon("chart-bar")),
      menuItem("Dane numeryczne", tabName="dane", icon = icon("stream")),
      sliderInput(inputId = "T_0", label = "Temperatura poczatkowa i temperatura zadana", value = c(0,100), min=0, max=100)
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          box(width=6,sliderInput(inputId="Td", label="Czas rozniczkowania", value = 0.2, min = 0.2, max = 1, step = 0.2 )
          ),
          box(width=6,
            sliderInput(inputId = "Ti", label = "Czas calkowania", value = 1, min = 1, max = 10)
          ),
          box(width=12,
            sliderInput(inputId = "kp", label = "Wzmocnienie", value = 0.1, min = 0.1, max = 1, step = 0.05)
          )
          )
        ),
      tabItem(tabName = "wykresy",
        fluidRow(
          column(12,align="center",
          box(
            plotOutput('val')
          ),
          box(
            plotOutput('val5')
          ),
          box(
            plotOutput('val3')
          ),
          box(
            plotOutput('val6')
          )
          )
        )
      ),
      tabItem(tabName = "dane",
        fluidRow(
          box(width=12,
            DTOutput('val4')
          )
        )
      )
    )
  )
)



server <- function(input, output) {
  output$suw <- renderPlot({ ggplot() + geom_histogram(aes(rnorm(input$suwak))) })
  
  T_cz <- V/Q
  k1 <- rep(0,sim_cycles)
  k2 <- rep(0,sim_cycles)
  SM <- R*ro*c*Q
  u <- rep(0,sim_cycles)
  e <- rep(0,sim_cycles)
  e_sum <- 0
  
  wyniki <- reactive({
    temp_out <- c(input$T_0, rep(0, sim_cycles))
    T0 <- input$T_0[1]
    T_dest <- input$T_0[2]
    kp <- input$kp
    Ti <- input$Ti
    Td <- input$Td

    f(sim_cycles, e, temp_out, u, T_dest, kp, Tp, Ti, T0, e_sum, Td, k1, k2, Q, SM, T_cz)
    dane <- data.frame(e,u,k1,k2, temp_out[c(-1,-2)])
    dane
  })
  
  output$val <- renderPlot({
    ggplot(NULL, aes(x = time_step, y=wyniki()$e)) + geom_point()
  })
  output$val3 <- renderPlot({
    ggplot(NULL, aes(x = time_step, y=wyniki()$u)) + geom_point()
  })
  output$val5 <- renderPlot({
    ggplot(NULL, aes(x = time_step, y=wyniki()$k1)) + geom_point()
  })
  output$val6 <- renderPlot({
    ggplot(NULL, aes(x = time_step, y = wyniki()$temp_out)) + geom_point()
  })
  output$val2 <- renderText({
    c(length(wyniki()$e), length(time_step),length(wyniki()$u),length(wyniki()$k1))
  })

  output$val4 <- renderDT(wyniki())
}

shinyApp(ui = ui, server = server)


