library(shiny)
library(ggplot2)
library(Rcpp)


V <- 0.01
c <- 4190
u_min <- 0
u_max <- 10
ro <- 1000
Q <- 0.000033333
P <- 5580
Ti <-  5
Td <-  0.0001
kp <-  0.1
t <-  1000
Tp <-  0.1

sim_cycles <-  floor(t / Tp)
time_step <- 1:sim_cycles*Tp
e_sum <- 0

R <- rep(0,sim_cycles)
u <- rep(0,sim_cycles)
e <- rep(0,sim_cycles)
temp <- rep(0,sim_cycles)

cppFunction('void f(int sim_cycles,NumericVector &e, NumericVector &temp, NumericVector &u, const double T, const double& kp, const double& Tp, const double& Ti, int T0, const double& u_max, const double& u_min, double& e_sum, const double& Td){
    for(int i=0 ; i<sim_cycles ; ++i) {
      e[i] = T - temp[i-1];
      e_sum += e[i];
      u[i] = kp * (e[i] + (Tp / Ti) * e_sum + (Td / Tp) * (e[i] - e[i-1]));
      temp[i] = u[i] * (T - T0) / (u_max - u_min) + T0;
    }
  }')
cppFunction('void f2(int sim_cycles, NumericVector &temp) {
  for (int i=0 ; i<sim_cycles ; ++i) {
    temp[i] += temp[i-1];
  }
}')
#U[0] = U0



ui <- fluidPage(
  sliderInput(inputId = "temp0",
    label = "Choose a value",
    value = 2, min = 0, max = 10),
  sliderInput(inputId = "temp_dest",
    label = "Choose a value",
    value = 20, min = 0, max = 100),
  sliderInput(inputId = "U",
    label = "Choose a value",
    value = 250, min = 0, max = 250),
  plotOutput('val'),
  textOutput('val2')
  
)



server <- function(input, output) {
  
  temp_dest <- isolate({ input$temp_dest })
  temp[1] <- isolate({ input$temp0 })
  tddd <- temp[1]
  #f2(sim_cycles, temp)
  f(sim_cycles, e, temp, u, temp_dest, kp, Tp, Ti, tddd, u_max, u_min, e_sum, Td)
  
  
  output$val <- renderPlot({
    plot(time_step,temp)
    })
  output$val2 <- renderText({
    temp
  })
}
shinyApp(ui = ui, server = server)












