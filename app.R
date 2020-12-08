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
    label = "Temp0.",
    value = 2, min = 0, max = 10),
  sliderInput(inputId = "temp_dest",
    label = "Temp_dest.",
    value = 20, min = 0, max = 100),
  sliderInput(inputId = "U",
    label = "Napiecie.",
    value = 250, min = 0, max = 250),
  plotOutput('val'),
  textOutput('val2')
  
)

cppFunction('void f3(NumericVector &a,NumericVector &b) {
  int n = a.size();
  for(int i=0; i<n ; ++i) {
  a[i] = i;
  b[i] = 9-i;
  }
}')

server <- function(input, output) {
  
  temp <- reactive({ c(input$temp, rep(0,sim_cycles)) })
  temp_dest <- reactive({ input$temp_dest })
  
  #f2(sim_cycles, temp)
  #reactive({f(sim_cycles, e, temp, u, temp_dest, kp, Tp, Ti, input$temp0, u_max, u_min, e_sum, Td)})
  
  
  
  obliczenia <- reactive({
    temp_l <- temp()
    temp_dest_l <- temp_dest()
    temp0_l <- input$temp0
    f(sim_cycles, e, temp_l, u, temp_dest_l, kp, Tp, Ti, temp0_l, u_max, u_min, e_sum, Td)
    temp_l
  })
  
  
  
  
  
  
  
  
  a <- reactive({ c(1,2,3) })
  b <- reactive({ c(1,2,3) })
  
  reactive({f3(a,b)})
  lala <- reactive({
    f3_a <- a()
    f3_b <- b()
    f3(f3_a,f3_b)
    c(f3_a,f3_b)
  })
  output$val <- renderPlot({
    plot(time_step,obliczenia())
    })
  output$val2 <- renderText({
    c(length(obliczenia()), length(time_step))
  })
}
shinyApp(ui = ui, server = server)












