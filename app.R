library(shiny)
library(tidyverse)
library(Rcpp)
library(shinydashboard)
library(DT)

c <- 4190           #cieplo wlasciwe
ro <- 1000          #gestosc
Q <- 0.000033333    #przeplyw wody
V <- 0.01           #objetosc zbiornika
R <- 10             #opor grzalki
t <- 600           #liczba cykli
Tp <- 0.1           #okres probkowania
sim_cycles <- t/Tp  #liczba krokow symulacji
time_step <- 1:(sim_cycles)*Tp
time_step2 <- 1:(sim_cycles+2)*Tp
u_min <- 0
u_max <- 50
UGmin <- 0
UGmax <- 350

cppFunction('void f(int sim_cycles,NumericVector &e, NumericVector &temp, NumericVector &u, const double T, const double& kp, const double& Tp, const double& Ti, int T0, double& e_sum, const double& Td, NumericVector &k1, NumericVector &k2, const double &Q, const double& SM, const double& T_cz, NumericVector &UG, NumericVector &Qw, NumericVector &Qg, double &n, NumericVector &koszt, NumericVector &uchyb){
    e[0] = 0;
    u[0] = 0;
    temp[0] = T0;
    temp[1] = T0;
    UG[0] = 25;
    Qw[0] = 0.01 * 4190 * 1000 * 0.5;
    Qg[0] = 625/11;
    for(int i=1 ; i<sim_cycles ; ++i) {
      e[i] = T - temp[i-1];
      e_sum += e[i];
      u[i] = kp * (e[i] + (Tp / Ti) * e_sum + (Td / Tp) * (e[i] - e[i-1]));
      UG[i] = u[i] * 500;
      Qw[i] = (temp[i] - (Q*T0+0.01*temp[i])/(Q+0.01))*(Q+0.01);
      Qg[i] = ((UG[i]*UG[i])/11)*n;
      if(i%10 == 0) {
        koszt[0] += Qg[i];
      }
      
        
      temp[i+1] = (Qg[i]-Qw[i])/(0.01*1000*4190) + temp[i];
    }
    koszt[0] *= 0.16 / 1000;

    uchyb[0] = abs(temp[sim_cycles] - T)/(T - T0) * 100;
    temp[sim_cycles+1] = temp[sim_cycles];

}')


ui <- dashboardPage(
  dashboardHeader(title = "Bilans energii"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Regulator PID", tabName="dashboard", icon = icon("dashboard")),
      menuItem("Wykresy", tabName="wykresy", icon = icon("chart-bar")),
      menuItem("Dane numeryczne", tabName="dane", icon = icon("stream")),
      sliderInput(inputId = "T_0", label = "Temperatura poczatkowa i temperatura zadana", value = c(20,80), min=0, max=100),
      sliderInput(inputId = "n", label = "Liczba grzalek", value = 1, min = 1, max = 4),
      numericInput("cena", "Cena kWh w groszach:", 60, min = 0, max = 500)
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          box(width=6,sliderInput(inputId="Td", label="Czas rozniczkowania (Td)", value = 0.00004, min = 0.00001, max = 0.0001, step = 0.00001 )
          ),
          box(width=6,
            sliderInput(inputId = "Ti", label = "Czas calkowania (Ti)", value = 550, min = 200, max = 1000, step = 50)
          ),
          box(width=12,
            sliderInput(inputId = "kp", label = "Wzmocnienie", value = 0.02, min = 0.01, max = 0.05, step = 0.01)
          )
          )
        ),
      tabItem(tabName = "wykresy",
        fluidRow(
          box(
            plotOutput('uchyb')
          ),
          box(
            plotOutput('ug')
          ),
          box(width = 12,
            plotOutput('temp')
          )
        )
      ),
      tabItem(tabName = "dane",
        fluidRow(
          box(width=12,
            DTOutput('val4')
          ),
          box(width = 12,
            textOutput('stats1')
          ),
          box(width = 12,
            textOutput('stats2')
          ),
          box(width = 12,
            textOutput('stats3')
          )
        )
      )
    )
  )
)



server <- function(input, output) {
  
  T_cz <- V/Q
  k1 <- rep(0,sim_cycles)
  k2 <- rep(0,sim_cycles)
  SM <- R*ro*c*Q
  u <- rep(0,sim_cycles)
  e <- rep(0,sim_cycles)
  UG <- rep(0, sim_cycles)
  Qw <- rep(0, sim_cycles)
  Qg <- rep(0,sim_cycles)
  e_sum <- 0
  koszt <- c(0)
  uchyb <- c(0)
  
  wyniki <- reactive({
    temp_out <- c(input$T_0, rep(0, sim_cycles))
    T0 <- input$T_0[1]
    T_dest <- input$T_0[2]
    kp <- input$kp
    Ti <- input$Ti
    Td <- input$Td
    n <- input$n
    

    f(sim_cycles, e, temp_out, u, T_dest, kp, Tp, Ti, T0, e_sum, Td, k1, k2, Q, SM, T_cz, UG, Qw, Qg, n, koszt, uchyb)
    dane <- data.frame(e[-1],u[-1],UG[-1], temp_out[c(-sim_cycles, -sim_cycles+1, -sim_cycles+2)])
    koszt2 <- koszt
    uchyb2 <- uchyb
    lista <- list(dane, koszt2, uchyb2)
    lista
  })
  
  koszt2 <- reactive({ koszt })
  
  data.dt <- reactive({ datatable(wyniki()[[1]], colnames = c("Uchyb", "Przeplyw", "Napiecie", "Temperatura")) })
  
  output$uchyb <- renderPlot({
    ggplot(NULL, aes(x = time_step[-1], y=wyniki()[[1]]$e)) + geom_line(colour="blue", size=0.8) + scale_x_continuous(name = "Czas [s]") + scale_y_continuous(name = "Uchyb") + theme(text = element_text(size = 17))   
  })
  output$u <- renderPlot({
    ggplot(NULL, aes(x = time_step[-1], y=wyniki()[[1]]$u)) + geom_line(colour="blue", size=0.8) + scale_x_continuous(name = "Czas [s]") + scale_y_continuous(name = "cos") + theme(text = element_text(size = 17))   
  })
  output$ug <- renderPlot({
    ggplot(NULL, aes(x = time_step[-1], y=wyniki()[[1]]$UG)) + geom_line(colour="blue", size=0.8) + scale_x_continuous(name = "Czas [s]") + scale_y_continuous(name = "Napięcie grzałki [V]") + theme(text = element_text(size = 17))   
  })
  output$temp <- renderPlot({
    ggplot(NULL, aes(x = time_step[-1], y = wyniki()[[1]]$temp_out)) + geom_line(colour="blue", size=0.8) + scale_x_continuous(name = "Czas [s]") + scale_y_continuous(name = "Temperatura [°C]") + theme(text = element_text(size = 17))   
  })


  output$val4 <- renderDT(wyniki()[[1]])
  
  output$stats1 <- renderText({
    paste("Praca wykonana przez grzałki", round(wyniki()[[2]] * input$n, 2), " [kWh]")
  })
  output$stats2 <- renderText({
    paste("Koszt energii elektrycznej: ", round(wyniki()[[2]]*input$cena/100 * input$n, 2), " PLN")
  })
  output$stats3 <- renderText({
    paste("Uchyb ustalony: ", round(wyniki()[[3]], 2), " %")
  })

}

shinyApp(ui = ui, server = server)


