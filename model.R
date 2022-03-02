library(forecast)
library(ggplot2)
library(plotly)
################################################################################

model <- function(pocetak, kraj){

  real_data <- model_data$noviSlucajevi
  original_data <- model_data$noviSlucajevi[1:(which(model_data$Datum==pocetak)-1)]
  rows <- NROW(original_data) #racuna broj redaka
  training_data<-original_data[1:(rows)] 
  N_forecasting_days <- as.numeric(kraj-pocetak)+1
  
  #NNAR Model 
  data_series<-ts(training_data)
  model_NNAR<-nnetar(data_series, size = 5)
  
  #predvidanje
  forecasting_NNAR <- forecast(model_NNAR, h=N_forecasting_days)

  #vizualizacija
  Predvidjanje <- round(forecasting_NNAR$mean)
  datum <- seq(pocetak, kraj, by="days")
  Datum <- model_data$Datum
  Slucajevi <- model_data$noviSlucajevi
  colors <- c("Slučajevi" = "tomato2", "Predviđanje" = "yellowgreen", encoding = 'UTF-8')
  krivulja_hrv <- ggplot() + 
      geom_line(aes(x=Datum, y = Slucajevi, color = "Slučajevi")) +
      geom_line(aes(x=datum, y = Predvidjanje, color = "Predviđanje")) +
      labs(title = "", x = "", y = "", color = "")
    
  graf <- ggplotly(krivulja_hrv, dynamicTicks = TRUE, tooltip = c("Datum", "datum", "Slucajevi", "Predvidjanje")) %>%
      rangeslider(thickness = 0.05) %>%
      layout(hovermode = "x unified")%>%
      layout(legend = list(orientation = "v", x = 0, y =1))
  
  return(graf)
}

box_model <- function(){
  HTML(paste0('<div class = "box_model" style = "background-color: #FFFFFF;"> 
                  <h3 class = "naslov">', "PREDVIĐANJE KRETANJA ZARAZE VIRUSOM SARS-CoV-2", '</h3>
                  <br><br>
                  <div class = "div3">
                    <p>', "Ovdje je implementiran jedan jednostavan i naivan model za predviđanje. Predviđanje se temelji na NNAR modelu (autoregresivna neuronska mreža). Podaci o zaraženima promatraju se isključivo kao vremenski niz te se ne uzima u obzir specifičnost epidemijskih podataka. Korištenjem NNAR modela napravljena je kratkoročna predikcija kretanja zaraze virusom SARS-CoV-2. Podaci korišteni za predikciju su podaci o broju novozaraženih osoba po danima, vrlo lako se može primijetiti da ti podaci na dane kada je vikend i/ili blagdan odstupaju u odnosu na ostale dane zbog manjeg broja testiranja za vrijeme vikenda i/ili blagdana. Iz navedenog razloga podaci su modificirani na način da je podatak za određeni dan dobiven kao prosjek podataka posljednja tri dana. Inicijalno je postavljeno da se predikcija računa za razdoblje od 23.8.2021. do 23.9.2021., pa su sukladno tome za treniranje korišteni podaci u razdoblju od 26.2.2020. do 22.8.2021. Razdoblje za predikciju možemo iazbrati sami, a kako se mijenja razdoblje za predikciju tako se mijenjaju i trening podaci. Rezultati predviđanja prikazani su grafom, na kojem plava linija predstavlja stvarnu situaciju kretanja zaraze, dok crvena linija prikazuje predviđanje kretanja zaraze. ", '</p>
                  </div>
              </div>'))
}
