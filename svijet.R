library(ggplot2)
library(plotly)
library(covid19.analytics)
################################################################################

karta <- function() {
  cases <- covid19.data("ts-confirmed")
  live.map(cases, select.projctn = FALSE, no.legend = TRUE, 
           title = "", szRef = 0.9)
}

karta2 <- function() {
  cases <- covid19.data("ts-deaths")
  live.map(cases, select.projctn = FALSE, no.legend = TRUE, szRef = 0.9)
}

##########################################################################


box_cijepljeni2 <- function(){
  HTML(paste0('<div class = "voronoys-block" style = "background-color: #1569C7;"> 
                  <h4 class = "name">', "CIJEPLJENI", '</h4>
                  <div>
                    <p class="tekst">', "posljednja 24h: ", svijet_cijepljeni$daily_vaccinations[length(svijet_cijepljeni$daily_vaccinations)], '</p>
                    <p class="tekst">', "potpuno cijepljeni: ", svijet_cijepljeni$people_fully_vaccinated[length(svijet_cijepljeni$people_fully_vaccinated)], '</p>
                    <p class="tekst">', "barem jedna doza: ", svijet_cijepljeni$people_vaccinated[length(svijet_cijepljeni$people_vaccinated)], '</p>
                  </div>
              </div>'))
}

box_umrli2 <- function(){
  HTML(paste0('<div class = "voronoys-block" style = "background-color: #657383;"> 
                  <h4 class = "name">', "UMRLI", '</h4>
                  <div>
                    <p class="tekst">', "posljednja 24h: ", podaci_hrv$UmrliSvijet[1] - podaci_hrv$UmrliSvijet[2], '</p>
                    <p class="tekst">', "ukupno: ", podaci_hrv$UmrliSvijet[1], '</p>
                  </div>
              </div>'))
}

box_zarazeni2 <- function(){
  HTML(paste0('<div class = "voronoys-block" style = "background-color: #E42217;"> 
                  <h4 class = "name">', "ZARAŽENI", '</h4>
                  <div>
                    <p class="tekst">', "posljednja 24h: ", podaci_hrv$SlucajeviSvijet[1] - podaci_hrv$SlucajeviSvijet[2], '</p>
                    <p class="tekst">', "trenutno: ",podaci_hrv$SlucajeviSvijet[1] - podaci_hrv$IzlijeceniSvijet[1] - podaci_hrv$UmrliSvijet[1], '</p>
                    <p class="tekst">', "ukupno: ", podaci_hrv$SlucajeviSvijet[1], '</p>
                  </div>
              </div>'))
}


box_oporavljeni2 <- function(){
  HTML(paste0('<div class = "voronoys-block" style = "background-color: #4CC552;"> 
                  <h4 class = "name">', "OPORAVLJENI", '</h4>
                  <div>
                    <p class="tekst">', "posljednja 24h: ", podaci_hrv$IzlijeceniSvijet[1] - podaci_hrv$IzlijeceniSvijet[2], '</p>
                    <p class="tekst">', "ukupno: ", podaci_hrv$IzlijeceniSvijet[1] , '</p>
                  </div>
              </div>'))
}

###############################################################################

graf11 <- function(){
  colors <- c("Slučajevi" = "tomato2", "Oporavljeni" = "yellowgreen", encoding = 'UTF-8')
  krivulja_svijet <- ggplot(podaci_hrv, aes(x=Datum)) + 
    geom_point(aes(y=SlucajeviSvijet, color = "Slučajevi", text=sprintf("Slučajevi: %d", SlucajeviSvijet)), stat="identity", alpha=0.4) +
    geom_point(aes(y=IzlijeceniSvijet, color = "Oporavljeni", text=sprintf("Oporavljeni: %d", IzlijeceniSvijet)), stat="identity", alpha=0.4) +
    #scale_y_continuous(breaks = seq(0, max(podaci_hrv$SlucajeviSvijet), by=20000)) +
    labs(title = "Epidemijska krivulja svijet", x = "", y = "", color = "") +
    scale_color_manual(values = colors) 
  
  
  ggplotly(krivulja_svijet, dynamicTicks = TRUE, tooltip = c("Datum", "text")) %>%
    rangeslider(thickness = 0.05) %>%
    layout(hovermode = "x unified")%>%
    layout(legend = list(orientation = "v", x = 0, y =1))
}


graf22 <- function(){
  colors <- c("Umrli" = "black")
  krivulja_umrli <- ggplot(podaci_hrv, aes(x=Datum)) + 
    geom_point(aes(y=UmrliSvijet, color = "Umrli", text=sprintf("Umrli: %d", UmrliSvijet)), stat="identity", alpha=0.4) +
    #scale_y_continuous(breaks = seq(0, max(podaci_hrv$UmrliSvijet), by=500)) +
    labs(title = "Epidemijska krivulja umrlih - svijet", x = "", y = "", color = "") +
    scale_color_manual(values = colors) 
  
  ggplotly(krivulja_umrli, dynamicTicks = TRUE, tooltip = c("Datum", "text")) %>%
    rangeslider(thickness = 0.05) %>%
    layout(hovermode = "x unified") %>%
    layout(legend = list(orientation = "v", x = 0, y =1))
  
}

graf33 <- function(){
  histogram_svijet_podaci <-
    ggplot(podaci_hrv, aes(x=Datum, y=noviSvijet, text=sprintf("Novi slučajevi: %d", noviSvijet))) + 
    geom_bar(stat = "identity", color='steelblue')  + 
    labs(title = "Epidemijska krivulja zaraženih po danima - svijet", x = "", y = "", color = "", encoding = 'UTF-8')

  ggplotly(histogram_svijet_podaci, dynamicTicks = TRUE, tooltip = c("Datum", "text")) %>%
    rangeslider(thickness = 0.05) %>%
    layout(hovermode = "x unified")
}

graf44 <- function(){
  Datum <- svijet_cijepljeni$date
  colors <- c("Jedna doza" = "goldenrod1", "Potpuno cijepljeni" = "mediumpurple3", encoding = 'UTF-8')
  cijepljeni_svijet <- ggplot(svijet_cijepljeni, aes(x=Datum)) + 
    geom_point(aes(y=people_vaccinated_per_hundred, color = "Jedna doza", text=sprintf("Jedna doza: %f", people_vaccinated_per_hundred)), alpha=0.4) +
    geom_point(aes(y=people_fully_vaccinated_per_hundred, color = "Potpuno cijepljeni", text=sprintf("Poptuno cijepljeni: %f", people_fully_vaccinated_per_hundred)), alpha=0.4) +
    #scale_y_continuous(breaks = seq(0, max(svijet_cijepljeni$people_vaccinated_per_hundred))) +
    labs(title = "Broj cijepljenih osoba na populaciji od 100 osoba", x = "", y = "", color = "") +
    scale_color_manual(values = colors)  
  
  ggplotly(cijepljeni_svijet, dynamicTicks = TRUE, tooltip = c("Datum","text")) %>%
    rangeslider(thickness = 0.05) %>%
    layout(hovermode = "x unified")%>%
    layout(legend = list(orientation = "v", x = 0, y =1))
}