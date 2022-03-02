library(ggplot2)
library(plotly)

################################################################################

################## GRAFOVI ##################################

graf1 <- function(){  
  colors <- c("Slučajevi" = "tomato2", "Oporavljeni" = "yellowgreen", encoding = 'UTF-8')
  krivulja_hrv <- ggplot(podaci_hrv, aes(x=Datum)) + 
    geom_point(aes(y=SlucajeviHrvatska,  color = "Slučajevi", text=sprintf("Slučajevi: %d", SlucajeviHrvatska)), stat="identity", alpha=0.4) +
    geom_point(aes(y=IzlijeceniHrvatska, color = "Oporavljeni", text=sprintf("Oporavljeni: %d", IzlijeceniHrvatska)), stat="identity", alpha=0.4) +
    labs(title = "Epidemijska krivulja Hrvatska", x = "", y = "", color = "") +
    scale_color_manual(values = colors)
  
  
  ggplotly(krivulja_hrv, dynamicTicks = TRUE, tooltip = c("Datum", "text")) %>%
    rangeslider(thickness = 0.05) %>%
    layout(hovermode = "x unified")%>%
    layout(legend = list(orientation = "v", x = 0, y =1))
}


graf2 <- function(){
  colors <- c("Slučajevi" = "tomato2", "Oporavljeni" = "yellowgreen", "Umrli" = "black")
  krivulja_umrli <- ggplot(podaci_hrv, aes(x=Datum)) + 
    geom_point(aes(y=UmrliHrvatska, color = "Umrli", text=sprintf("Umrli: %d", UmrliHrvatska)), stat="identity", alpha=0.4) +
    labs(title = "Epidemijska krivulja umrlih Hrvatska", x = "", y = "", color = "") +
    scale_color_manual(values = colors)
  
  ggplotly(krivulja_umrli, dynamicTicks = TRUE, tooltip = c("Datum", "text")) %>%
    rangeslider(thickness = 0.05) %>%
    layout(hovermode = "x unified") %>%
    layout(legend = list(orientation = "v", x = 0, y =1))
  
}


graf3 <- function(){
  histogram_hrv_podaci <-
    ggplot(podaci_hrv, aes(x=Datum, y=noviSlucajevi, text=sprintf("Novi slučajevi: %d", noviSlucajevi))) + 
    geom_bar(stat = "identity", color='steelblue')  + 
    labs(title = "Epidemijska krivulja zaraženih po danima", x = "", y = "", color = "", encoding = 'UTF-8')
  
  ggplotly(histogram_hrv_podaci, dynamicTicks = TRUE, tooltip = c("Datum", "text")) %>%
    rangeslider(thickness = 0.05) %>%
    layout(hovermode = "x unified")
}


graf4 <- function(){
  #epidemijska krivulja Hrvatska cijepljeni jednom dozom i dvije doze
  colors <- c("Jedna doza" = "goldenrod1", "Dvije doze" = "mediumpurple3", encoding = 'UTF-8')
  cijepljeni_hrv <- ggplot(podaci_hrv, aes(x=Datum)) + 
    geom_point(aes(y=CijepljeniJednomDozom, color = "Jedna doza", text=sprintf("Jedna doza: %d", CijepljeniJednomDozom)), alpha=0.4) +
    geom_point(aes(y=CijepljeniDvijeDoze, color = "Dvije doze", text=sprintf("Dvije doze: %d", CijepljeniDvijeDoze)), alpha=0.4) +
    labs(title = "Cijepljenje", x = "", y = "", color = "") +
    scale_color_manual(values = colors)  
  
  ggplotly(cijepljeni_hrv, dynamicTicks = TRUE, tooltip = c("Datum", "text")) %>%
    rangeslider(thickness = 0.05) %>%
    layout(hovermode = "x unified")%>%
    layout(legend = list(orientation = "v", x = 0, y =1))
}

######################################

box_cijepljeni <- function(){
  HTML(paste0('<div class = "voronoys-block" style = "background-color: #1569C7;"> 
                  <h4 class = "name">', "CIJEPLJENI", '</h4>
                  <div>
                    <p class="tekst">', "posljednja 24h: ", podaci_hrv$CijepljeniUProtekla24[1], '</p>
                    <p class="tekst">', "dvije doze: ", podaci_hrv$CijepljeniDvijeDoze[1], '</p>
                    <p class="tekst">', "barem jedna doza: ", podaci_hrv$CijepljeniJednomDozom[1], '</p>
                  </div>
              </div>'))
}

box_umrli <- function(){
  HTML(paste0('<div class = "voronoys-block" style = "background-color: #657383;"> 
                  <h4 class = "name">', "UMRLI", '</h4>
                  <div>
                    <p class="tekst">', "posljednja 24h: ", podaci_hrv$UmrliHrvatska[1]-podaci_hrv$UmrliHrvatska[2], '</p>
                    <p class="tekst">', "ukupno: ", podaci_hrv$UmrliHrvatska[1], '</p>
                  </div>
              </div>'))
}

box_zarazeni <- function(){
  HTML(paste0('<div class = "voronoys-block" style = "background-color: #E42217;"> 
                  <h4 class = "name">', "ZARAŽENI", '</h4>
                  <div>
                    <p class="tekst">', "posljednja 24h: ", podaci_hrv$SlucajeviHrvatska[1]-podaci_hrv$SlucajeviHrvatska[2], '</p>
                    <p class="tekst">', "trenutno: ",podaci_hrv$SlucajeviHrvatska[1] - podaci_hrv$IzlijeceniHrvatska[1] - podaci_hrv$UmrliHrvatska[1], '</p>
                    <p class="tekst">', "ukupno: ", podaci_hrv$SlucajeviHrvatska[1], '</p>
                  </div>
              </div>'))
}

box_oporavljeni <- function(){
  HTML(paste0('<div class = "voronoys-block" style = "background-color: #4CC552;"> 
                  <h4 class = "name">', "OPORAVLJENI", '</h4>
                  <div>
                    <p class="tekst">', "posljednja 24h: ", podaci_hrv$IzlijeceniHrvatska[1]-podaci_hrv$IzlijeceniHrvatska[2], '</p>
                    <p class="tekst">', "ukupno: ",podaci_hrv$IzlijeceniHrvatska[1] , '</p>
                  </div>
              </div>'))
}