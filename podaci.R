################################################################################

podaci_osobe <- json_to_csv(url = "https://www.koronavirus.hr/json/?action=po_osobama")
novoz <-  table(podaci_osobe$Datum)
podaci_dnevni <- as.data.frame(novoz)
podaci_dnevni[,1] <- as.Date(podaci_dnevni[,1])
colnames(podaci_dnevni) <- c('Datum', 'Slucajevi')

graf5 <- function() {
  muskarci <- length(which(podaci_osobe$spol == "M"))

  zene <- length(which(podaci_osobe$spol == "Ž"))

  podaci_spol1 <- data.frame(
    spol=factor(c("Žene","Muškarci")) ,  
    ukupno=c(zene, muskarci))
  
  podaci_spol <- ggplot(podaci_spol1, aes(x=spol, y=ukupno, fill = spol)) + 
    geom_bar(stat = "identity", width = 0.4) +
    #scale_y_continuous(breaks = seq(0, max(muskarci + zene), by=20000)) +
    scale_fill_manual(values = c("Muškarci" = "cadetblue3", "Žene" = "lightgoldenrod1"))+
    labs(title = "Ukupan broj zaraženih po spolu u Hrvatskoj", x = "", y = "", color = "")
  
  
  ggplotly(podaci_spol, dynamicTicks = TRUE, tooltip = c("ukupno")) %>%
    layout(hovermode = "x unified")%>%
    layout(showlegend = FALSE)
}


graf6 <- function() {
  dob1 <- length(which(podaci_osobe$dob <= 1930))

  dob2 <- length(which(podaci_osobe$dob <= 1940 & podaci_osobe$dob > 1930))

  dob3 <- length(which(podaci_osobe$dob <= 1950 & podaci_osobe$dob > 1940))

  dob4 <- length(which(podaci_osobe$dob <= 1960 & podaci_osobe$dob > 1950))

  dob5 <- length(which(podaci_osobe$dob <= 1970 & podaci_osobe$dob > 1960))

  dob6 <- length(which(podaci_osobe$dob <= 1980 & podaci_osobe$dob > 1970))

  dob7 <- length(which(podaci_osobe$dob <= 1990 & podaci_osobe$dob > 1980))

  dob8 <- length(which(podaci_osobe$dob <= 2000 & podaci_osobe$dob > 1990))

  dob9 <- length(which(podaci_osobe$dob <= 2010 & podaci_osobe$dob > 2000))

  dob10 <- length(which(podaci_osobe$dob > 2010))

  
  podaci_godiste <- data.frame(
    godine=factor(c("-1930","1931-1940","1941-1950","1951-1960","1961-1970","1971-1980","1981-1990","1991-2000","2001-2010","2011-")) ,  
    zarazeni=c(dob1,dob2,dob3,dob4,dob5,dob6,dob7,dob8,dob9,dob10))
  
  Godište <- podaci_godiste$godine
  podaci_godiste <- ggplot(podaci_godiste, aes(x=Godište, y=zarazeni, text=sprintf("Zaraženi: %d", zarazeni))) + 
    geom_bar(stat = "identity", fill = "#FF6666") +
    #scale_y_continuous(breaks = seq(0, max(dob1+dob2+dob3+dob4+dob5+dob6+dob7+dob8+dob9+dob10), by=20000)) +
    labs(title = "Ukupan broj zaraženih po godištu u Hrvatskoj", x = "", y = "", color = "")
  
  ggplotly(podaci_godiste, dynamicTicks = TRUE, tooltip = c("Godište", "text")) %>%
    layout(hovermode = "x unified")
}

graf7 <- function() {
  
  zagreb <- length(which(podaci_osobe$Zupanija == "Grad Zagreb"))

  dub_ner <- length(which(podaci_osobe$Zupanija == "Dubrovačko-neretvanska"))

  brod_pos <- length(which(podaci_osobe$Zupanija == "Brodsko-posavska"))

  spl_dal <- length(which(podaci_osobe$Zupanija == "Splitsko-dalmatinska"))

  os_bar <- length(which(podaci_osobe$Zupanija == "Osječko-baranjska"))

  zad <- length(which(podaci_osobe$Zupanija == "Zadarska"))

  vuk_sri <- length(which(podaci_osobe$Zupanija == "Vukovarsko-srijemska"))

  sib_kni <- length(which(podaci_osobe$Zupanija == "Šibensko-kninska"))

  var <- length(which(podaci_osobe$Zupanija == "Varaždinska"))

  bjel_bil <- length(which(podaci_osobe$Zupanija == "Bjelovarsko-bilogorska"))
  
  sis_mosl <- length(which(podaci_osobe$Zupanija == "Sisačko-moslavačka"))

  prim_gor <- length(which(podaci_osobe$Zupanija == "Primorsko-goranska"))

  zag <- length(which(podaci_osobe$Zupanija == "Zagrebačka"))

  karl <- length(which(podaci_osobe$Zupanija == "Karlovačka"))

  kop_kri <- length(which(podaci_osobe$Zupanija == "Koprivničko-križevačka"))

  lic_senj <- length(which(podaci_osobe$Zupanija == "Ličko-senjska"))

  krap_zag <- length(which(podaci_osobe$Zupanija == "Krapinsko-zagorska"))

  ist <- length(which(podaci_osobe$Zupanija == "Istarska"))

  med <- length(which(podaci_osobe$Zupanija == "Međimurska"))

  poz_sla <- length(which(podaci_osobe$Zupanija == "Požeško-slavonska"))

  vir_pod <- length(which(podaci_osobe$Zupanija == "Virovitičko-podravska"))

  
  podaci_zupanije <- data.frame(
    županija=factor(c("Grad Zagreb","Dubrovačko-neretvanska","Brodsko-posavska","Splitsko-dalmatinska","Osječko-baranjska","Zadarska","Vukovarsko-srijemska","Šibensko-kninska","Varaždinska",
                      "Bjelovarsko-bilogorska", "Sisačko-moslavačka","Primorsko-goranska","Zagrebačka","Karlovačka", "Koprivničko-križevačka", "Ličko-senjska", "Krapinsko-zagorska",
                      "Istarska", "Međimurska", "Požeško-slavonska", "Virovitičko-podravska")) ,  
    zaraženi=c(zagreb,dub_ner,brod_pos,spl_dal,os_bar,zad,vuk_sri,sib_kni,var,bjel_bil,sis_mosl,prim_gor,zag,karl,
               kop_kri,lic_senj,krap_zag, ist, med, poz_sla,vir_pod))
  
  podaci_zupanije <- ggplot(podaci_zupanije, aes(x=županija, y=zaraženi, text = sprintf("Broj zaraženih: %d", zaraženi))) + 
    geom_bar(stat = "identity", fill = "#FF6666") +
    #scale_y_continuous(breaks = seq(0, max(zagreb+dub_ner+brod_pos+spl_dal+os_bar+zad+vuk_sri+sib_kni+var+bjel_bil+sis_mosl+
    #                                         prim_gor+zag+karl+kop_kri+lic_senj+krap_zag+ist+med+poz_sla+vir_pod), by=20000))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = "Ukupan broj zaraženih po županiji u Hrvatskoj", x = "", y = "", color = "")
  
  
  ggplotly(podaci_zupanije, dynamicTicks = TRUE, tooltip = c("Ĺľupanija", "text")) %>%
    layout(hovermode = "x unified")
}
graf7()

############################################
brStanovnikaHrv <- 4076000

graf1D <- function(broj){
  colors <- c("Slučajevi" = "tomato2", "Oporavljeni" = "yellowgreen", encoding = 'UTF-8')
  krivulja_hrv <- ggplot(podaci_hrv, aes(x=Datum)) + 
    geom_point(aes(y=round(SlucajeviHrvatska/x), color = "Slučajevi", text = sprintf("Slučajevi: %d", round(SlucajeviHrvatska/x))),  alpha=0.4) +
    geom_point(aes(y=round(IzlijeceniHrvatska/x), color = "Oporavljeni", text = sprintf("Oporavljeni: %d", round(IzlijeceniHrvatska/x))), alpha=0.4) +
    #scale_y_continuous(breaks = seq(0, max(podaci_hrv$SlucajeviHrvatska), by=20000)) +
    labs(title = "Broj zaraženih na 100 000 stanovnika u Hrvatskoj", x = "", y = "", color = "") +
    scale_color_manual(values = colors)  
  
  
  ggplotly(krivulja_hrv, dynamicTicks = TRUE, tooltip = c("Datum", "text")) %>%
    rangeslider(thickness = 0.05) %>%
    layout(hovermode = "x unified")%>%
    layout(legend = list(orientation = "v", x = 0, y =1))
}
 