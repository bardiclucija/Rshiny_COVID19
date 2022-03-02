box_opcenito <- function(){
  HTML(paste0('<div class = "opcenito" style = "background-color: #FAFAFA;"> 
                  <h3 class = "opcenito_naslov">', "Što je COVID-19?", '</h3>
                  <div>
                    <p class="tekst">', "Novi koronavirus koji je  otkriven u Kini krajem 2019. godine, nazvan je SARS-CoV-2", '</p>
                    <p class="tekst">', "COVID-19 je naziv bolesti uzrokovane SARS-CoV-2", '</p>
                    <p class="tekst">', "Iako virus potječe od životinja, on se sada širi s osobe na osobu", '</p>
                    <p class="tekst">', "Trenutno dostupni epidemiološki podaci ukazuju da se virus relativno brzo i lako širi među ljudima", '</p>
                    <p class="tekst">', "Procjenjuje se da bi jedna oboljela osoba u prosjeku mogla zaraziti dvije do tri osjetljive osobe", '</p>
                    <img src="https://www.hopkinsmedicine.org/-/media/images/health/1_-conditions/coronavirus/vaccine5.ashx" width=550px height=300px>
                  </div>
              </div>'))
}

box_simptomi <- function(){
  HTML(paste0('<div class = "simptomi" style = "background-color: #FAFAFA;"> 
                  <h3 class = "opcenito_naslov">', "Simptomi", '</h3>
                  <div>
                    <img src="https://cdn-icons-png.flaticon.com/512/2877/2877822.png" width=45px height=45px als ="Povišena tjelesna temperatura">
                    <p class="tekst">', "Povišena tjelesna temperatura", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/2877/2877820.png" width=45px height=45px>
                    <p class="tekst">', "Kašalj", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/2166/2166997.png" width=45px height=45px>
                    <p class="tekst">', "Manjak kisika", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/3781/3781970.png" width=45px height=45px>
                    <p class="tekst">', "Otežano disanje", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/3017/3017369.png" width=45px height=45px>
                    <p class="tekst">', "Umor", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/3782/3782557.png" width=45px height=45px>
                    <p class="tekst">', "Gubitak okusa i mirisa", '</p>
                  </div>
              </div>'))
}

box_prevencija <- function(){
  HTML(paste0('<div class = "prevencija" style = "background-color: #FAFAFA;"> 
                  <h3 class = "opcenito_naslov">', "Prevencija", '</h3>
                  <div>
                    <img src="https://cdn-icons-png.flaticon.com/512/2966/2966456.png" width=45px height=45px>
                    <p class="tekst">', "Cijepljenje", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/4415/4415688.png" width=45px height=45px>
                    <p class="tekst">', "Često pranje ruku", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/3165/3165220.png" width=45px height=45px>
                    <p class="tekst">', "Korištenje dezinficijensa", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/3365/3365408.png" width=45px height=45px>
                    <p class="tekst">', "Korištenje zaštitnih maski", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/2750/2750340.png" width=45px height=45px>
                    <p class="tekst">', "Održavanje socijalne distance", '</p>
                    <img src="https://cdn-icons-png.flaticon.com/512/2798/2798450.png" width=45px height=45px>
                    <p class="tekst">', "Izbjegavati dirati lice rukama", '</p>
                  </div>
              </div>'))
}



footer <- function(){
  tags$footer(id ="footer", column(6, "© Lucija Bardić i Ana Koturić, 2021."), 
              column(6, "Završni praktični projekt, Odjel za matematiku, Osijek")
  )
}

box_aplikacija <- function(){
  HTML(paste0('<div class = "aplikacija" style = "background-color: #FAFAFA;"> 
                  <h3 class = "opcenito_naslov">', "O aplikaciji", '</h3>
                  <div>
                    <p class="tekst">', "Ovu aplikaciju izradile su Lucija Bardić i Ana Koturić, studentice treće godine prediplomskog studija Matematika i računarstvo Odjela za matematiku u Osijeku u okviru završnog praktičnog projekta predviđenog studijskim programom.", '</p>
                  </div>
              </div>',
              '<div class = "aplikacija1" style = "background-color: #FAFAFA;"> 
         <h3 class = "opcenito_naslov">', "Kod", '</h3>
         <div>
         <p class="tekst">', "Aplikacija je izrađena korištenjem R Shiny tehnologije. Cjelokupan izvorni kod dostupan je na adresi: ", '</p>
         <p><a class="tekst" href="https://github.com/akoturic/COVID-19-RShiny-App">', "https://github.com/akoturic/COVID-19-RShiny-App", '</a></p>
         </div>
         </div>'))
}

box_aplikacija1 <- function(){
  HTML(paste0('<div class = "aplikacija" style = "background-color: #FAFAFA;"> 
                  <h3 class = "opcenito_naslov">', "Podaci", '</h3>
                  <div>
                    <p class="tekst">', "Za prikaz podataka korištene su javno dostupne baze podataka: ", '</p>
                    <p><a class="tekst" href="https://www.koronavirus.hr/json/?action=podaci">', "https://www.koronavirus.hr/json/?action=podaci", '</a></p>
                    <p><a class="tekst" href="https://www.koronavirus.hr/json/?action=po_osobama">', "https://www.koronavirus.hr/json/?action=po_osobama", '</a></p>
                    <p><a class="tekst" href="https://github.com/CSSEGISandData/COVID-19">', "https://github.com/CSSEGISandData/COVID-19", '</a></p>
                    <p><a class="tekst" href="https://github.com/owid/covid-19-data/tree/master/public/data">', "https://github.com/owid/covid-19-data/tree/master/public/data", '</a></p>
                  </div>
              </div>'))
}

