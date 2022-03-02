library(jsonlite)
library(lubridate)
library(dplyr)

################################################################################

#pretvara json file u csv file
json_to_csv <- function(url){
  
  my.JSON <- fromJSON(url, flatten = TRUE)
  data_frame <- as.data.frame(my.JSON)
  return(data_frame)
}

#obrada podataka
podaci_hrv <- json_to_csv(url = "https://www.koronavirus.hr/json/?action=podaci")
podaci_hrv[,11] <- round_date(as.Date(podaci_hrv[,11], origin='01-01-1997'), "day")
podaci_hrv <- podaci_hrv %>% mutate_if(is.character,as.numeric)
podaci_hrv <- podaci_hrv[rev(order(podaci_hrv$Datum)),]


slucajevi_hr <- data.frame(podaci_hrv$SlucajeviHrvatska[1]-podaci_hrv$SlucajeviHrvatska[2])
svijet <- data.frame(podaci_hrv$SlucajeviSvijet[1]-podaci_hrv$SlucajeviSvijet[2])
for(x in 2:(length(podaci_hrv$SlucajeviHrvatska)-1)){
  novi_hr <- podaci_hrv$SlucajeviHrvatska[x] - podaci_hrv$SlucajeviHrvatska[x+1]
  novi_svijet <- podaci_hrv$SlucajeviSvijet[x] - podaci_hrv$SlucajeviSvijet[x+1]
  if(novi_svijet<0){
    novi_svijet <- 0
  }
  slucajevi_hr <- rbind(slucajevi_hr, novi_hr)
  svijet <- rbind(svijet, novi_svijet)
}


slucajevi_hr <- rbind(slucajevi_hr, podaci_hrv$SlucajeviHrvatska[length(podaci_hrv$SlucajeviHrvatska)])
svijet <- rbind(svijet, podaci_hrv$SlucajeviSvijet[length(podaci_hrv$SlucajeviSvijet)])
colnames(slucajevi_hr) <- c("noviSlucajevi")
colnames(svijet) <- c("noviSvijet")
podaci_hrv <- cbind(podaci_hrv, slucajevi_hr, svijet)

rm(slucajevi_hr, svijet, novi_hr, novi_svijet, x)


podaci_cijepljenje <- json_to_csv(url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.json")
svijet <- podaci_cijepljenje$data[podaci_cijepljenje$country == "World"]
rm(podaci_cijepljenje)
svijet_cijepljeni <- data.frame(Reduce(rbind, svijet))
rm(svijet)
svijet_cijepljeni$date <- as.Date(svijet_cijepljeni$date)

######## PODACI ZA MODEL #################

model_data <-data.frame(rev(podaci_hrv$noviSlucajevi), rev(podaci_hrv$Datum))
for(x in 3:length(podaci_hrv$noviSlucajevi)){
  model_data$rev.podaci_hrv.noviSlucajevi.[x] <-  round(mean(c(rev(podaci_hrv$noviSlucajevi)[x], rev(podaci_hrv$noviSlucajevi)[x-1], rev(podaci_hrv$noviSlucajevi)[x-2])))
}
names(model_data)[1] <- "noviSlucajevi"
names(model_data)[2] <- "Datum"

