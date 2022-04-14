library(tidyverse)
library(plotly)
library(latex2exp)
library(forecast)
library(patchwork)
library(kableExtra)
library(htmltools)
library(lubridate)
library(xts)

################################################################################

setwd("~/Desktop/M1 MAS/Semestre 2/Séries temporelles/Projet")

vm <- read.csv("valeurs_mensuelles.csv")

# On garde les dates et les depenses
vm_depenses <- vm[1:2]
colnames(vm_depenses) <- c("date", "depenses")
vm_depenses[,1]<-ym(vm_depenses[,1]) #convertion en dates

################################################################################

vm_depenses%>% plot_ly(type = "scatter", mode = "lines") %>% add_trace( x = ~date, y=~depenses)

################################################################################

# si on ne veut pas les dépenses non-cumulées
for (j in 1:13){
  for (i in 2:12){
    vm_depenses[i+((j-1)*12),2]<-vm_depenses[i+((j-1)*12),2]-sum(vm_depenses[(1+((j-1)*12)):((i-1)+((j-1)*12)),2])
  }
}

################################################################################

vm_depenses %>% plot_ly(type = "scatter", mode = "lines") %>% add_trace( x = ~date, y=~depenses) 

plot(xts(vm_depenses$depenses, vm_depenses$date))

################################################################################

ts.depenses = xts(vm_depenses$depenses, vm_depenses$date)
ts.depenses %>% 
  ggtsdisplay(
    plot.type = "scatter"
  )
# tendance polynomiale?

ts.depenses %>% 
  diff(
    differences = 1
  ) %>% 
  ggtsdisplay(
    plot.type = "scatter"
  )

# Pas degré 1

ts.depenses %>% 
  diff(
    differences = 2
  ) %>% 
  ggtsdisplay(
    plot.type = "scatter"
  )

################################################################################

#month plot
ts = ts(vm_depenses$depenses, frequency=12)
ggmonthplot(ts)

################################################################################

#Tendance polynomiale
plot(diff(ts, differences = 1))
abline(h=na.omit(mean(diff(ts, differences = 1))), col='red', lwd=1.5)
#degré 1 semble intéressant
acf((diff(ts, differences = 1)))

################################################################################

# on enleve la tendance (diff de 1)
serie_wo_tendance <- diff(ts, differences=1)

plot(serie_wo_tendance)

plot(diff(serie_wo_tendance, lag=12))

# on enleve la saisonnalité

acf(diff(serie_wo_tendance, lag=12))

################################################################################

#Le lag-plot : mosaïque de nuages de points pour détecter des corrélations :
gglagplot(ts, lags=12, do.lines=FALSE)

################################################################################


