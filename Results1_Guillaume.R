library(tidyverse)
library(plotly)
library(latex2exp)
library(forecast)
library(patchwork)
library(kableExtra)
library(htmltools)
library(lubridate)
library(xts)

vm <- read.csv("data/valeurs_mensuelles.csv", encoding = "utf-8")

# On garde les annees et les depenses

vm_depenses <- vm[1:2]
colnames(vm_depenses) <- c("date", "depenses")
vm_depenses[,1]<-ym(vm_depenses[,1])
class(vm_depenses[,1])
vm_depenses%>% plot_ly(type = "scatter", mode = "lines") %>% add_trace( x = ~date, y=~depenses)
test <- diff(ts.depenses)

# si on ne veut pas cumulé
for (j in 1:13){
  for (i in 2:12){
     vm_depenses[i+((j-1)*12),2]<-vm_depenses[i+((j-1)*12),2]-sum(vm_depenses[(1+((j-1)*12)):((i-1)+((j-1)*12)),2])
   }
 }
 
vm_depenses%>% plot_ly(type = "scatter", mode = "lines") %>% add_trace( x = ~date, y=~depenses) 


ts.depenses = xts(vm_depenses$depenses, vm_depenses$date)
ts.depenses %>% 
  ggtsdisplay(
    plot.type = "scatter"
  )

ts %>% 
  ggtsdisplay(
    plot.type = "scatter"
  )
ts = ts(vm_depenses$depenses, frequency=12)
forecast::ggmonthplot(ts)
# tendance polynomiale?

pacf(ts.depenses)

ts.depenses %>% 
  diff(
    differences = 1
  ) %>% 
  ggtsdisplay(
    plot.type = "scatter"
  )

plot(diff(ts, differences = 1))
abline(h=mean(na.omit((diff(ts, differences = 1)))), col="red", lwd=1)

# degré 1 semble interessant
acf((diff(ts, differences = 1)))

# on enleve la tendance ( diff de 1)
serie_wo_tendance <- diff(ts, differences=1)

plot(serie_wo_tendance)

plot(diff(serie_wo_tendance, lag=12))

# on enleve la saisonnalité

acf(diff(serie_wo_tendance, lag=12))
