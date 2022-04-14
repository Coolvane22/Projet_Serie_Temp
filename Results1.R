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

# si on ne veut pas cumulé
for (j in 1:13){
  for (i in 2:12){
     vm_depenses[i+((j-1)*12),2]<-vm_depenses[i+((j-1)*12),2]-sum(vm_depenses[(1+((j-1)*12)):((i-1)+((j-1)*12)),2])
   }
 }
 
vm_depenses%>% plot_ly(type = "scatter", mode = "lines") %>% add_trace( x = ~date, y=~depenses) 
plot(xts(vm_depenses$depenses, vm_depenses$date))

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

# degré 2 semble interessant