# Alex Van Plantinga
# Apr and May 2020
# Plotter functions for the epidemiological model

# SIR.hist makes a histogram of models A and B

# SIR.plotter makes time series of the model results


#-------------------------
#### GGPLOT version of SIR.hist

SIR.hist = function(x, y){
  
  # Deaths histogram
  
  
  library(ggplot2)
  
  df = data.frame(100 * x$deaths.hist / x$pop, 
                  100 * y$deaths.hist / y$pop)
  colnames(df) = c("Model_A", "Model_B")
  
  df = melt(df)
  
  d.hist = ggplot( df, aes(x = value, color = variable) ) +
    
    geom_histogram( fill = "white", position = "dodge") +
    
    geom_vline(xintercept = mean(100 * x$deaths.hist / x$pop), 
               col = "magenta", lty = 2, lwd = 1.1) +
    
    geom_vline(xintercept = mean(100 * y$deaths.hist / y$pop), 
               col = "red", lty = 2, lwd = 1.1) +
    
    scale_color_manual(values=c("magenta", "red")) +
    
    scale_fill_manual(values=c("magenta", "red")) +
    
    theme( legend.position = "top") + 
    
    labs(title = "Histogram: Mortality Rate Comparison\n(dashes = model average)", 
         x = "Death Rate (% of population)",
         y = "Frequency (model trials)")
  
  d.hist
  
  #-------------------------
  
  
  # Max infections, hosp beds histogram
  
  df = data.frame(x$inf.max, 
                  y$inf.max)
  colnames(df) = c("Model_A", "Model_B")
  
  df = melt(df)
  
  max.inf.hist = ggplot( df, aes(x = value, color = variable) ) +
    
    geom_histogram( fill = "white", position = "dodge") +
    
    geom_vline(xintercept = mean(x$inf.max), 
               col = "magenta", lty = 2, lwd = 1.1) +
    
    geom_vline(xintercept = mean(y$inf.max), 
               col = "red", lty = 2, lwd = 1.1) +
    
    scale_color_manual(values=c("magenta", "red")) +
    
    scale_fill_manual(values=c("magenta", "red")) +
    
    theme( legend.position = "top") + 
    
    labs(title = "Histogram: Peak Infection Volume\n (hospital bed demand, dashes = model average)", 
         x = "Peak Infection Volume (indivuals)",
         y = "Frequency (model trials)")
  
  max.inf.hist
  
  
  library("gridExtra")
  grid.arrange(d.hist, max.inf.hist, nrow = 2)
  
}





SIR.plotter = function(x){

# average time series for ntrials
  
  library(ggplot2)
  library(reshape2)
  
daysrun = x$daysrun
  
  
if(daysrun == 1){

succeptible = x$succeptible
pop = x$pop
removed = x$removed
recovered = x$recovered
confirmed = x$confirmed
daysrun  = x$daysrun
deaths = x$deaths
sickvolume = x$infected.sick

}else({
  
  
  deaths = round(rowMeans(simplify2array( x$deaths.list )))
  succeptible = round(rowMeans(simplify2array( x$succeptible.list )))
  removed = round(rowMeans(simplify2array( x$removed.list )))
  recovered = round(rowMeans(simplify2array( x$recovered.list )))
  confirmed = round(rowMeans(simplify2array( x$confirmed.list )))
  daysrun = x$daysrun
  pop = x$pop
  sickvolume = round(rowMeans((simplify2array((x$sick.volume)))))
  
})

df = cbind(c(1:daysrun), succeptible, removed, recovered, confirmed, deaths, sickvolume)

colnames(df)[1] = "days"

df = melt(df, id = "days")
df = df[ (daysrun + 1 ) : nrow(df) , ]

colnames(df)[which(colnames(df) == "Var2")] = "Category"

p =   
  ggplot(df, aes(x = Var1, y = value)) +
  geom_area(aes(color = Category, fill = Category), 
            alpha = 0.02, 
            position = position_dodge(0.8)
  )  + geom_point(aes(color = Category, fill = Category)
  )  +  scale_color_manual(
    values = c("blue", "black", "green", "gold",   "darkred", "magenta")) + 
  scale_fill_manual(
        values = c("blue", "black", "green", "gold",   "darkred", "magenta")
      ) + labs(x = "Days", 
               y = "Cases", 
               title = paste(pop, "population,", daysrun, "days,\n", 
                             deaths[daysrun], "deaths,", 
                             removed[daysrun], "true removal,", 
                             confirmed[daysrun], " recorded", sep = " ")) + 
  scale_y_log10() + theme_minimal()


return(p)

}

