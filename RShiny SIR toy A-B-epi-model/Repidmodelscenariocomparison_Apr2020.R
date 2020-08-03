# Alex Van Plantinga
# Apr 2020
# R script to compare epidemiological model scenarios

#SIR.model()
x = SIR.model(ntrials = 10, 
              pop = 100000, 
              daysrun = 360, 
              asympt.cont = 0.4, 
              sick.cont = 0.8,
              pct.ppe = 0.80,
              ppe.effect = 0.95,
              encounter.asymptomatic.nosocdist = 6, 
              encounter.asymptomatic.socdist = 1.5, 
              encounter.sick.input = 0.5, 
              death.chance = 0.02, 
              recover.time = 23, 
              rate.asymptomatic = 0.4, 
              incub.pd = 3,
              take.action = 50,
              med.scarcity.factor = 1.2,
              hosp.beds = 1500,
              virus.evolves.mildness = "no",
              relax.socdist.mean.sick = 10)

SIR.plotter(x)

# SCENARIO WITH NO SOCIAL DISTANCING
y = SIR.model(ntrials = 1, 
              pop = 100000, 
              daysrun = 800, 
              asympt.cont = 0.2, 
              sick.cont = 0.5, 
              encounter.asymptomatic.nosocdist = 3, 
              encounter.asymptomatic.socdist = 0.9, 
              encounter.sick = 0.2, 
              death.chance = 0.005, 
              recover.time = 16, 
              rate.asymptomatic = 0.2, 
              incub.pd = 3,
              take.action = 100,
              med.scarcity.factor = 0.7,
              hosp.beds = 1500,
              virus.evolves.mildness = "yes",
              relax.socdist.mean.sick = 500)

SIR.plotter(y)

SIR.hist(x, y)




df = data.frame(1:10)
df$PPEcompliance = NA
df$deaths = NA
df$confirmed = NA
c = 0

for(iii in 1:10){

  x = SIR.model(ntrials = 10, 
                pop = 100000, 
                daysrun = 360, 
                asympt.cont = 0.4, 
                sick.cont = 0.8,
                pct.ppe = c,
                ppe.effect = 0.95,
                encounter.asymptomatic.nosocdist = 6, 
                encounter.asymptomatic.socdist = 1.5, 
                encounter.sick.input = 0.5, 
                death.chance = 0.02, 
                recover.time = 23, 
                rate.asymptomatic = 0.4, 
                incub.pd = 3,
                take.action = 50,
                med.scarcity.factor = 1.2,
                hosp.beds = 1500,
                virus.evolves.mildness = "no",
                relax.socdist.mean.sick = 10)
  
  #x$deaths.hist
  #hist(x$deaths.hist, breaks = 40)
  #SIR.plotter(x)
  df$PPEcompliance[iii] = c
  df$deaths[iii]  = mean(x$deaths.hist)/x$pop
  df$confirmed[iii] = mean(x$confirmed.hist)/x$pop
  c = c + 0.1 # update PPE compliance
}

ddf = df[4:10,2:4]
#ddf$PPEcompliance = ddf$PPEcompliance*100
library(ggplot2)
plot(ddf$PPEcompliance, ddf$deaths, type = "l",
     pch = 16, lty = 1, lwd = 1, col = "red",
     #ylim = c(0,0.6), xlim = c(0.65, 0.95)
     )
par(new = TRUE)
plot(ddf$PPEcompliance, ddf$confirmed, type = "l",
     pch = 16, lty = 1, lwd = 1, col = "orange",
     #ylim = c(0,0.6), xlim = c(0.65, 0.95)
     )



ggplot(data = df) +
geom_point(mapping = aes(x = PPEcompliance*100, y = confirmed*100),  
           size = 2, stat = "identity", color = "orange") +
  geom_line(mapping = aes(x = PPEcompliance*100, y = confirmed*100), 
            stat = "identity", color = "orange") +
  
  geom_point(mapping = aes(x = PPEcompliance*100, y = deaths*100*(50/5)), 
             size = 2, color = "darkred") + 
  geom_line(mapping = aes(x = PPEcompliance*100, y = deaths*100*(50/5), 
                          color = "darkred")) + 
  scale_y_continuous(name = "Confirmed Cases (% of Population)", 
                     sec.axis = sec_axis(~ . * 5 / 50 , 
                                         name = "Death Rate (% of Pop.)"), 
                     limits = c(0, 50)) +
  scale_x_continuous(name = "% of Population Publicly Wearing Masks", limits = c(50,100)) +
  
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.y = element_text(color = "orange"),
    axis.title.y.right = element_text(color = "darkred"),
    legend.position="none",
    text = element_text(size = 14),
    line = element_line(size = 1.1)
  ) +
  ggtitle("1yr scenario for small city\n with varying rates of wearing masks")

