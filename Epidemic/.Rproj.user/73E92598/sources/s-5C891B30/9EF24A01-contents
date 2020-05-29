# Alex Van Plantinga
# Apr 2020
# R script to compare epidemiological model scenarios

#SIR.model()
x = SIR.model(ntrials = 1, 
              pop = 100000, 
              daysrun = 100, 
              asympt.cont = 0.15, 
              sick.cont = 0.45, 
              encounter.asymptomatic.nosocdist = 8, 
              encounter.asymptomatic.socdist = 0.7, 
              encounter.sick.input = 0.15, 
              death.chance = 0.02, 
              recover.time = 16, 
              rate.asymptomatic = 0.2, 
              incub.pd = 3,
              take.action = 1000,
              med.scarcity.factor = 0.9,
              hosp.beds = 1500,
              virus.evolves.mildness = "no",
              relax.socdist.mean.sick = 10)

x$deaths.hist
hist(x$deaths.hist, breaks = 40)

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




