# Alex Van Plantinga
# April 2020
# Coronavirus-inspired 
# psuedo Markov chain 
# SIR Toy model for a small city 10,000 - 200,000 people
# "Balance the model and flatten the curve"


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## Only run examples in interactive R sessions

ui <- fluidPage(shinyjs::useShinyjs(),
  
  titlePanel("Small City Epidemiological Model"),
  
  conditionalPanel(condition="input.plot_tabs != 'User guide'",
                   
                   tabsetPanel(id="ui_tab",
                               tabPanel("Model A",
                                        
                                        # Sidebar layout with input and output definitions ----
                                        sidebarLayout(
                                          
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            
                                            
                                            
                                            # Input: actionButton() to defer the rendering of output ----
                                            # until the user explicitly clicks the button (rather than
                                            # doing it immediately when inputs change). This is useful if
                                            # the computations required to render output are inordinately
                                            # time-consuming.
                                            actionButton("update", "Plot Model Results"),
                                            
                                            # Input: Specify the number of observations to view ----
                                            textInput("modelname", "Name your model (what kind of disease):", "Coronavirus (viral pneumonia)"),
                                            helpText(""),
                                            
                                            # Input: Specify the number of observations to view ----
                                            numericInput("ntrials", "Number of trials the model is run (more trials takes longer to run):", 4),
                                            helpText(""),
                                            
                                            numericInput("pop", "City population:", 100000),
                                            helpText(""),
                                            
                                            numericInput("daysrun", "Time in days the model simulates:", 120),
                                            helpText("More days may slow the model run."),
                                            
                                            
                                            numericInput("pct.ppe", "What percent of people wear PPE (masks)? (from 0 to 1):", 0.9),
                                            helpText(""),
                                            
                                            
                                            numericInput("ppe.effect", "How effective is the PPE/mask at preventing spread? (from 0 to 1):", 0.95),
                                            helpText(""),
                                            
                                            numericInput("asympt.cont", "How contagious is an asymptomatic person? (from 0 to 1):", 0.35),
                                            helpText(""),
                                            
                                            numericInput("sick.cont", "How contagious is a sick person? (from 0 to 1):", 0.75),
                                            helpText(""),
                                            
                                            numericInput("encounter.asymptomatic.nosocdist", "Encounters per day of asymptomatic people who do not practice distancing:", 6),
                                            helpText(""),
                                            
                                            numericInput("encounter.asymptomatic.socdist", "Encounters per day of asymptomatic people who practice distancing:", 1.5),
                                            helpText(""),
                                            
                                            numericInput("encounter.sick.input", "Encounters per day of people suffering from flu symptoms:", 0.4),
                                            helpText(""),
                                            
                                            numericInput("death.chance", "How deadly is the disease (from 0 to 1):", 0.02),
                                            helpText("Many flat-curve countries average at 1/21"),
                                            
                                            numericInput("recover.time", "Days after infection it takes for an infected person to recover:", 18),
                                            helpText(""),
                                            
                                            numericInput("rate.asymptomatic", "Fraction of infected people who are asymptomatic (from 0 to 1):", 1/5),
                                            helpText("~1/5 for Coronavirus"),
                                            
                                            numericInput("incub.pd", "Days from transmission to manifesting symptoms (incubation period):", 3),
                                            helpText(""),
                                            
                                            numericInput("take.action", "How many people get sick in your city before imposing social distancing?:", 100),
                                            helpText("The model is seeded with 3 sick and 2 asymptomatic people"),
                                            
                                            numericInput("med.scarcity.factor", "What effect does the volume of sick cases have on death rate?:", 0.7),
                                            helpText("Higher ratio of sick cases to population drives medical scarcity. A value of 0 assumes no effect. A value of >1 intensifies the effect."),
                                            
                                            numericInput("hosp.beds", "How many hospital beds are there in your city?:", 2500),
                                            helpText(""),
                                            
                                            selectInput("virus.evolves.mildness", "Simulate progressively lower death rate as removal increases? ", c("no" = "no", "yes" = "yes")),
                                            helpText("Does the virus evolve to be less deadly?"),
                                            
                                            numericInput("relax.socdist.mean.sick", "Relax social distancing when new confirmed cases goes below:", 10), 
                                            helpText("weekly mean of daily new confirmed cases")
                                            
                                            
                                          ),# Show a plot of the generated distribution
                                          mainPanel(fluidRow(column(10,
                                                                    h4("Time Series"),
                                                                    pre(id = "console"), 
                                                                  plotOutput("TSPlot")
                                          )))
                                          
                                        )
                                        
                               )
                               ,
                               tabPanel("Model B",
                                        # Sidebar layout with input and output definitions ----
                                        sidebarLayout(
                                          
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            # Input: actionButton() to defer the rendering of output ----
                                            # until the user explicitly clicks the button (rather than
                                            # doing it immediately when inputs change). This is useful if
                                            # the computations required to render output are inordinately
                                            # time-consuming.
                                            actionButton("update2", "Plot Model Results"),
                                            
                                            # Input: Specify the number of observations to view ----
                                            textInput("modelname", "Name your model (what kind of disease):", "Ebola"),
                                            helpText(""),
                                            
                                            # Input: Specify the number of observations to view ----
                                            numericInput("ntrials2", "Number of trials the model is run (more trials takes longer to run):", 4),
                                            helpText(""),
                                            
                                            numericInput("pop2", "City population:", 100000),
                                            helpText(""),
                                            
                                            numericInput("daysrun2", "Time in days the model simulates:", 120),
                                            helpText("More days may slow the model run."),
                                            
                                            numericInput("pct.ppe2", "What percent of people wear PPE (masks)? (from 0 to 1):", 0.95),
                                            helpText(""),
                                            
                                            numericInput("ppe.effect2", "How effective is the PPE/mask at preventing spread? (from 0 to 1):", 0.95),
                                            helpText(""),
                                            
                                            numericInput("asympt.cont2", "How contagious is an asymptomatic person? (from 0 to 1):", 0.15),
                                            helpText(""),
                                            
                                            numericInput("sick.cont2", "How contagious is a sick person? (from 0 to 1):", 0.65),
                                            helpText(""),
                                            
                                            numericInput("encounter.asymptomatic.nosocdist2", "Encounters per day of asymptomatic people who do not practice distancing:", 8),
                                            helpText(""),
                                            
                                            numericInput("encounter.asymptomatic.socdist2", "Encounters per day of asymptomatic people who practice distancing:", 0.5),
                                            helpText(""),
                                            
                                            numericInput("encounter.sick.input2", "Encounters per day of people suffering from flu symptoms:", 0.15),
                                            helpText(""),
                                            
                                            numericInput("death.chance2", "How deadly is the disease (from 0 to 1):", 0.7),
                                            helpText("Many flat-curve countries average at 1/21"),
                                            
                                            numericInput("recover.time2", "Days after infection it takes for an infected person to recover:", 8),
                                            helpText(""),
                                            
                                            numericInput("rate.asymptomatic2", "Fraction of infected people who are asymptomatic  (from 0 to 1):", 0.01),
                                            helpText("~1/5 for Coronavirus"),
                                            
                                            numericInput("incub.pd2", "Days from transmission to manifesting symptoms (incubation period):", 3),
                                            helpText(""),
                                            
                                            numericInput("take.action2", "How many people get sick in your city before imposing social distancing?:", 3),
                                            helpText("The model is seeded with 3 sick and 2 asymptomatic people"),
                                            
                                            numericInput("med.scarcity.factor2", "What effect does the volume of sick cases have on death rate?:", 0.7),
                                            helpText("Ratio of sick cases to population drives medical scarcity. A value of 0 assumes no effect. A value of >1 intensifies the effect."),
                                            
                                            numericInput("hosp.beds2", "How many hospital beds are there in your city?:", 2500),
                                            helpText(""),
                                            
                                            selectInput("virus.evolves.mildness2", "Simulate progressively lower death rate as removal increases? ", c("no" = "no", "yes" = "yes")),
                                            helpText("Does the virus evolve to be less deadly?"),
                                            
                                            numericInput("relax.socdist.mean.sick2", "Relax social distancing when new confirmed cases goes below:", 0.5), 
                                            helpText("weekly mean of daily new confirmed cases")
                                            ),
                                          # Show a plot of the generated distribution
                                          mainPanel(fluidRow( column(10,
                                                                     h4("Time Series"),
                                                                     pre(id = "console2"), 
                                                                     plotOutput("TSPlot2")
                                          )))
                                          
                                        )
                                        
                               )
                               ,
                               tabPanel("Histogram Comparison",
                                        mainPanel(fluidRow( column(10,
                                                                   h4("Histogram comparison of Models A and B:\n 1) Mortality rate\n 
                                                                      2) Peak infection volume (hospital bed demand estimate).\n
                                                                      Run Models A and B to generate histogram.\n 
                                                                      Disparate number of trials may cause an ERROR."),
                                                                   plotOutput(outputId = "sirhist",
                                                                              height = "550px")))))
                               ,
                               
                               tabPanel("Epidemic Dashboard & Github", mainPanel(fluidRow( column(10,
                                                                                         h4("Link to Coronavirus Dashboard and Github Code"),
                                                                                         uiOutput("virusDashboard"),
                                                                                         uiOutput("github")
                               ))))
                               
                               
                   )
  )
                                        )






######################################################################

# Alex Van plantinga virus model
# April 2020

# City coronavirus analysis.R
# 30 days 104942 people    153 confirmed      7 deaths

library(fGarch)

SIR.model = function(ntrials, 
                     pop, 
                     daysrun,
                     asympt.cont,
                     sick.cont,
                     pct.ppe,
                     ppe.effect,
                     encounter.asymptomatic.nosocdist,
                     encounter.asymptomatic.socdist,
                     encounter.sick.input,
                     death.chance,
                     recover.time,
                     rate.asymptomatic, 
                     incub.pd,
                     take.action,
                     med.scarcity.factor,
                     hosp.beds,
                     virus.evolves.mildness,
                     relax.socdist.mean.sick ){
  
  recovered.list = list()
  
  deaths.list = list()
  
  confirmed.list = list()
  
  susceptible.list = list()
  
  removed.list = list()
  
  sick.hist = c()
  
  deaths.hist = c() # array of deaths for each run of the model, 
  
  # to make a histogram for different settings of death rate under different parameters
  inf.max = c()
  
  sick.volume = list()
  
  #ntrials = 100
  
  for(p in 1:ntrials){  
    
    #pop = 100000 # fixed population value 
    
    # seed the model with 5 infected people
    susceptible = c(pop-5) #starting pop
    
    recover.chance = 1/recover.time
    
    infected.asymptomatic = c(2) # start with one infected person who shows no symptons and does not die
    
    infected.sick = c(3) # separate the count of infected sick who have a 1/10 chance of dying
    # 50% of the infected will get sick
    
    infected.new = c(5) # distribution channel for each model iteration, feeds into inf.asympt or inf.sick
    
    confirmed = c(3) # recognized confirmed cases based on sick cases
    
    deaths = c(0) # those who die from the disease and are removed from the population
    
    recovered = c(0) # those who recover and are removed from the population of potential host/victims
    
    removed = c(5) # deaths + recovered, cannot be infected, removed from population pool of vulnerable
    
    # asymptomatic cases are roughly "half as contagious" as sick people
    # https://www.discovermagazine.com/health/asymptomatic-carriers-are-fueling-the-covid-19-pandemic-heres-why-you-dont
    
    # # https://www.worldometers.info/coronavirus/coronavirus-death-rate/
    # recover.chance = 1/21 
    
    # the factor that influences recovery rate, depends on how many people were sick two weeks ago
    rr = 0
    
    #incub.pd = 3 # let there be a 1-4 day incubation period
    
    for(i in 2:daysrun){
      
      #------------------------------
      # INFECTIONS
      #--------------------------
      infected.new[i] = 0
      
      # make the number of encounters random,
      # but adjust the encounter rate depending on the volume of sick people
      
      
      if(infected.sick[i-1] < take.action ){ # take.action is for the first implimentation of social distancing in the city
        
        encounter.asymptomatic = encounter.asymptomatic.nosocdist
        
        if(encounter.sick.input < encounter.asymptomatic){
          encounter.sick = (encounter.asymptomatic.nosocdist - encounter.sick.input) / 5
        }else(  encounter.sick = encounter.sick.input )
        
      }else({
        encounter.asymptomatic = encounter.asymptomatic.socdist
        encounter.sick = encounter.sick.input
      })
      
      
      # Relax social distancing for asymptomatic people after two weeks
      # User inputs the percent increase in sick threshold
      
      
      if(i>14){
        if(
          #(1-rate.asymptomatic) * mean(infected.new[(i-8) : (i-1)]) > relax.socdist.mean.sick ){
          mean(confirmed[(i-7) : (i-1)] - confirmed[(i-8) : (i-2)]) > relax.socdist.mean.sick ){
          encounter.asymptomatic = encounter.asymptomatic.socdist
          encounter.sick = encounter.sick.input
        }else(
          {
            encounter.asymptomatic = encounter.asymptomatic.nosocdist
            
            if(encounter.sick.input < encounter.asymptomatic){
              encounter.sick = (encounter.asymptomatic.nosocdist - encounter.sick.input) / 5
            }else(  encounter.sick = encounter.sick.input )
          }
        )
      }
      
      
      # Incubation inbub.pd determines what timeslice of infected people are spreading the disease
      
      if(i > incub.pd){
        
        # calculate the randum number of encounters
        # the mean number will be encounter.sick by generating over the range 2*encounter.sick
        # the infected who spread the disease where those infected as of 3days ago (inbub.pd)
        # subtract the ones recovered in that elapsed time from 
        if( 
          round((infected.sick[i-incub.pd] - (recovered[i-1] - recovered[i-incub.pd]) * (1-rate.asymptomatic))) <= 0 
          
          |
          
          round((infected.asymptomatic[i-incub.pd] - (recovered[i-1] - recovered[i-incub.pd]) * (rate.asymptomatic))) <= 0
          
        ){
          
          rand.sick.encounters = 0
          
          rand.asympt.encounters = 0
          
        }else({
          
          rand.sick.encounters = round(
            sum(
              abs(rsnorm( # calculate sick encounters using fGarch::rsnorm
                round(
                  infected.sick[i-incub.pd] - (recovered[i-1] - recovered[i-incub.pd]) * (1-rate.asymptomatic)
                )
                # subtract the since-recovered people
                , mean = encounter.sick, sd = 0.75*encounter.sick, xi = 10))
            ) #* (susceptible[i-1]/pop) # encounters multiplied by the availability of succiptible people
          )
          
          
          rand.asympt.encounters = round(
            sum(
              abs(rsnorm( # calculate again for asymptomatic people using fGarch::rsnorm
                round(
                  infected.asymptomatic[i-incub.pd] - (recovered[i-1] - recovered[i-incub.pd]) * (rate.asymptomatic)
                ) # subtract the since-recovered people
                , mean = encounter.asymptomatic, sd = 0.75 * encounter.asymptomatic, xi = 10))
            ) #* (susceptible[i-1]/pop) # encounters multiplied by the availability of succiptible people
          )
          
          
          # with the rand.sick.encounters and rand.asympt encounters in hand,
          # make a vector of 1's and 0's representing the total number of susceptible (1's) and removed (0's), excluding the number of dead who do not interact in the future
          
          shuffled.susceptible = c((1:susceptible[i-1])/(1:susceptible[i-1]), 0*(1: (removed[i-1] - deaths[i-1] ) ) )[sample(1:(pop - deaths[i-1] ), (pop - deaths[i-1] ), replace = FALSE)] 
          
          rand.sick.encounters = sum(shuffled.susceptible[1:rand.sick.encounters]) # sum the 1's and 0's from the random interactions 
          
          shuffled.susceptible = c((1:susceptible[i-1])/(1:susceptible[i-1]), 0*(1: ( removed[i-1] - deaths[i-1] ) ) )[sample(1: (pop - deaths[i-1] ), (pop - deaths[i-1] ), replace = FALSE)] 
          
          rand.asympt.encounters = sum(shuffled.susceptible[1:rand.asympt.encounters]) # sum the 1's and 0's from the random interactions
          
        })
        
        
        if(susceptible[i-1] > 0 && !is.na(rand.sick.encounters)){
          # infections caused by encounters with sick people
          
          rand.sick.encounters.ppe = floor(rand.sick.encounters * pct.ppe)
          
          rand.sick.encounters.NOppe = rand.sick.encounters - rand.sick.encounters.ppe
          
          x = runif(rand.sick.encounters.ppe, 0, 1)
          
          y = runif(rand.sick.encounters.NOppe, 0, 1)
          
          # random interactions within the sick.contagiousness cutoff is a transmission to infected.new
          infected.new[i] = infected.new[i] + length(x[which(x < (sick.cont*(1-ppe.effect)))])
          
          # again for the no-ppe group (the bad people who don't wear masks)
          infected.new[i] = infected.new[i] + length(y[which(y < sick.cont)])
          
        }
        
        # assume asymptomatic people have the chance to infect more people than sick people
        if(susceptible[i-1] > 0 && !is.na(rand.sick.encounters)){
          
          rand.asympt.encounters.ppe = floor(rand.asympt.encounters * pct.ppe)
          
          rand.asympt.encounters.NOppe = rand.asympt.encounters - rand.asympt.encounters.ppe
          
          x = runif(rand.asympt.encounters.ppe, 0, 1)
          
          y = runif(rand.asympt.encounters.NOppe, 0, 1)
          
          infected.new[i] = infected.new[i] + length(x[which(x < (asympt.cont * (1-ppe.effect)))])
          
          infected.new[i] = infected.new[i] + length(y[which(y < asympt.cont)])
          
        }else(infected.new[i] = 0)
        
      }else(infected.new[i] =   round( # in the first three days of the model during the incubation period
        sum(
          runif(
            removed[i-1], 0, 2 * encounter.asymptomatic * asympt.cont)
        )
      ) 
      )  
      
      # if we run out of uninfected people in a trial, then infect all susceptibleing people
      if(infected.new[i] > susceptible[i-1]){infected.new[i] = susceptible[i-1]; susceptible[i] = 0} # zero people susceptible
      
      # randomly decide who of the new infections are sick and asymptomatic
      asympt.new = 0 # counter variable 
      sick.new = 0 # counter variable 
      if(infected.new[i] > 0 ){
        for(k in 1:infected.new[i]){
          # 20% chance of being asymptomatic or being sick
          if(rate.asymptomatic > runif(n = 1, min = 1e-12, max = 1)){
            asympt.new = asympt.new + 1}else(sick.new = sick.new + 1) 
        }
      }
      
      # now we have have a count of new cases of sick and asymptomatic individuals for this iteration (day)
      
      #-----------------------
      # REMOVALS - outcomes of the previous day's sick and asymptomatic pools of people
      #-------------------------
      
      recovered.new.sick = 0 # those of the sick that recover
      
      deaths.new = 0 # new deaths that come out of the pre-existing sick pool
      
      
      if(infected.sick[i-1] > 0){
        for(k in 1: ceiling( infected.sick[i-1] / recover.time) ) { # divide the daily outcomes by the recovery time
          
          # if they die, count the death tally
          # adjust chance of death while sick
          # include a factor that increases the chance of death in proportion to the number of infections
          # this can simulate the effect of the relative scarcity of medical resources
          
          if(i > recover.time){
            rr = (infected.asymptomatic[i-recover.time] + infected.sick[i-recover.time]) / pop # rr boosts the probability of recovery based on how many sick people 14 days ago
          }else(rr = 0) # rr is the factor that increases recovery rate. It kicks in when i > 15. Start healing people after 15 days
          
          # scarcity of hospital beds chances the death chance
          if(infected.sick[i-1] < hosp.beds){
            death.chance.mod = death.chance
          }else(death.chance.mod = (death.chance + med.scarcity.factor*abs(infected.sick[i-1]/pop)) )
          
          
          # if the user chooses "yes" to model a simple time-dependent evolution of a less deadly strain
          # let it become half as virulent every 6 months
          if(virus.evolves.mildness ==  "yes"){
            
            if(infected.sick[i-1] > hosp.beds){death.chance.mod = # "decreasing deadliness" is same as death.chance.mod except that it decreases with the removal 
              
              (death.chance + med.scarcity.factor*abs(infected.sick[i-1]/pop)) * (1-removed[i-1]/pop)}else(
                death.chance.mod = death.chance  * (1-removed[i-1]/pop)
                
              )}
          
          if( death.chance.mod  > runif(n = 1, min = 1e-12, max = 1)){
            deaths.new = deaths.new + 1}else( # if they do not die, then give them a chance to recover
              recovered.new.sick = recovered.new.sick + 1
              #  1*((recover.chance + rr) > runif(n = 1, min = 1e-12, max = 1))
              # multiply recover.chance by i, because chance of recovery increases with time
            )
          
        }
      }
      
      recovered.new.asympt = 0
      # based on prev iteration count of inf.asympt people, how many recovered?
      #if(i > 15){rr = 10}else(rr = 1) #already written above, but note this variable
      if(infected.asymptomatic[i-1] > 0){
        for(k in 1:infected.asymptomatic[i-1]){
          recovered.new.asympt = recovered.new.asympt + 
            1*((recover.chance + rr) > runif(n = 1, min = 1e-12, max = 1))
          # multiply recover.chance by i, because chance of recovery increases with time
        }
      }
      
      ############
      # Update the chains
      
      confirmed[i] = confirmed[i-1] + sick.new
      
      # update for any deaths or recoveries and new cases
      infected.sick[i] = infected.sick[i-1] + sick.new - deaths.new - recovered.new.sick 
      
      # update for recoveries and new cases
      infected.asymptomatic[i] = infected.asymptomatic[i-1] + asympt.new - recovered.new.asympt  
      #if(infected.asymptomatic[i] < 0){infected.asymptomatic[i] = 0}
      
      deaths[i] = deaths.new + deaths[i-1]
      
      recovered[i] = recovered[i-1] + recovered.new.asympt + recovered.new.sick
      
      # assign a value to the removed category
      #if(infected.new[i] == susceptible[i-1] | removed[i-1] == pop){
      #  removed[i] = pop}else(
      removed[i] =   removed[i-1] + sick.new + asympt.new #+ removed[i-1] + infected.new[i])
      
      # assign a value to the susceptibleing population
      #if(!is.na(susceptible[i]) | susceptible[i-1] == 0){susceptible[i] = 0}else(susceptible[i] = pop - removed[i])
      susceptible[i] = susceptible[i-1] - infected.new[i]
      
      
    }
    
    # timeseries builder section
    
    deaths.hist = append(deaths.hist, deaths[length(deaths)])
    
    inf.max = append(inf.max, max(infected.asymptomatic + infected.sick))
    
    sick.hist = append(sick.hist, confirmed[length(confirmed)])
    
    # lists
    sick.volume[[p]] = infected.sick
    
    recovered.list[[p]] = recovered
    
    deaths.list[[p]] = deaths
    
    confirmed.list[[p]] = confirmed
    
    susceptible.list[[p]] =  susceptible
    
    removed.list[[p]] = removed
    
    
    # added a message to tell the user how many trials have run
    if(p != ntrials){ message(paste("Running... Number of trials run:", p, "...")) }else(
      message(paste(p, "trials complete!"))
    )
  }
  
  
  # return from model multiple objects must be returned in a list
  x = list(# inputs, conditions of the model
    ntrials, 
    pop, 
    daysrun,
    asympt.cont,
    sick.cont,
    pct.ppe,
    ppe.effect ,
    encounter.asymptomatic.nosocdist,
    encounter.asymptomatic.socdist,
    encounter.sick,
    death.chance,
    recover.time,
    rate.asymptomatic, 
    incub.pd,
    take.action,
    med.scarcity.factor,
    hosp.beds,
    relax.socdist.mean.sick, 
    
    #time series from the last iteration of the model
    infected.asymptomatic,
    infected.sick,
    infected.new,
    recovered,
    deaths,
    removed,
    susceptible,
    confirmed,
    
    # return time series from trials
    deaths.hist,
    sick.volume,
    inf.max,
    sick.hist,
    recovered.list,
    deaths.list,
    confirmed.list,
    susceptible.list,
    removed.list)
  
  # give the items in the list names
  names(x) = c(
    # input names
    "ntrials", 
    "pop", 
    "daysrun",
    "asympt.cont",
    "sick.cont",
    "pct.ppe",
    "ppe.effect",
    "encounter.asymptomatic.nosocdist",
    "encounter.asymptomatic.socdist",
    "encounter.sick",
    "death.chance",
    "recover.time",
    "rate.asymptomatic", 
    "incub.pd",
    "take.action",
    "med.scarcity.factor",
    "hosp.beds",
    "relax.socdist.mean.sick",
    
    # time series from the last iteration of the model
    "infected.asymptomatic",
    "infected.sick",
    "infected.new",
    "recovered",
    "deaths",
    "removed",
    "susceptible",
    "confirmed",
    
    # return time series from trials
    "deaths.hist",
    "sick.volume",
    "inf.max",
    "sick.hist",
    "recovered.list",
    "deaths.list",
    "confirmed.list",
    "susceptible.list",
    "removed.list")
  
  return(x)
  
}



######################################################################




SIR.plotter = function(x){
  
  # average time series for ntrials
  
  library(ggplot2)
  library(reshape2)
  
  daysrun = x$daysrun
  
  
  if(daysrun == 1){
    
    susceptible = x$susceptible
    pop = x$pop
    removed = x$removed
    recovered = x$recovered
    confirmed = x$confirmed
    daysrun  = x$daysrun
    deaths = x$deaths
    sickvolume = x$infected.sick
    
  }else({
    
    
    deaths = round(rowMeans(simplify2array( x$deaths.list )))
    susceptible = round(rowMeans(simplify2array( x$susceptible.list )))
    removed = round(rowMeans(simplify2array( x$removed.list )))
    recovered = round(rowMeans(simplify2array( x$recovered.list )))
    confirmed = round(rowMeans(simplify2array( x$confirmed.list )))
    daysrun = x$daysrun
    pop = x$pop
    sickvolume = round(rowMeans((simplify2array((x$sick.volume)))))
    
  })
  
  df = cbind(c(1:daysrun), susceptible, removed, recovered, confirmed, deaths, sickvolume)
  
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
                           confirmed[daysrun], " recorded cases", sep = " ")) + 
    scale_y_log10() + theme_minimal()
  
  
  return(p)
  
}




######################################################################



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


######################################################################

server <- function(input, output) {
  
  v <- reactiveValues(data=NULL)
  
  observeEvent(
    input$update,
    {
      v$data1 = withCallingHandlers(
        
        SIR.model(input$ntrials,
                          input$pop,
                          input$daysrun,
                          input$asympt.cont,
                          input$sick.cont,
                          input$pct.ppe,
                          input$ppe.effect,        
                          input$encounter.asymptomatic.nosocdist,
                          input$encounter.asymptomatic.socdist,
                          input$encounter.sick.input,
                          input$death.chance,
                          input$recover.time,
                          input$rate.asymptomatic,
                          input$incub.pd,
                          input$take.action,
                          input$med.scarcity.factor,
                          input$hosp.beds,
                          input$virus.evolves.mildness,
                          input$relax.socdist.mean.sick),
      
        # can use "warning" instead/on top of "message" to catch warnings too
      message = function(m) {
        shinyjs::html("console", m$message, FALSE)
      }
  )
      
      
      
      
      v$data2 = SIR.plotter(v$data1)
      
      
      
    }
  )
  
  
  
  output$TSPlot = renderPlot({
    
    if (is.null( v$data2 )) return()
    
    v$data2
    
  })
  
  
  
  ##########################
  
  #initialize u
  
  u <- reactiveValues(data=NULL)
  
  observeEvent(
    
    input$update2,
    
    {
      u$data1 = withCallingHandlers(
        
        SIR.model(input$ntrials2,
                          input$pop2,
                          input$daysrun2,
                          input$asympt.cont2,
                          input$sick.cont2,
                          input$pct.ppe2,
                          input$ppe.effect2,
                          input$encounter.asymptomatic.nosocdist2,
                          input$encounter.asymptomatic.socdist2,
                          input$encounter.sick.input2,
                          input$death.chance2,
                          input$recover.time2,
                          input$rate.asymptomatic2,
                          input$incub.pd2,
                          input$take.action2,
                          input$med.scarcity.factor2,
                          input$hosp.beds2,
                          input$virus.evolves.mildness2,
                          input$relax.socdist.mean.sick2),
        
        # can use "warning" instead/on top of "message" to catch warnings too
        message = function(m) {
          shinyjs::html("console2", m$message, FALSE)
        }
      )
      
      
      u$data2 = SIR.plotter(u$data1)
      
      
    }
  )
  
  
  
  output$TSPlot2 = renderPlot({
    
    if (is.null(u$data2)) return()
    
    u$data2
    
  })
  
  
  #################################
  
  # Histogram Plot Output
  output$sirhist = renderPlot({
    
    if (is.null(u$data1) ) return()
    
    SIR.hist(v$data1, u$data1)
    
  })
  
  
  url <- a("RShiny Coronavirus Dashboard Link", href = "https://acvanp.shinyapps.io/COVID-19/")
  output$virusDashboard <- renderUI({
    tagList("Navigate to:", url)
  })
  
  
  
  url2 <- a("R Code for App and Model https://github.com/acvanp/Epidemic", href = "https://github.com/acvanp/Epidemic")
  output$github <- renderUI({
    tagList("Navigate to:", url2)
  })
  
  
  
}

shinyApp(ui, server)