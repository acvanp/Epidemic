# Alex Van Plantinga

rawdata = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
rawdata = rawdata[which(rawdata$Country_Region == "US"),]
rawdata = rawdata[with(rawdata, order(rawdata$UID)),]

rawdeaths = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
rawdeaths = rawdeaths[which(rawdeaths$Country_Region == "US"), abs(ncol(rawdata)-ncol(rawdeaths)):ncol(rawdeaths)]
rawdeaths = rawdeaths[with(rawdeaths, order(rawdata$UID)),]

confirmed.ar.us = aggregate(. ~ rawdata$Province_State, rawdata[which(colnames(rawdata)=="X1.22.20"):ncol(rawdata)], FUN = sum)

US.latlon =  aggregate(. ~ rawdata$Province_State, rawdata[,which(colnames(rawdata) %in% c("Lat", "Long_"))], FUN = median)

deaths.ar.us = aggregate(. ~ rawdeaths$Province_State, rawdeaths[which(colnames(rawdeaths)=="X1.22.20"):ncol(rawdeaths)], FUN = sum)

# get rid of nuisance US entry that does not contain data
#confirmed.ar.us = confirmed.ar.us[which(confirmed.ar.us$Province.State != "US"),]
#deaths.ar.us = deaths.ar.us[which(deaths.ar.us$Province.State != "US"),]

local.cases = cbind(rawdata$Province_State,rawdata$Combined_Key, rawdata$Lat, rawdata$Long_, rawdeaths$Population,
                    rawdata[ncol(rawdata)], rawdeaths[ncol(rawdeaths)] )

colnames(local.cases) = c("State","Combined_Key", "Lat", "Long_", "Population", "Confirmed", "Deaths")

ggplot(local.cases, 
       aes(x = local.cases$Population, y = local.cases$Deaths, 
           label = gsub("^(.*?),.*", "\\1",local.cases$Combined_Key))) + 
  geom_text(size = 1.5)+
  scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')

local.cases$colr = NA
local.cases$region = NA
deepsouth = c("Alabama",
              "Florida",
              "Georgia",
              "Louisiana",
              "Mississippi")

local.cases$colr[which(local.cases$State %in% deepsouth )] = "red"
local.cases$region[which(local.cases$State %in% deepsouth )] = "Deep South"

midsouth = c("Kentucky",
             "North Carolina",
             "South Carolina",
             "Tennessee",
             "Virginia")

local.cases$colr[which(local.cases$State %in% midsouth)] = "yellow1"
local.cases$region[which(local.cases$State %in% midsouth )] = "Mid South"


pacnwest = c("California",
  "Idaho",
  "Montana",
  "Oregon",
  "Washington",
  "Wyoming")

local.cases$colr[which(local.cases$State %in% pacnwest )] = "darkgreen"
local.cases$region[which(local.cases$State %in% pacnwest )] = "Pacific West"


greatlakes = c("Illinois",
               "Indiana",
               "Michigan",
               "Ohio",
               "Wisconsin")

local.cases$colr[which(local.cases$State %in% greatlakes)] = "cyan"
local.cases$region[which(local.cases$State %in% greatlakes)] = "Great Lakes"

newengland = c("Connecticut",
               "Maine",
               "Massachusetts",
               "New Hampshire",
               "Rhode Island",
               "Vermont")

local.cases$colr[which(local.cases$State %in% newengland)] = "navy"
local.cases$region[which(local.cases$State %in% newengland)] = "New England"

southcentral = c("Arkansas",
                 "Kansas",
                 "Missouri",
                 "Oklahoma",
                 "Texas")

local.cases$colr[which(local.cases$State %in% southcentral)] = "magenta"
local.cases$region[which(local.cases$State %in% southcentral)] = "South Central"

midatlantic = c("Delaware",
                "Maryland",
                "New Jersey",
                "New York",
                "Pennsylvania",
                "West Virginia")

local.cases$colr[which(local.cases$State %in% midatlantic)] = "green"
local.cases$region[which(local.cases$State %in% midatlantic)] = "Mid Atlantic"

southwest = c("Arizona",
              "Colorado",
              "Nevada",
              "New Mexico",
              "Utah")

local.cases$colr[which(local.cases$State %in% southwest)] = "darkorange"
local.cases$region[which(local.cases$State %in% southwest)] = "Southwest"

centralplains = c("Iowa",
                  "Minnesota",
                  "Nebraska",
                  "North Dakota",
                  "South Dakota")

local.cases$colr[which(local.cases$State %in% centralplains)] = "purple"
local.cases$region[which(local.cases$State %in% centralplains)] = "Central Plains"


other = c("Alaska", "Hawaii")

local.cases$colr[which(local.cases$State %in% other )] = "gray"
local.cases$region[which(local.cases$State %in% other)] = "Pacific"


local.cases$colr[which(is.na(local.cases$region))] = "black"
local.cases$colr[which(is.na(local.cases$colr))] = "black"

local.cases = local.cases[which(local.cases$Population > 10),]

local.cases = local.cases[which(local.cases$Confirmed > 10),]


local.cases = local.cases[order(local.cases$region),]

library(ggplot2)

ggplot(local.cases[order(local.cases$region),], 
       aes(x = `Population`, y = `Confirmed`, 
           label = gsub("^(.*?),.*", "\\1",local.cases$Combined_Key))) + 
  geom_text(aes(color = local.cases$region), size = 2.5, angle = 45*abs(log10(local.cases$Population/max(local.cases$Population)))) +
  scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')+
  scale_colour_manual(name = "Regions", values=unique(local.cases$colr))



ggplot(local.cases, 
       aes(x = `Confirmed`/`Population`, y = `Deaths`/`Population`, 
           label = gsub("^(.*?),.*", "\\1", local.cases$Combined_Key))) + 
  geom_text(aes(color = local.cases$region), size = 2.5) +
  scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10') +
  scale_colour_manual(values=unique(local.cases$colr))



stateabbreviations = c("AL","AK","AZ",
                       "AR","CA","CO",
                       "CT","DE","FL",
                       "GA","HI","ID",
                       "IL","IN","IA",
                       "KS","KY","LA",
                       "ME","MD","MA",
                       "MI","MN","MS",
                       "MO","MT","NE",
                       "NV","NH","NJ",
                       "NM","NY","NC",
                       "ND","OH","OK",
                       "OR","PA","RI",
                       "SC","SD","TN",
                       "TX","UT","VT",
                       "VA","WA","WV",
                       "WI","WY")


# plot confirmed cases since january vs. 7 day average increase in daily new cases
# https://www.npr.org/sections/health-shots/2020/03/16/816707182/map-tracking-the-spread-of-the-coronavirus-in-the-u-s
x = confirmed.ar.us[,3:ncol(confirmed.ar.us)] - confirmed.ar.us[,2:(ncol(confirmed.ar.us)-1)]
y = list()
for(i in 1:nrow(x)){
  v = c()
  for(j in 1:(length(x[i,])-3)){v[j] = mean(x[i,j:(j+3)])}
  y[[i]] = v
}

par("mar")
#margins bottom, left, top, right
par(mar=c(4,4,2,6))
i = 12
var1 = as.vector(t(confirmed.ar.us)[3:ncol(confirmed.ar.us),i])
var1 = as.numeric(var1)
var2 = as.vector(t(x)[,i])
plot(log10(var1), log10(var2), cex = 0.2, type = "l", 
     xlim = c(0, 6), ylim = c(0,4),
     main = confirmed.ar.us$`rawdata$Province_State`[i],
     xlab = "New Cases Since January (log10 individuals)",
     ylab = "Daily New Cases (log10 individuals)")

cols = colorRampPalette(c("red", "yellow", "blue"))(nrow(confirmed.ar.us))
#par(mfrow =c(1,2))
par(mar=c(4,4,2,6))
for(i in 1:nrow(confirmed.ar.us)){
  var1 = as.vector(t(confirmed.ar.us)[3:ncol(confirmed.ar.us),i])
  var1 = rollmean(as.numeric(var1), 7, na.pad = TRUE)
  var2 = rollmean(as.vector(t(x)[,i]), 7, na.pad = TRUE)
  #var1 = as.numeric(var1)
  #var2 = as.vector(t(x)[,i])
  plot(log10(var1), log10(var2), cex = 0.2, type = "l", col = cols[i], xlim = c(0, 6), ylim = c(0,4))
  par(new = TRUE)
}
par(xpd=TRUE)
legend(6.35, 4.6,legend = confirmed.ar.us$`rawdata$Province_State`, 
       col=cols, cex = 0.4, lty = 1)

par(new = FALSE)
plot(0, 0, yaxt = "n", xaxt = "n", ylab = "", xlab = "", ylim = c(-2,2))
legend(-1,2.2, 
       legend = confirmed.ar.us$`rawdata$Province_State`, 
       col=cols, cex = 0.4, lty = 1)
     
filter(x, rep(1/3,3)) 





# convert to ggplot
i = 12
var1 = as.vector(t(confirmed.ar.us)[3:ncol(confirmed.ar.us),i])
#var1 = as.numeric(var1)
#var2 = as.vector(t(x)[,i])

var1 = rollmean(as.numeric(var1), 7, na.pad = TRUE)
var2 = rollmean(as.vector(t(x)[,i]), 7, na.pad = TRUE)


df = data.frame(var1, var2)
colnames(df) = c("Total.Cases", "Daily.New.Cases")

ggplot(df, aes(x = `Total.Cases`, y = `Daily.New.Cases`))+
  scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')+
  geom_line()+theme_minimal()+#xlim(0,10e5)+ylim(0,10e3)+
  labs(title=paste0("New Case Growth in: ", confirmed.ar.us$`rawdata$Province_State`[i]),
       y="Daily New Cases (log10 individuals)", 
       x = "Total Cases (log10 individuals)")
