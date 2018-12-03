# Make sure wbstats is installed
if(!require('wbstats')){install.packages('wbstats')}
# Load wbstats and cache the information
library(wbstats)
update_cache <- wbcache()
# looking for information related to gini
gini_vars <- wbsearch(pattern = 'gini')
# inspecting 3.0.Gini
gini_vars[gini_vars$indicatorID == '3.0.Gini',]
# retrieve information about it
dt_gini <- wb(indicator = '3.0.Gini')
# Gathering information about education
edu_vars <- wbsearch(pattern = 'years of schooling')
edu_vars[edu_vars$indicatorID == 'UIS.EA.MEAN.1T6.AG25T99',]
dt_edu <- wb(indicator = 'SE.SCH.LIFE')
# Gathering information about population
pop_vars <- wbsearch(pattern = 'total population')
pop_vars[pop_vars$indicatorID == 'SP.POP.TOTL',]
dt_pop <-wb(indicator = 'SP.POP.TOTL')
# Selecting only useful columns
dt_gini <- dt_gini[, c('date', 'value', 'country')]
# Looking at their names
names(dt_gini)
# Renaming value to gini
names(dt_gini)[2] <- 'gini'
# Doing the same to dt_edu
dt_edu <- dt_edu[, c('date', 'value', 'country')]
names(dt_edu)[2] <- 'mean_yrs_schooling'
# Doing the same to dt_pop
dt_pop <- dt_pop[, c('date', 'value', 'country')]
names(dt_pop)[2] <- 'population'
# Creating an unique key to allow merging
dt_gini$merge_key <- paste(dt_gini$date, dt_gini$country, 
                           sep = '_')
dt_edu$merge_key <- paste(dt_edu$date, dt_edu$country, 
                           sep = '_')
dt_pop$merge_key <- paste(dt_pop$date, dt_pop$country, 
                          sep = '_')
# Merging the three data frames into a single one
dt <- merge(dt_edu, dt_gini, by = 'merge_key', all = F)
dt <- merge(dt, dt_pop, by = 'merge_key', all = F)
# Selecting fewer columns
dt <- dt[,c('gini', 'population', 'mean_yrs_schooling', 'date', 'country')]
# Checking/installing ggplot2
if(!require('ggplot2')){install.packages('ggplot2')}
# Plotting a simple scatterplot with ggplto2
library(ggplot2)
ggplot(data = dt) + 
  geom_point(aes(x = gini, 
                 y = mean_yrs_schooling))
# plotting a bbuble plot with ggplot2
ggplot(data = dt) + 
  geom_point(aes(x = gini, 
                 y = mean_yrs_schooling, 
                 color = factor(country), 
                 size = population), alpha = .4)
# Improving the plot
ggplot(data = dt) + 
  geom_point(aes(x = gini, 
                 y = mean_yrs_schooling, 
                 color = factor(country), 
                 size = population), alpha = .4) +
  xlab('Gini Index') +
  ylab('Mean years of schooling (25+)')+
  guides(size = F) +
  theme_classic(base_size = 16)
# Checking/installing ggvis
if(!require('ggvis')){install.packages('ggvis')}
# Drawing a bubble plot with ggvis
library(ggvis)
dt %>% 
  ggvis(~gini, ~mean_yrs_schooling, fill = ~factor(country)) %>% 
  layer_points(size = ~population, opacity:=.4)
# Checking/installing plotly
if(!require('plotly')){install.packages('plotly')}
# Drawing a bubble plot with plotly
library(plotly)
plot_ly(dt, x = ~gini, y = ~mean_yrs_schooling, 
        type = 'scatter', mode = 'markers',
        color = ~country, size = ~population)
# Changing the tooltip
plot_ly(dt, x = ~gini, y = ~mean_yrs_schooling, 
        type = 'scatter', mode = 'markers',
        color = ~country, size = ~population,
        hoverinfo = 'text',
        text = ~paste('<b>',country,' - ', date, '</b><br>Gini: ', round(gini), 
                      '<br>Population:', population, 
                      '<br>Mean years of schooling:', round(mean_yrs_schooling)))
# Installing devtools / rCharts / yaml
if(!require('devtools')){install.packages('devtools')}
if(!require('rCharts')){
  if(!require('yaml')){install.packages('yaml')}
  devtools::install_github('ramnathv/rCharts')
}
# Drwaing a bubble plot with rCharts
library(rCharts)
p1 <- hPlot(mean_yrs_schooling ~ gini,
                     data = dt, type = 'bubble',
                     size = 'population', group = 'country')
p1$chart(zoomType = 'xy')
p1$exporting(enabled = T)
p1$show()
# Enabling zoom, rescaling the plot, and changing the tooltip
p2 <- hPlot(mean_yrs_schooling ~ gini,
                     data = dt, type = 'bubble',
                     size = 'population', group = 'country')
p2$tooltip(formatter = "#! function() { return 'Gini :' + Math.round(this.x * 100) / 100 + 
           '<br>Mean years of schooling :' + Math.round(this.y * 100) / 100; } !#")
p2$legend(align = 'right', verticalAlign = 'top', layout = 'vertical')
p2$chart(zoomType = 'xy')
p2$exporting(enabled = T)
p2$set(width = 528, height = 528)
p2$show()
# Checking / installing googleVis
if(!require('googleVis')){install.packages('googleVis')}
# Drawing a bubble plot with googleVis
library(googleVis) 
p3 <- gvisBubbleChart(dt, idvar = 'country',
                      xvar = 'gini', yvar = 'mean_yrs_schooling',
                      sizevar = 'population', colorvar = 'country')
plot(p3)
# Hiding idvar, rescaling the plot and enabling zoom
dt$id <- ''
p4 <- gvisBubbleChart(dt, idvar = 'id',
                      xvar = 'gini', yvar = 'mean_yrs_schooling',
                      sizevar = 'population', colorvar = 'country',
                      options = list(
                        width=600, height=600,
                        explorer= "{ actions: ['dragToZoom', 'rightClickToReset'] }"
                      ))
plot(p4)
# Drawing a choropleth with googleVis
map <- gvisGeoChart(dt_gini[dt_gini$date == '2008',], locationvar='country', 
                 colorvar='gini', 
                 options=list(projection='kavrayskiy-vii', 
                              backgroundColor = '#81d4fa',
                              region = '005'))
plot(map)
# Quiz answers
set.seed(10)
round(runif(3,1,4))