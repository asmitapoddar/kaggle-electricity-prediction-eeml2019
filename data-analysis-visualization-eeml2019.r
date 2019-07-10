
library(tidyverse) # metapackage with lots of helpful functions
list.files(path = "../input")

library(lubridate)
library(zoo)
library(plotly)
library(forecast)
library(xts)
library(tseries)
library(gplots)
library(reshape2)

train  = read_csv("../input/electricity/train_electricity.csv")
train$Date = as_datetime(train$Date)
test  = read_csv("../input/electricity/test_electricity.csv")
test$Date = as_datetime(test$Date)
test_consumption_pred = read_csv("../input/test-consumption-predicted/baseline_improved.csv")

add_datetime_features <- function(df)
{
    features = c("Year", "Month", "Week", "Day", "Dayofyear", "Dayofweek", "Quarter", "Hour", "Minute")
    df = cbind(df, "Year" = year(df$Date))
    df = cbind(df, "Month" = month(df$Date))
    df = cbind(df, "Week" = week(df$Date))
    df = cbind(df, "Day" = day(df$Date))
    df = cbind(df, "Dayofyear" = as.numeric(strftime(df$Date, format = "%j")))
    df = cbind(df, "Dayofweek" = wday(df$Date))
    df = cbind(df, "Quarter" = quarters(df$Date))
    df = cbind(df, "Hour" = hour(df$Date))
    df = cbind(df, "Minute" = minute(df$Date))           
    return (df)
}
df = add_datetime_features(train)
df_test = add_datetime_features(test)
df_test_con_pred = add_datetime_features(test_consumption_pred)
df$actualprod = rowSums(df[,3:9])
summary(df)

g=nrow(df[df$Gas_MW<0,1:10])/nrow(df)*100
w=nrow(df[df$Wind_MW<0,1:10])/nrow(df)*100
s=nrow(df[df$Solar_MW<0,1:10])/nrow(df)*100
print(paste0("Percentage of negative Gas_MW values: ",g))
print(paste0("Percentage of negative Wind_MW values: ", w))
print(paste0("Percentage of negative Solar_MW values: ", s))
neg_energy = do.call("rbind", list(df[df$Coal_MW<0,1:10],df[df$Gas_MW<0,1:10], df[df$Wind_MW<0,1:10], df[df$Solar_MW<0,1:10]))
print("Yearwise negative values for solar energy:")
for (y in unique(df$Year))
    print(paste0("For ", y, ": ", nrow(df[df$Solar_MW<0&df$Year==y,1:10])/nrow(df)*100, "%"))

unique_years = unique(df$Year)
df_year = list()
for (u in unique_years)
    df_year = append(df_year,list(df[df$Year==u,]))

yearwise = df[df$Year!=2018, ]
yearwise = yearwise[,2:12] %>%  group_by(Year, Month) %>%  summarise_each(list(mean))
yearwise = df[df$Year!=2018, ]
yearwise = yearwise[,2:12] %>%  group_by(Year) %>%  summarise_each(list(mean))
colours = c("red", "green", "orange", "pink", "yellow", "purple", "black", "cyan", "blue")
p = plot_ly(yearwise, x = ~Year)
j=1
for (i in 2:ncol(yearwise))
{
    p = p %>% add_lines(y = yearwise[[i]], name = colnames(yearwise)[i], line = list(color = colours[j]))
    j=j+1
}

t = vector()
i=1
for (y in unique(df$Year))
{
    t = c(t, yearwise$Production_MW[yearwise$Year==y]/yearwise$Consumption_MW[yearwise$Year==y]*100)
    print(paste0("Percentage of MW Produced as a function of Conumption in Year ", y, ": ", t[i]))
    i=i+1
}
print(mean(t, na.rm=T))

func_year<-function(df)
{
    cols = 1:10
    df1 = df[,c(cols,12,20)]  #Col 12 for Month, Col 20 for actualprod
    df1 = df1[,-1]
    df1
}
df_year_sliced = lapply(df_year, func_year)
df_year_month = lapply(df_year_sliced, function(x) x %>%  group_by(Month) %>%  summarise_each(funs(sum)))
                       
df_year_month_merged = do.call("cbind", list(df_year_month[[1]], df_year_month[[2]], df_year_month[[3]], df_year_month[[4]], df_year_month[[5]], df_year_month[[6]], df_year_month[[7]], df_year_month[[8]], df_year_month[[9]]))
monthnames = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
df_year_month_merged$Monthnames = factor(monthnames, levels = monthnames)

colours = c("red", "green", "orange", "pink", "yellow", "purple", "black", "cyan", "blue", "darkgoldenrod1")
p = plot_ly(df_year_month_merged, x = ~Monthnames)
j=1
c = 0
for (i in 1:ncol(df_year_month_merged))
{
    if(i%%11==1)
    {
        j=1
        next
    }
    if(i%%11==0)
    {
       p = p %>% add_trace(y = df_year_month_merged[[i]], type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,100,80,1)'),
            showlegend = TRUE, name = 'Theoretical Production')
        next
    }    
    p = p %>% add_lines(y = df_year_month_merged[[i]], name = colnames(df_year_month_merged)[i], line = list(color = colours[j]))
    j=j+1
    c = c+1
}

t = list()
for (i in 1:10)
    t[[i]] = c(rep(F, (i-1)*10), rep(T, 10), rep(F, (10-i)*10))

p <- p %>% 
  layout(
    title = "Month Wise Energy Production and Consumption from 2010 -- 2018",
    xaxis = list(title="Month", label= c('1','2','3','4','5','6','7','8','9','9','89', '100')),
    yaxis = list(title = "Energy (in MW)"),
    updatemenus = list(
        list(
        y = 0.7,
        buttons = list(
          list(method = "restyle",
               args = list("visible", as.list(t[[1]])),
               label = "2010"),

          list(method = "restyle",
               args = list("visible", as.list(t[[2]])),
               label = "2011"),
          
        list(method = "restyle",
               args = list("visible", as.list(t[[3]])),
               label = "2012"),
            
        list(method = "restyle",
               args = list("visible", as.list(t[[4]])),
               label = "2013"),

          list(method = "restyle",
               args = list("visible", as.list(t[[5]])),
               label = "2014"),
          
        list(method = "restyle",
               args = list("visible", as.list(t[[6]])),
               label = "2015"),
            
        list(method = "restyle",
               args = list("visible", as.list(t[[7]])),
               label = "2016"),

          list(method = "restyle",
               args = list("visible", as.list(t[[8]])),
               label = "2017"),
          
        list(method = "restyle",
               args = list("visible", as.list(t[[9]])),
               label = "2018"))
        )))

no_of_each_hour = table(df$Hour)
unique_hour = sort(unique(df$Hour))
bnw = data.frame(hour=NULL, type=NULL, mw=NULL)
temp = data.frame(hour=NULL, type=NULL, mw=NULL)
for (i in 1:length(unique_hour))
{
    for (j in 2:10)
    {
        temp = data.frame(hour = rep(unique_hour[i], no_of_each_hour[i]), 
                          type = rep(colnames(df)[j], no_of_each_hour[i]), mw=as.numeric( df[[j]][df[[18]]==unique_hour[i]]) )
        bnw = rbind(bnw, temp)
    }
}

bb=list()
for (i in 2:10)
{
 bb[[i-1]]=bnw[bnw$type==colnames(df)[i],]
}

options(repr.plot.width=4.05, repr.plot.height=4.05)
for (i in 1:length(bb))
{
    if(colours[i]=="black")
        colours[i]="darkgoldenrod1"
    boxplot(mw~hour,data=bb[[i]], main=colnames(df)[i+1], xlab="Hour", ylab="Electricity (in MW)", col=colours[i], outline=F)
}


monthwise = df[,2:12] %>%  group_by(Year, Month)  %>% summarise_each(funs(mean))
monthwise.ts = ts(monthwise, frequency=12, start=c(2010,1), end=c(2018,1))
monthwise_pred.ts = ts(monthwise, frequency=12, start=c(2010,1), end=c(2017,12))

monthwise_test = df_test[,2:11] %>%  group_by(Year, Month)  %>% summarise_each(funs(mean))
monthwise_test.ts = ts(monthwise_test, frequency=12, start=c(2018,1), end=c(2019,1))

monthwise_test_con_pred = df_test_con_pred[,2:4] %>%  group_by(Year, Month)  %>% summarise_each(funs(mean))
monthwise_test_con_pred.ts = ts(monthwise_test_con_pred, frequency=12, start=c(2018,1), end=c(2019,1))

colours = c("red", "green", "orange", "pink", "yellow", "purple", "black", "cyan", "blue", "darkgoldenrod1")

my_plot.decomposed.ts = function(df, monthwise.ts, start, end) 
{
    options(repr.plot.width=3.35, repr.plot.height=3.35)
  for (i in start:end)
    {
        decomposedRes <- decompose(monthwise.ts[,i], type="additive")
        plot(cbind(observed = decomposedRes$x, trend = decomposedRes$trend, seasonal = decomposedRes$seasonal, random = decomposedRes$random), 
             main=colnames(df)[i], col = colours[i-start+1], cex.main = 0.75, cex.lab = 0.75, cex.axis = 0.75)
    }
}
my_plot.decomposed.ts(df, monthwise.ts, 2,10)

colours = c("red", "green", "orange", "pink", "yellow", "purple", "black", "cyan", "blue", "darkgoldenrod1")

sortedm = df[,2:14] %>%  group_by(Year, Month)  %>% summarise_each(funs(sum))
for (i in 3:10)
    sortedm[paste0(colnames(sortedm)[i],"%")] = sortedm[,i]/sortedm[,11]*100
sortedm.ts = ts(sortedm, frequency=12, start=c(2010,1), end=c(2017,12))
my_plot.decomposed.ts(sortedm, sortedm.ts, 14,21)


sortedm = df[,2:14] %>%  group_by(Year, Month)  %>% summarise_each(funs(sum))
sortedm$renewable = sortedm$Hidroelectric_MW + sortedm$Wind_MW + sortedm$Solar_MW + sortedm$Biomass_MW
sortedm$renewable_percentage = sortedm$renewable/sortedm$Production_MW*100
avg_percentage = sum(sortedm$renewable)/sum(sortedm$Production_MW)*100
print(paste0('% of total electricity production from renewable energy sources: ', avg_percentage, '%'))
sortedm.ts = ts(sortedm, frequency=12, start=c(2010,1), end=c(2017,12))
my_plot.decomposed.ts(sortedm, sortedm.ts, length(sortedm), length(sortedm))

multi_stat_tests<- function(df){
    p <- ncol(df)
    make.ts<- function(x)
    {
        ts(x, frequency=12, start=c(2010,1), end=c(2018,1)) 
    }
    df_multi <- data.frame(#var=names(df),
                           adf.pvalue=sapply(df, function(v) adf.test(make.ts(v),alternative = "stationary", k=10)$p.value),
                           kpss.pvalue=sapply(df, function(v) kpss.test(make.ts(v))$p.value)
                           )
    df_multi$adf <- ifelse (df_multi$adf.pvalue < 0.05, "Stationary", "Non-Stationary") 
    df_multi$kpss <- ifelse(df_multi$kpss.pvalue > 0.05, "Stationary", "Non-Stationary")
    df_multi
}
df_stat_tests = as.data.frame(multi_stat_tests(monthwise[,3:11]))
df_stat_tests

p <- plot_ly()
for (i in 4:11)
{
    fit <- ets(monthwise_pred.ts[,i])
    fore <- forecast(fit,h=12,level=c(80,95),model=fit)

p = p %>%
add_lines(x = time(monthwise.ts[,i]), y = monthwise.ts[,i], color = I("black"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray95"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name = "prediction") %>%
  add_lines(x = time(monthwise_test.ts[,i-1]), y = monthwise_test.ts[,i-1], color = I("green"), name = "test")

}
t = list()
for (i in 1:8)
    t[[i]] = c(rep(F, (i-1)*5), rep(T, 5), rep(F, (8-i)*5))
p = p %>%
layout(
    title = "Time Series Forecasting Plots",
    xaxis = list(title="Year", label= c('1','2','3','4','5','6','7')),
    yaxis = list(title = "Energy (in MW)"),
    updatemenus = list(
        list(
        y = 0.7,
        #    type="buttons",
        buttons = list(
            list(method = "restyle",
               args = list("visible", as.list(t[[1]])),
               label = colnames(df)[3]),
            
            list(method = "restyle",
               args = list("visible", as.list(t[[2]])),
               label = colnames(df)[4]),
            
            list(method = "restyle",
               args = list("visible", as.list(t[[3]])),
               label = colnames(df)[5]),
            
            list(method = "restyle",
               args = list("visible", as.list(t[[4]])),
               label = colnames(df)[6]),
            
            list(method = "restyle",
               args = list("visible", as.list(t[[5]])),
               label = colnames(df)[7]),
            
            list(method = "restyle",
               args = list("visible", as.list(t[[6]])),
               label = colnames(df)[8]),
            
            list(method = "restyle",
               args = list("visible", as.list(t[[7]])),
               label = colnames(df)[9]),
            
            list(method = "restyle",
               args = list("visible", as.list(t[[8]])),
               label = colnames(df)[10]) )
          
        )))

fit <- ets(monthwise_pred.ts[,3])
fore <- forecast(fit,h=12,level=c(80,95),model=fit)

if(F){pp <- plot_ly() %>%
add_lines(x = time(monthwise.ts), y = monthwise.ts[,3], color = I("black"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray95"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name = "prediction") %>%
  add_lines(x = time(monthwise_test_con_pred.ts[,3]), y = monthwise_test_con_pred.ts[,3], color = I("green"), name = "predicted test values") %>%
  layout( title = "Forecast for Energy Consumption for 1 Year", xaxis = list(title ="Time"), yaxis = list(title = "Energy (in MW)"))
#pp
}      

sorted = df[,2:14] %>%  group_by(Year, Month, Day)  %>% summarise_each(funs(mean))
unique_years = unique(sorted$Year)
df_byday = list()
for (i in 1:length(unique_years))
{
    s = sorted[sorted$Year==unique_years[i],]
    tmp <- data.frame(x = s$Month, y = s$Day, z = s$Consumption_MW)
    df_byday[[i]] = acast(tmp, x~y, value.var="z")
}    
names(df_byday) = as.character(unique_years)

min_c = min(unlist(lapply(df_byday, function(x) min(x, na.rm=T))))
max_c = max(unlist(lapply(df_byday, function(x) max(x, na.rm=T))))
b = seq(min_c,max_c,length=366)
                          
colfunc <- colorRampPalette(c( "lightyellow","yellow", "orange", "red", "darkred", "black"))
options(repr.plot.width=5, repr.plot.height=5)
h = list()                          
for (i in 1:8)
{heatmap.2(df_byday[[i]], Rowv=F, Colv=F, dendrogram="none", scale="none", col=colfunc(365), trace="none", 
            margins=c(3.2,3), ColSideColors = colfunc(31), key = T, tracecol="cyan", keysize = 2.5, 
           breaks= b, density.info="histogram", xlab = "Day", ylab = "Month", na.rm=T,
            main=names(df_byday)[i], key.title = "Colour Key", key.xlab = "Consumption_MW", key.ylab = NULL, 
           key.par=list(cex.main=0.75, cex.axis=0.65,cex.lab=0.85), lwid = c(1,2), lhei= c(.855,2))
 }

sorted1 = df[,2:14] %>%  group_by(Month, Day)  %>% summarise_each(funs(mean))
all <- data.frame(x = sorted1$Month, y = sorted1$Day, z = sorted1$Consumption_MW)
all = acast(all, x~y, value.var="z")

min_c = min(all, na.rm=T)
max_c = max(all, na.rm=T)
b = seq(min_c,max_c,length=366)
heatmap.2(all, Rowv=F, Colv=F, dendrogram="none", scale="none", col=colfunc(365), trace="none", margins=c(3.2,3), 
          ColSideColors = colfunc(31), key = T, tracecol="cyan", keysize = 2.5, breaks= b, 
          density.info="histogram", xlab = "Day", ylab = "Month", na.rm=T,
           main="Heatmap Over 8 Years", key.title = "Colour Key", key.xlab = "Consumption_MW", key.ylab = NULL, 
           key.par=list(cex.main=0.75, cex.axis=0.65,cex.lab=0.85), lwid = c(1,2), lhei= c(.855,2))
