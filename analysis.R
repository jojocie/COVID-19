rm(list=ls())
library(tidyverse)
#install.packages('numDeriv',dependencies = TRUE)
library(numDeriv)
setwd("~/Documents/COVID-19/")
deaths = read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

confirmed = read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# params
by_country = TRUE



prepare_df = function(df, by_country =FALSE){
  provinces = df$Province.State;
  df =  df %>% select(-c(Lat,Long,Province.State)) # On conserve uniquement les variables de nombre
  
  if ( by_country) {
    df = df%>% group_by(Country.Region) %>%summarise_all(sum)
    new_columns = df$Country.Region
  } else {
    new_columns = provinces
  }
  df = df%>% select(-Country.Region)
  new_rows = colnames(df) %>%str_replace('X','0') %>%as.Date(format = '%m.%d.%y')
  out_df = as_tibble(t(df))
  colnames(out_df) = new_columns
  out_df$ind_date = new_rows
  out_df$ind_time = as.numeric(new_rows- as.Date("2020-1-1"))
  print(class(out_df$ind_time))
  #out_df  = out_df%>%column_to_rownames('date')
  return(out_df)
}

deaths = prepare_df(deaths,by_country)
confirmed = prepare_df(confirmed,by_country)
plot(log(deaths$France))

lm(deaths$France~deaths$ind_time)
res = lm(deaths[['France']] ~ deaths$ind_time)

index_columns = grep("ind_",colnames(deaths))
country_columns = colnames(deaths)[- index_columns]
croissance = NULL
results = data.frame(Country=character(),growth=double(),ind_min = integer(), ind_max = integer())
for (c in country_columns){
  d =  deaths[[c]]
  ind_min = min(which(d >=20))
  ind_max = min(length(d),10 + ind_min)
  if (is.finite(ind_min) && ind_max - ind_min>=5){
    tps = (ind_min:ind_max) - ind_min
    d = log(d[ind_min:ind_max])
    #print(ind_max)
    res = lm(d~tps)
    #plot(res)
    croissance = c(croissance ,exp(res$coefficients[2]))
    tmp =data.frame(Country=c,growth=exp(res$coefficients[2]),ind_min = ind_min, ind_max = ind_max)
    results = rbind(results,tmp)
  }

  
}
rownames(results) = c()
results %>% arrange(growth)



# mod_pred = function(x,p){
#   return(p[1] * atan(p[2] * (x-p[3]))/pi + p[1]/2)
#   
# }
mod_pred = function(x,p){
  ret_value = p[1] * pnorm(x,p[3],p[2])
  return(ret_value)
  
}
err = function(p,x,y){
  #w = 1/log(y)
  w=1
  diff =  abs(y - mod_pred(x,p))^2
  
  #return(sum(diff*w) + 0.1 * (norm(p,type = '2')))
  return(sum(diff*w))
  
}

#### MODELISATION ####

start_limits = seq(from = 200 , to =5000,length = 100)
#start_limits = 3000
indices = 10:(dim(deaths)[1]-7)
j1 = 50
j2 = 120
pays = 'China'



n_deaths = deaths[[pays]][indices]
jours = deaths$ind_time[indices]
jours_pour_pred =seq(from = j1 , to = j2 , by=1)

v_min = .Machine$double.xmax
p = c(3000,0.1,45)
for (l in start_limits){
  p[1] = max(l, max(n_deaths))
  #p[1] = ldeath  

  reg = optim(p,err,x=jours,y=n_deaths
              #,method = "L-BFGS-B"
              #,list('ndeps' = 1e-8, 'abstol' = 1e-6,'reltol' = 1e-12)
              )
  if (v_min > reg$value){
    v_min = reg$value
    best_reg = reg
  }
}  
u = t(as.matrix(c(0,1,0)))
c = 0
#reg_contrainte = constrOptim(theta =p, ui=u, ci = c,f=err,x=jours,y=n_deaths,grad = NULL)

p_tmp = best_reg$par
#p_tmp = c(3500,0.11,46)
plot(jours + as.Date("2020-01-01"),n_deaths,xlim = c(j1 + as.Date("2020-01-01"),j2 + as.Date("2020-01-01")),ylim = c(0,p_tmp[1]), main = pays,ylab = "nbr de morts",xlab='date')
par(new=T)
plot(jours_pour_pred+ as.Date("2020-01-01"),mod_pred(x = jours_pour_pred,p = p_tmp),xlim = c(j1 + as.Date("2020-01-01"),j2 + as.Date("2020-01-01")),ylim = c(0,p_tmp[1]),type = 'l',main = "",xlab = "", ylab = "")
grid()
#abline(v = as.Date("2020-01-01") + p_tmp[3])
print(p_tmp)
print(as.Date("2020-01-01") + p_tmp[3])











plot(jours,n_deaths,xlim = c(60,100),ylim = c(0,2000), main = pays)
par(new=T)
plot(jours_pour_pred,mod_pred(x = jours_pour_pred,p = p_tmp),c(60,100),ylim = c(0,2000),type = 'l',main = "")
grid()
print(p_tmp)
print(as.Date("2020-01-01") + p_tmp[3])
