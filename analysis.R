rm(list=ls())
library(tidyverse)
setwd("~/Documents/COVID-19/")
deaths = read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
confirmed = read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

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
  out_df$ind_time = new_rows- as.Date("2020-1-1")
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
  ind_min = min(which(d >=30))
  ind_max = min(length(d),10 + ind_min)
  if (is.finite(ind_min) && ind_max - ind_min>=5){
    tps = (ind_min:ind_max) - ind_min
    d = log(d[ind_min:ind_max])
    print(ind_max)
    res = lm(d~tps)
    plot(res)
    croissance = c(croissance ,exp(res$coefficients[2]))
    tmp =data.frame(Country=c,growth=exp(res$coefficients[2]),ind_min = ind_min, ind_max = ind_max)
    results = rbind(results,tmp)
  }

  
}
rownames(results) = c()
results %>% arrange(growth)
