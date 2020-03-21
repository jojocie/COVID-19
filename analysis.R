rm(list=ls())
library(tidyverse)
setwd("~/Documents/COVID-19/")
deaths = read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
confirmed = read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

prepare_df = function(df){
  df =  df %>% select(-c(Lat,Long,Province.State)) # On conserve uniquement les variables de nombre
  df = df%>% group_by(Country.Region) %>%summarise_all(sum)
  new_columns = df$Country.Region
  df = df%>% select(-Country.Region)
  new_rows = colnames(df) %>%str_replace('X','0') %>%as.Date(format = '%m.%d.%y')
  out_df = as_tibble(t(df))
  colnames(out_df) = new_columns
  #out_df$date = new_rows
  #out_df  = out_df%>%column_to_rownames('date')
  return(out_df)
}

deaths = prepare_df(deaths)
confirmed = prepare_df(confirmed)

countries_list =  deaths$Country.Region %>% unique() %>% sort()
country = "France"
country %in% countries_list 

deaths_fr = deaths%>%filter(Country.Region == 'Chile') %>% select(-c(Lat,Long,Province.State,Country.Region))
confirmed_country = confirmed%>%filter(Country.Region == 'Chile') %>% select(-c(Lat,Long,Province.State,Country.Region))
dates = colnames(confirmed_country) %>% str_replace('X','0') %>%as.Date(format = '%m.%d.%y')

serie = t(confirmed_country)
plot(dates,serie)
title(c("confirmed ",country))
