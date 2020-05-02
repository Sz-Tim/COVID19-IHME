# Functions for the shiny app


pretty_numbers <- function(x) format(x, scientific=F, big.mark=",")


load_obs <- function() {
  library(tidyverse)
  wkdays <- c("Su","M","T","W","R","F","Sa")
  ts.url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/",
                   "COVID-19/master/csse_covid_19_data/",
                   "csse_covid_19_time_series/")
  
  #--- IHME data ---------------------------------------------------------------
  ihme.f <- dir("mods", ".csv", recursive=T, full.names=T) %>%
    setNames(., str_split_fixed(., "/", 3)[,2])
  ihme.df <- map2_dfr(ihme.f, names(ihme.f), 
                      ~suppressMessages(suppressWarnings(read_csv(.x))) %>% 
                        select(location_name, 
                               contains("date"), 
                               contains("death")) %>%
                        rename(Date=contains("date"),
                               Country=location_name) %>%
                        mutate(model_date=.y)) 
  ihme.df <- ihme.df %>% group_by(Country, model_date) %>% 
    mutate(deaths_tot_mn=cumsum(deaths_mean),
           deaths_tot_lower=cumsum(deaths_lower),
           deaths_tot_upper=cumsum(deaths_upper),
           modeled=Date > as.Date(model_date, format="%Y_%m_%d")) %>%
    select(model_date, Country, Date, deaths_mean, deaths_tot_mn) %>%
    rename(Daily=deaths_mean, Total=deaths_tot_mn) %>%
    pivot_longer(4:5, names_to="span", values_to="Deaths")
  ihme.df$Country[ihme.df$Country=="United States of America"] <- "US"
  
  
  #--- Countries ---------------------------------------------------------------
  obs.gl <- read.csv(paste0(ts.url, "time_series_covid19_deaths_global.csv")) %>%
    pivot_longer(13:ncol(.), names_to="Date", values_to="deaths") %>%
    mutate(Date=lubridate::mdy(str_sub(Date, 2, -1L)),
           Country=Country.Region) %>%
    group_by(Country, Date) %>% summarise(Total=sum(deaths)) %>%
    group_by(Country) %>% arrange(Country, Date) %>%
    mutate(Daily=Total-lag(Total)) %>% ungroup %>%
    mutate(Country=as.character(Country),
           wDay=factor(wkdays[lubridate::wday(Date)], levels=wkdays)) %>%
    pivot_longer(3:4, names_to="span", values_to="Deaths.obs")
  obs.gl.max <- obs.gl %>% filter(Country != "Diamond Princess") %>%
    group_by(Country) %>% 
    filter(!is.na(Deaths.obs) & span=="Daily") %>% arrange(Country, Date) %>% 
    mutate(obs.l1=lag(Deaths.obs), obs.l2=lag(Deaths.obs, 2),
           obs.l3=lag(Deaths.obs, 3), obs.l4=lag(Deaths.obs, 4),
           obs.l5=lag(Deaths.obs, 5), obs.l6=lag(Deaths.obs, 6),
           mn_7d=(Deaths.obs+obs.l1+obs.l2+obs.l3+obs.l4+obs.l5+obs.l6)/7) %>%
    filter(!is.na(mn_7d)) %>% arrange(Country, mn_7d) %>% group_by(Country) %>% 
    summarise(Date=last(Date), obs=last(mn_7d))
  
  gl.df <- full_join(filter(ihme.df, Country %in% obs.gl$Country & 
                                Country != "Georgia"), 
                       filter(obs.gl, Country %in% ihme.df$Country), 
                       by=c("Date", "Country", "span")) %>%
    filter(!is.na(model_date)) %>%
    mutate(modelDate=as.Date(model_date, format="%Y_%m_%d") %>%
             format("%b %d"))
  
  
  #--- US States ---------------------------------------------------------------
  obs.us <- read.csv(paste0(ts.url, "time_series_covid19_deaths_US.csv")) %>%
    pivot_longer(13:ncol(.), names_to="Date", values_to="deaths") %>%
    mutate(Date=lubridate::mdy(str_sub(Date, 2, -1L)),
           State=Province_State) %>%
    group_by(State, Date) %>% summarise(Total=sum(deaths)) %>%
    group_by(State) %>% arrange(State, Date) %>%
    mutate(Daily=Total-lag(Total)) %>% ungroup %>%
    mutate(State=as.character(State),
           wDay=factor(wkdays[lubridate::wday(Date)], levels=wkdays)) %>%
    pivot_longer(3:4, names_to="span", values_to="Deaths.obs")
  obs.us.max <- obs.us %>% 
    filter(State %in% c(state.name, "Puerto Rico", "District of Columbia")) %>% 
    group_by(State) %>% 
    filter(!is.na(Deaths.obs) & span=="Daily") %>% arrange(State, Date) %>% 
    mutate(obs.l1=lag(Deaths.obs), obs.l2=lag(Deaths.obs, 2),
           obs.l3=lag(Deaths.obs, 3), obs.l4=lag(Deaths.obs, 4),
           obs.l5=lag(Deaths.obs, 5), obs.l6=lag(Deaths.obs, 6),
           mn_7d=(Deaths.obs+obs.l1+obs.l2+obs.l3+obs.l4+obs.l5+obs.l6)/7) %>%
    filter(!is.na(mn_7d)) %>% arrange(State, mn_7d) %>% group_by(State) %>% 
    summarise(Date=last(Date), obs=last(mn_7d))
  
  us.df <- full_join(ihme.df %>% rename(State=Country) %>% 
                       filter(State %in% unique(obs.us$State)), 
                     filter(obs.us, State %in% unique(ihme.df$Country)), 
                     by=c("State", "Date", "span")) %>% group_by(State) 
  
  return(list(obs.gl=obs.gl, obs.gl.max=obs.gl.max, gl.df=gl.df,
              obs.us=obs.us, obs.us.max=obs.us.max, us.df=us.df))
}






