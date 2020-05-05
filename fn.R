# Functions for the shiny app

#' Pretty labels for axes
pretty_numbers <- function(x) format(x, scientific=F, big.mark=",")





#' Update IHME csv with new model
refresh_IHME_csv <- function(ihme.dir="mods",
                             csv.out="ihme_compiled.csv") {
  library(tidyverse)
  ihme.f <- dir(ihme.dir, ".csv", recursive=T, full.names=T) %>%
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
  write_csv(ihme.df, csv.out)
}






#' Load datasets needed for shiny app
load_obs <- function(ihme_csv="ihme_compiled.csv",
                     jhu.repo=paste0("https://raw.githubusercontent.com/",
                                     "CSSEGISandData/",
                                     "COVID-19/master/")) {
  library(tidyverse)
  wkdays <- c("Su","M","T","W","R","F","Sa")
  ts.url <- paste0(jhu.repo, "csse_covid_19_data/csse_covid_19_time_series/")
  pop.gl <- read_csv("countryPops.csv") %>% mutate(pop_pK=Population/1e6)
  pop.us <- read_csv("statePops.csv") %>% mutate(pop_pK=Population/1e4)
  
  
  #--- IHME data ---------------------------------------------------------------
  ihme.df <- read_csv(ihme_csv)
  
  
  #--- Countries ---------------------------------------------------------------
  obs.d.gl <- read.csv(paste0(ts.url, "time_series_covid19_deaths_global.csv")) %>%
    pivot_longer(13:ncol(.), names_to="Date", values_to="deaths") %>%
    mutate(Date=lubridate::mdy(str_sub(Date, 2, -1L)),
           Country=Country.Region) %>%
    group_by(Country, Date) %>% summarise(Total=sum(deaths)) %>%
    group_by(Country) %>% arrange(Country, Date) %>%
    mutate(Daily=Total-lag(Total)) %>% ungroup %>%
    mutate(Country=as.character(Country),
           wDay=factor(wkdays[lubridate::wday(Date)], levels=wkdays)) %>%
    pivot_longer(3:4, names_to="span", values_to="Deaths.obs") %>%
    mutate(pop=pop.gl$pop_pK[match(Country, pop.gl$Country)])
  obs.d.gl.max <- obs.d.gl %>% filter(Country != "Diamond Princess") %>%
    group_by(Country) %>% 
    filter(!is.na(Deaths.obs) & span=="Daily") %>% arrange(Country, Date) %>% 
    mutate(obs.l1=lag(Deaths.obs), obs.l2=lag(Deaths.obs, 2),
           obs.l3=lag(Deaths.obs, 3), obs.l4=lag(Deaths.obs, 4),
           obs.l5=lag(Deaths.obs, 5), obs.l6=lag(Deaths.obs, 6),
           mn_7d=(Deaths.obs+obs.l1+obs.l2+obs.l3+obs.l4+obs.l5+obs.l6)/7) %>%
    filter(!is.na(mn_7d)) %>% arrange(Country, mn_7d) %>% group_by(Country) %>% 
    summarise(Date=last(Date), obs=last(mn_7d), pop=first(pop))
  obs.c.gl <- read.csv(paste0(ts.url, "time_series_covid19_confirmed_global.csv")) %>%
    pivot_longer(13:ncol(.), names_to="Date", values_to="deaths") %>%
    mutate(Date=lubridate::mdy(str_sub(Date, 2, -1L)),
           Country=Country.Region) %>%
    group_by(Country, Date) %>% summarise(Total=sum(deaths)) %>%
    group_by(Country) %>% arrange(Country, Date) %>%
    mutate(Daily=Total-lag(Total)) %>% ungroup %>%
    mutate(Country=as.character(Country),
           wDay=factor(wkdays[lubridate::wday(Date)], levels=wkdays)) %>%
    pivot_longer(3:4, names_to="span", values_to="Cases.obs") %>%
    mutate(pop=pop.gl$pop_pK[match(Country, pop.gl$Country)])
  obs.c.gl.max <- obs.c.gl %>% filter(Country != "Diamond Princess") %>%
    group_by(Country) %>% 
    filter(!is.na(Cases.obs) & span=="Daily") %>% arrange(Country, Date) %>% 
    mutate(obs.l1=lag(Cases.obs), obs.l2=lag(Cases.obs, 2),
           obs.l3=lag(Cases.obs, 3), obs.l4=lag(Cases.obs, 4),
           obs.l5=lag(Cases.obs, 5), obs.l6=lag(Cases.obs, 6),
           mn_7d=(Cases.obs+obs.l1+obs.l2+obs.l3+obs.l4+obs.l5+obs.l6)/7) %>%
    filter(!is.na(mn_7d)) %>% arrange(Country, mn_7d) %>% group_by(Country) %>% 
    summarise(Date=last(Date), obs=last(mn_7d), pop=first(pop))
  gl.df <- full_join(filter(ihme.df, Country %in% obs.d.gl$Country & 
                                Country != "Georgia"), 
                       filter(obs.d.gl, Country %in% ihme.df$Country), 
                       by=c("Date", "Country", "span")) %>%
    filter(!is.na(model_date)) %>%
    mutate(modelDate=as.Date(model_date, format="%Y_%m_%d") %>%
             format("%b %d")) %>%
    mutate(pop=pop.gl$pop_pK[match(Country, pop.gl$Country)])
  
  
  #--- US States ---------------------------------------------------------------
  obs.d.us <- read.csv(paste0(ts.url, "time_series_covid19_deaths_US.csv")) %>%
    pivot_longer(13:ncol(.), names_to="Date", values_to="deaths") %>%
    mutate(Date=lubridate::mdy(str_sub(Date, 2, -1L)),
           State=Province_State) %>%
    group_by(State, Date) %>% summarise(Total=sum(deaths)) %>%
    group_by(State) %>% arrange(State, Date) %>%
    mutate(Daily=Total-lag(Total)) %>% ungroup %>%
    mutate(State=as.character(State),
           wDay=factor(wkdays[lubridate::wday(Date)], levels=wkdays)) %>%
    pivot_longer(3:4, names_to="span", values_to="Deaths.obs") %>%
    mutate(pop=pop.us$pop_pK[match(State, pop.us$State)])
  obs.d.us.max <- obs.d.us %>% 
    filter(State %in% c(state.name, "Puerto Rico", "District of Columbia")) %>% 
    group_by(State) %>% 
    filter(!is.na(Deaths.obs) & span=="Daily") %>% arrange(State, Date) %>% 
    mutate(obs.l1=lag(Deaths.obs), obs.l2=lag(Deaths.obs, 2),
           obs.l3=lag(Deaths.obs, 3), obs.l4=lag(Deaths.obs, 4),
           obs.l5=lag(Deaths.obs, 5), obs.l6=lag(Deaths.obs, 6),
           mn_7d=(Deaths.obs+obs.l1+obs.l2+obs.l3+obs.l4+obs.l5+obs.l6)/7) %>%
    filter(!is.na(mn_7d)) %>% arrange(State, mn_7d) %>% group_by(State) %>% 
    summarise(Date=last(Date), obs=last(mn_7d), pop=first(pop))
  obs.c.us <- read.csv(paste0(ts.url, "time_series_covid19_confirmed_US.csv")) %>%
    pivot_longer(13:ncol(.), names_to="Date", values_to="deaths") %>%
    mutate(Date=lubridate::mdy(str_sub(Date, 2, -1L)),
           State=Province_State) %>%
    group_by(State, Date) %>% summarise(Total=sum(deaths)) %>%
    group_by(State) %>% arrange(State, Date) %>%
    mutate(Daily=Total-lag(Total)) %>% ungroup %>%
    mutate(State=as.character(State),
           wDay=factor(wkdays[lubridate::wday(Date)], levels=wkdays)) %>%
    pivot_longer(3:4, names_to="span", values_to="Cases.obs") %>%
    mutate(pop=pop.us$pop_pK[match(State, pop.us$State)])
  obs.c.us.max <- obs.c.us %>% 
    filter(State %in% c(state.name, "Puerto Rico", "District of Columbia")) %>% 
    group_by(State) %>% 
    filter(!is.na(Cases.obs) & span=="Daily") %>% arrange(State, Date) %>% 
    mutate(obs.l1=lag(Cases.obs), obs.l2=lag(Cases.obs, 2),
           obs.l3=lag(Cases.obs, 3), obs.l4=lag(Cases.obs, 4),
           obs.l5=lag(Cases.obs, 5), obs.l6=lag(Cases.obs, 6),
           mn_7d=(Cases.obs+obs.l1+obs.l2+obs.l3+obs.l4+obs.l5+obs.l6)/7) %>%
    filter(!is.na(mn_7d)) %>% arrange(State, mn_7d) %>% group_by(State) %>% 
    summarise(Date=last(Date), obs=last(mn_7d), pop=first(pop))
  us.df <- full_join(ihme.df %>% rename(State=Country) %>% 
                       filter(State %in% unique(obs.d.us$State)), 
                     filter(obs.d.us, State %in% unique(ihme.df$Country)), 
                     by=c("State", "Date", "span")) %>% group_by(State) %>%
    mutate(pop=pop.us$pop_pK[match(State, pop.us$State)])
  
  return(list(obs.d.gl=obs.d.gl, obs.d.gl.max=obs.d.gl.max, 
              obs.c.gl=obs.c.gl, obs.c.gl.max=obs.c.gl.max,
              gl.df=gl.df,
              obs.d.us=obs.d.us, obs.d.us.max=obs.d.us.max, 
              obs.c.us=obs.c.us, obs.c.us.max=obs.c.us.max, 
              us.df=us.df))
}





#' Update static peak plots 
refresh_peak_plots <- function(obs) {
  library(tidyverse)
  theme_set(theme_bw() + theme(panel.grid=element_blank()))
  
  country.d <- obs$obs.d.gl.max %>% filter(obs > 5) %>%
    arrange(Date, desc(Country)) %>% 
    mutate(Country=factor(Country, levels=unique(Country))) %>%
    ggplot(aes(x=Country, ymin=Date, ymax=Date-7, colour=obs*7)) + 
    geom_linerange(size=5) + coord_flip() + 
    scale_colour_gradient("Total new deaths in 7-day period",  
                          low="#fff5f0" , high="red",
                          trans="log", limits=c(1, 20000),
                          breaks=c(2, 20, 200, 2e3, 2e4),
                          labels=c("2", "20", "200", "2,000", "20,000"), 
                          guide=guide_colorbar(title.position="top", 
                                               title.hjust=0.5,
                                               ticks.colour="black",
                                               ticks=T)) +
    scale_y_date(breaks=seq(max(obs$obs.d.gl$Date),
                            min(obs$obs.d.gl.max$Date), 
                            by=-7), minor_breaks=NULL,
                 labels=as.Date(seq(max(obs$obs.d.gl$Date),
                                    min(obs$obs.d.gl.max$Date), 
                                    by=-7), format="%Y_%m_%d") %>%
                   format("%b %d")) +
    scale_x_discrete(position="top") +
    theme(legend.position="bottom", 
          legend.box.margin=margin(-10,0,0,0),
          legend.key.width=unit(20, "mm"),
          legend.key.height=unit(2, "mm"),
          axis.text=element_text(size=14),
          legend.text=element_text(size=14),
          axis.title=element_text(size=16),
          legend.title=element_text(size=16), 
          title=element_text(size=18),
          panel.grid.major.x=element_line(size=0.25, colour="gray90")) +
    labs(x="", y="", title="Countries") 
  ggsave("www/country_d_peak.png", country.d, width=7.5, height=13.5, units="in")
  
  country.c <- obs$obs.c.gl.max %>% filter(obs > 100) %>%
    arrange(Date, desc(Country)) %>% 
    mutate(Country=factor(Country, levels=unique(Country))) %>%
    ggplot(aes(x=Country, ymin=Date, ymax=Date-7, colour=obs*7)) + 
    geom_linerange(size=4) + coord_flip() + 
    scale_colour_gradient("Total new cases in 7-day period",  
                          low="#fff5f0" , high="red",
                          trans="log", limits=c(1, 300000),
                          breaks=c(30, 300, 3e3, 3e4, 3e5),
                          labels=c("30", "300", "3000", "30,000", "300,000"), 
                          guide=guide_colorbar(title.position="top", 
                                               title.hjust=0.5,
                                               ticks.colour="black",
                                               ticks=T)) +
    scale_y_date(breaks=seq(max(obs$obs.c.gl$Date),
                            min(obs$obs.c.gl.max$Date), 
                            by=-14), minor_breaks=NULL,
                 labels=as.Date(seq(max(obs$obs.c.gl$Date),
                                    min(obs$obs.c.gl.max$Date), 
                                    by=-14), format="%Y_%m_%d") %>%
                   format("%b %d")) +
    scale_x_discrete(position="top") +
    theme(legend.position="bottom", 
          legend.box.margin=margin(-10,0,0,0),
          legend.key.width=unit(20, "mm"),
          legend.key.height=unit(2, "mm"),
          axis.text=element_text(size=12),
          legend.text=element_text(size=14),
          axis.title=element_text(size=16),
          legend.title=element_text(size=16), 
          title=element_text(size=18),
          panel.grid.major.x=element_line(size=0.25, colour="gray90")) +
    labs(x="", y="", title="Countries") 
  ggsave("www/country_c_peak.png", country.c, width=7.5, height=13.5, units="in")
  
  us.d <- obs$obs.d.us.max %>% 
    arrange(Date, desc(State)) %>% 
    mutate(State=factor(State, levels=unique(State))) %>%
    ggplot(aes(x=State, ymin=Date, ymax=Date-7, colour=obs*7)) + 
    geom_linerange(size=5) + coord_flip() + 
    scale_colour_gradient("Total new deaths in 7-day period",  
                          low="#fff5f0" , high="red",
                          trans="log", limits=c(1, 10000),
                          breaks=c(1, 10, 100, 1e3, 1e4),
                          labels=c("1", "10", "100", "1,000", "10,000"), 
                          guide=guide_colorbar(title.position="top", 
                                               title.hjust=0.5,
                                               ticks.colour="black",
                                               ticks=T)) +
    scale_y_date(breaks=seq(max(obs$obs.d.us$Date),
                            min(obs$obs.d.us.max$Date), 
                            by=-7), minor_breaks=NULL, 
                 labels=as.Date(seq(max(obs$obs.d.us$Date),
                                    min(obs$obs.d.us.max$Date), 
                                    by=-7), format="%Y_%m_%d") %>%
                   format("%b %d")) +
    scale_x_discrete(position="top") +
    theme(legend.position="bottom", 
          legend.box.margin=margin(-10,0,0,0),
          legend.key.width=unit(20, "mm"),
          legend.key.height=unit(2, "mm"),
          axis.text=element_text(size=14),
          legend.text=element_text(size=14),
          axis.title=element_text(size=16),
          legend.title=element_text(size=16), 
          title=element_text(size=18),
          panel.grid.major.x=element_line(size=0.25, colour="gray90")) +
    labs(x="", y="", title="US States")
  ggsave("www/states_d_peak.png", us.d, width=7.5, height=13.5, units="in")
  
  us.c <- obs$obs.c.us.max %>% 
    arrange(Date, desc(State)) %>% 
    mutate(State=factor(State, levels=unique(State))) %>%
    ggplot(aes(x=State, ymin=Date, ymax=Date-7, colour=obs*7)) + 
    geom_linerange(size=5) + coord_flip() + 
    scale_colour_gradient("Total new cases in 7-day period",  
                          low="#fff5f0" , high="red",
                          trans="log", limits=c(1, 70000),
                          breaks=c(7, 70, 700, 7e3, 7e4),
                          labels=c("7", "70", "700", "7,000", "70,000"), 
                          guide=guide_colorbar(title.position="top", 
                                               title.hjust=0.5,
                                               ticks.colour="black",
                                               ticks=T)) +
    scale_y_date(breaks=seq(max(obs$obs.c.us$Date),
                            min(obs$obs.c.us.max$Date), 
                            by=-7), minor_breaks=NULL, 
                 labels=as.Date(seq(max(obs$obs.c.us$Date),
                                    min(obs$obs.c.us.max$Date), 
                                    by=-7), format="%Y_%m_%d") %>%
                   format("%b %d")) +
    scale_x_discrete(position="top") +
    theme(legend.position="bottom", 
          legend.box.margin=margin(-10,0,0,0),
          legend.key.width=unit(20, "mm"),
          legend.key.height=unit(2, "mm"),
          axis.text=element_text(size=14),
          legend.text=element_text(size=14),
          axis.title=element_text(size=16),
          legend.title=element_text(size=16), 
          title=element_text(size=18),
          panel.grid.major.x=element_line(size=0.25, colour="gray90")) +
    labs(x="", y="", title="US States")
  ggsave("www/states_c_peak.png", us.c, width=7.5, height=13.5, units="in")
} 

