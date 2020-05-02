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
  
  #--- IHME data ---------------------------------------------------------------
  ihme.df <- read_csv(ihme_csv)
  
  
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





#' Update static peak plots 
refresh_peak_plots <- function(obs) {
  library(tidyverse)
  theme_set(theme_bw() + theme(panel.grid=element_blank()))
  
  country.p <- obs$obs.gl.max %>% filter(obs > 5) %>%
    arrange(Date, desc(Country)) %>% 
    mutate(Country=factor(Country, levels=unique(Country))) %>%
    ggplot(aes(x=Country, ymin=Date, ymax=Date-7, colour=obs*7)) + 
    geom_linerange(size=5) + coord_flip() + 
    scale_colour_gradient("Total deaths in 7-day period",  
                          low="#fff5f0" , high="red",
                          trans="log", limits=c(1, 20000),
                          breaks=c(2, 20, 200, 2e3, 2e4),
                          labels=c("2", "20", "200", "2,000", "20,000"), 
                          guide=guide_colorbar(title.position="top", 
                                               title.hjust=0.5,
                                               ticks.colour="black",
                                               ticks=T)) +
    scale_y_date(breaks=seq(max(obs$obs.gl$Date),
                            min(obs$obs.gl.max$Date), 
                            by=-7), minor_breaks=NULL,
                 labels=as.Date(seq(max(obs$obs.gl$Date),
                                    min(obs$obs.gl.max$Date), 
                                    by=-7), format="%Y_%m_%d") %>%
                   format("%b %d")) +
    scale_x_discrete(position="top") +
    theme(legend.position="bottom", 
          legend.box.margin=margin(-10,0,0,0),
          legend.key.width=unit(15, "mm"),
          legend.key.height=unit(2, "mm"),
          axis.text=element_text(size=14),
          legend.text=element_text(size=14),
          axis.title=element_text(size=16),
          legend.title=element_text(size=16), 
          title=element_text(size=18),
          panel.grid.major.x=element_line(size=0.25, colour="gray90")) +
    labs(x="", y="", title="Countries") 
  ggsave("www/country_peak.png", width=7.5, height=13.5, units="in")
  
  obs$obs.us.max %>% 
    arrange(Date, desc(State)) %>% 
    mutate(State=factor(State, levels=unique(State))) %>%
    ggplot(aes(x=State, ymin=Date, ymax=Date-7, colour=obs*7)) + 
    geom_linerange(size=5) + coord_flip() + 
    scale_colour_gradient("Total deaths in 7-day period",  
                          low="#fff5f0" , high="red",
                          trans="log", limits=c(1, 10000),
                          breaks=c(1, 10, 100, 1e3, 1e4),
                          labels=c("1", "10", "100", "1,000", "10,000"), 
                          guide=guide_colorbar(title.position="top", 
                                               title.hjust=0.5,
                                               ticks.colour="black",
                                               ticks=T)) +
    scale_y_date(breaks=seq(max(obs$obs.us$Date),
                            min(obs$obs.us.max$Date), 
                            by=-7), minor_breaks=NULL, 
                 labels=as.Date(seq(max(obs$obs.us$Date),
                                    min(obs$obs.us.max$Date), 
                                    by=-7), format="%Y_%m_%d") %>%
                   format("%b %d")) +
    scale_x_discrete(position="top") +
    theme(legend.position="bottom", 
          legend.box.margin=margin(-10,0,0,0),
          legend.key.width=unit(15, "mm"),
          legend.key.height=unit(2, "mm"),
          axis.text=element_text(size=14),
          legend.text=element_text(size=14),
          axis.title=element_text(size=16),
          legend.title=element_text(size=16), 
          title=element_text(size=18),
          panel.grid.major.x=element_line(size=0.25, colour="gray90")) +
    labs(x="", y="", title="US States")
  ggsave("www/states_peak.png", width=7.5, height=13.5, units="in")
} 

