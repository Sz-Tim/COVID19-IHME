

library(shiny); library(tidyverse)
theme_set(theme_bw() + theme(panel.grid=element_blank()))
source("fn.R")
obs <- load_obs()
refresh_peak_plots(obs)
latest.mod.Ymd <- max(obs$gl.df$model_date)
latest.mod.bd <- format(as.Date(latest.mod.Ymd, format="%Y_%m_%d"), "%b %d")



# Define UI for application
ui <- navbarPage("COVID-19 Data Trends", theme=shinythemes::shinytheme("yeti"),
                 
        navbarMenu("Countries",
            tabPanel("Deaths",
                     tags$h3("How well have models predicted", tags$b("mortality"), "for countries?"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(inputId="d.country",
                                         label="Choose a country",
                                         choices=sort(unique(obs$gl.df$Country)),
                                         selected="US"),
                             checkboxGroupInput(inputId="d.modSource.gl",
                                                label="Select models to show",
                                                choices=c("IHME", "MIT"),
                                                selected=c("IHME", "MIT"),
                                                inline=TRUE),
                             selectInput(inputId="d.modDates.gl",
                                         label="Choose IHME model releases to show",
                                         choices=setNames(unique(obs$gl.df$model_date),
                                                          as.Date(unique(obs$gl.df$model_date),
                                                                  format="%Y_%m_%d") %>%
                                                              format("%b %d")),
                                         selected=c("2020_03_25", "2020_05_04",
                                                    "2020_04_16", latest.mod.Ymd),
                                         multiple=TRUE),
                             dateRangeInput(inputId="d.dates.gl",
                                            label="Choose dates to display",
                                            start="2020-03-01", end="2020-07-01",
                                            min="2020-01-03", max="2020-08-04"),
                             checkboxInput(inputId="d.pK.gl",
                                           label="Display per million people",
                                           value=FALSE),
                             tags$hr(),
                             "The", tags$b("points"), "show reported deaths, with the", tags$b("point color"), "indicating the day of the week (darkest = weekends), and the", tags$b("gray line"), "as the smoothed average. The", tags$b("model lines"), "show only the", tags$em("mean predictions,"), "starting from the date the model was released (i.e., the 'Apr 01' model starts on April 01). Currently, only the latest MIT model is available to compare. The vertical", tags$b("dotted line"), "shows the end of the deadliest 7-day period.",
                             tags$hr(),
                             "Mar 25: original release", tags$br(),
                             "Apr 16: most optimistic for US", tags$br(),
                             "May 04: most pessimistic for US", tags$br(),
                             paste0(latest.mod.bd, ":"), "most recent"
                         ),
                         mainPanel(plotOutput(outputId="country.d", width="100%"))
                     )
            ),
            tabPanel("Cases",
                     tags$h3("How are", tags$b("confirmed cases"), "changing among countries?"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(inputId="c.country",
                                         label="Choose a country",
                                         choices=sort(unique(obs$obs.c.gl$Country)),
                                         selected="US"),
                             checkboxGroupInput(inputId="c.modSource.gl",
                                                label="Select models to show",
                                                choices=c("MIT"),
                                                selected=c("MIT"),
                                                inline=TRUE),
                             dateRangeInput(inputId="c.dates.gl",
                                            label="Choose dates to display",
                                            start="2020-03-01", end=Sys.Date()+14,
                                            min="2020-01-03", max=Sys.Date()+14),
                             checkboxInput(inputId="c.pK.gl",
                                           label="Display per million people",
                                           value=FALSE),
                             tags$hr(),
                             "The", tags$b("points"), "show reported cases, with the", tags$b("point color"), "indicating the day of the week (darkest = weekends), and the", tags$b("gray line"), "as the smoothed average. The vertical", tags$b("dotted line"), "shows the end of the 7-day period with the most new cases."
                         ),
                         mainPanel(plotOutput(outputId="country.c", width="100%"))
                     )
            ),
            tabPanel("Compare",
                     tags$h3("How do countries compare?"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(inputId="comp.country",
                                         label="Choose countries to show",
                                         choices=sort(unique(obs$obs.c.gl$Country)),
                                         selected=c("Switzerland", "Italy", "US"),
                                         multiple=TRUE),
                             dateRangeInput(inputId="comp.dates.gl",
                                            label="Choose dates to display",
                                            start="2020-03-01", end=Sys.Date(),
                                            min="2020-01-03", max=Sys.Date()),
                             checkboxInput(inputId="comp.pK.gl",
                                           label="Display per million people",
                                           value=TRUE),
                             tags$hr(),
                             "The", tags$b("points"), "show reported cases or deaths with the", tags$b("lines"), "as the smoothed averages."
                         ),
                         mainPanel(plotOutput(outputId="country.comp", width="100%"))
                     )
            )
        ),
        navbarMenu("US States",
            tabPanel("Deaths",
                     tags$h3("How well have models predicted", tags$b("mortality"), "for US states?"), 
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(inputId="d.state",
                                         label="Choose a state",
                                         choices=sort(unique(obs$us.df$State)),
                                         selected="Colorado"),
                             checkboxGroupInput(inputId="d.modSource.us",
                                                label="Select models to show",
                                                choices=c("IHME", "MIT"),
                                                selected=c("IHME", "MIT"),
                                                inline=TRUE),
                             selectInput(inputId="d.modDates.us",
                                         label="Choose model releases to show",
                                         choices=setNames(unique(obs$us.df$model_date),
                                                          as.Date(unique(obs$us.df$model_date),
                                                                  format="%Y_%m_%d") %>%
                                                              format("%b %d")),
                                         selected=c("2020_03_25", "2020_05_04",
                                                    "2020_04_16", latest.mod.Ymd),
                                         multiple=TRUE),
                             dateRangeInput(inputId="d.dates.us",
                                            label="Choose dates to display",
                                            start="2020-03-01", end="2020-07-01",
                                            min="2020-01-03", max="2020-08-04"),
                             checkboxInput(inputId="d.pK.us",
                                           label="Display per 10,000 people",
                                           value=FALSE),
                             tags$hr(),
                             "The", tags$b("points"), "show reported deaths, with the", tags$b("point color"), "indicating the day of the week (darkest = weekends), and the", tags$b("gray line"), "as the smoothed average. The", tags$b("model lines"), "show only the", tags$em("mean predictions,"), "starting from the date the model was released (i.e., the 'Apr 01' model starts on April 01). The vertical", tags$b("dotted line"), "shows the end of the deadliest 7-day period.",
                             tags$hr(),
                             "Mar 25: original release", tags$br(),
                             "Apr 16: most optimistic for US", tags$br(),
                             "May 04: most pessimistic for US", tags$br(),
                             paste0(latest.mod.bd, ":"), "most recent"
                         ),
                         mainPanel(plotOutput(outputId="state.d", width="100%"))
                     )
            ),
            tabPanel("Cases",
                     tags$h3("How are", tags$b("confirmed cases"), "changing among US states?"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(inputId="c.state",
                                         label="Choose a state",
                                         choices=sort(unique(obs$us.df$State)),
                                         selected="Colorado"),
                             checkboxGroupInput(inputId="c.modSource.us",
                                                label="Select models to show",
                                                choices=c("MIT"),
                                                selected=c("MIT"),
                                                inline=TRUE),
                             dateRangeInput(inputId="c.dates.us",
                                            label="Choose dates to display",
                                            start="2020-03-01", end=Sys.Date()+14,
                                            min="2020-01-03", max=Sys.Date()+14),
                             checkboxInput(inputId="c.pK.us",
                                           label="Display per 10,000 people",
                                           value=FALSE),
                             tags$hr(),
                             "The", tags$b("points"), "show reported deaths, with the", tags$b("point color"), "indicating the day of the week (darkest = weekends), and the", tags$b("gray line"), "as the smoothed average. The vertical", tags$b("dotted line"), "shows the end of the 7-day period with the most new cases."
                         ),
                         mainPanel(plotOutput(outputId="state.c", width="100%")),
                     )
            ),
            tabPanel("Compare",
                     tags$h3("How do states compare?"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(inputId="comp.state",
                                         label="Choose states to show",
                                         choices=sort(unique(obs$obs.c.us$State)),
                                         selected=c("Colorado", "Georgia", "Illinois"),
                                         multiple=TRUE),
                             dateRangeInput(inputId="comp.dates.us",
                                            label="Choose dates to display",
                                            start="2020-03-01", end=Sys.Date(),
                                            min="2020-01-03", max=Sys.Date()),
                             checkboxInput(inputId="comp.pK.us",
                                           label="Display per 10k people",
                                           value=TRUE),
                             tags$hr(),
                             "The", tags$b("points"), "show reported cases or deaths with the", tags$b("lines"), "as the smoothed averages."
                         ),
                         mainPanel(plotOutput(outputId="state.comp", width="100%"))
                     )
            )
        ),
        navbarMenu(title="Peaks",
            tabPanel("Deaths",
                     tags$h3("Which weeks have had the", tags$b("most deaths"), "so far?"),
                     fluidRow(
                         column(6, tags$img(src="country_d_peak.png", width='95%')),
                         column(6, tags$img(src="states_d_peak.png", width='95%'))
                     )
            ),
            tabPanel("Cases",
                     tags$h3("Which weeks have had the", tags$b("most new cases"), "so far?"),
                     fluidRow(
                         column(6, tags$img(src="country_c_peak.png", width='95%')),
                         column(6, tags$img(src="states_c_peak.png", width='95%'))
                     )
            ),
            tabPanel("Compare",
                     tags$h3("How much time is between the peaks in", tags$b("new cases"), "and", tags$b("deaths"), "?"),
                     fluidRow(
                         column(6, tags$img(src="country_comp_peak.png", width='95%')),
                         column(6, tags$img(src="states_comp_peak.png", width='95%'))
                     )
            )
        ),
        
        tabPanel("About",
                 fluidRow(column(6, offset=3, align="center", 
                                 "This site allows you to compare the mortality predicted by COVID-19 models with the mortality we've actually seen so far, see the trend in number of confirmed cases, and compare the patterns among countries or US states.", 
                                 tags$br(), 
                                 tags$br(), 
                                 "The models produced by the", tags$a("Institute for Health Metrics and Evaluation", href="http://www.healthdata.org/covid/data-downloads"), "(IHME), also known as the 'Chris Murray models', originally used a simple curve-fitting approach, but have built in additional complexity with time. The", tags$a("MIT", href="https://www.covidanalytics.io/projections"), "model uses machine-learning techniques along with a more traditional SEIR disease model.",
                                 tags$br(), 
                                 tags$br(), 
                                 "The observed data come from", tags$a("Johns Hopkins.", href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data"), "IHME releases updated models approximately twice each week, while both MIT and Johns Hopkins releases new predictions or data each day. No corrections have been made for testing effort.")),
                 tags$hr(),
                 fluidRow(column(12, align="center",
                                 tags$br(),
                                 "All code is published on",
                                 tags$a("GitHub", href="https://github.com/Sz-Tim/COVID19-IHME"),
                                 tags$br(),
                                 "Created by ",
                                 tags$a("Tim Szewczyk", href="https://sz-tim.github.io/about/")
                 ))
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ###---- Reactives: Countries
    obs.comp.gl <- reactive({
        bind_rows(
            obs$obs.c.gl %>% ungroup %>%
                filter(Country %in% input$comp.country & !is.na(Cases.obs) &
                           Date >= input$comp.dates.gl[1] & 
                           Date <= input$comp.dates.gl[2]) %>%
                mutate(Type="Cases",
                       obs=ifelse(rep(input$comp.pK.gl, n()), 
                                  Cases.obs/pop, Cases.obs)) %>%
                select(-pop, -wDay, -Cases.obs),
            obs$obs.d.gl %>% ungroup %>%
                filter(Country %in% input$comp.country & !is.na(Deaths.obs) &
                           Date >= input$comp.dates.gl[1] & 
                           Date <= input$comp.dates.gl[2]) %>%
                mutate(Type="Deaths",
                       obs=ifelse(rep(input$comp.pK.gl, n()), 
                                  Deaths.obs/pop, Deaths.obs)) %>%
                select(-pop, -wDay, -Deaths.obs)
        ) %>% arrange(desc(span), Type) %>%
            mutate(SpanType=factor(paste(span, Type), 
                                   levels=c(unique(paste(span, Type)))))
    })
    obs.lab.comp.gl <- reactive({
        tibble(SpanType=factor(levels(obs.comp.gl()$SpanType)[1], 
                               levels=levels(obs.comp.gl()$SpanType)),
               Date=input$comp.dates.gl[1], 
               obs=max(obs.comp.gl()$obs),
               lab=ifelse(input$comp.pK.gl, "per million", ""))
    })
    c.mit.gl.i <- reactive({
        obs$mit.gl %>%
            filter(Country==input$c.country &
                       Date >= input$c.dates.gl[1] & Date <= input$c.dates.gl[2]) %>%
            mutate(Cases=ifelse(rep(input$c.pK.gl, n()), Cases/pop, Cases),
                   modType="MIT")
    })
    obs.c.gl.i <- reactive({
        obs$obs.c.gl %>% ungroup %>%
            filter(Country==input$c.country & !is.na(Cases.obs) &
                       Date >= input$c.dates.gl[1] & Date <= input$c.dates.gl[2]) %>%
            mutate(src="Average",
                   Cases.obs=ifelse(rep(input$c.pK.gl, n()), 
                                    Cases.obs/pop, Cases.obs))
    })
    obs.max.c.gl.i <- reactive({
        obs$obs.c.gl.max %>% filter(Country==input$c.country) %>%
            filter(Date >= input$c.dates.gl[1] & Date <= input$c.dates.gl[2]) %>%
            mutate(obs=ifelse(input$c.pK.gl, obs/pop, obs))
    })
    obs.lab.c.gl <- reactive({
        tibble(span=c("Total", "Daily"), lab.size=c(6,5),
               Date=c(input$c.dates.gl[1], obs.max.c.gl.i()$Date),
               Cases=c(max(c(filter(obs.c.gl.i(), span=="Total")$Cases.obs), 
                           na.rm=T),
                       max(c(filter(obs.c.gl.i(), span=="Daily")$Cases.obs), 
                           na.rm=T)*1.07),
               lab=c(paste0(input$c.country, "\n", 
                            c("", "(per million)")[input$c.pK.gl+1]),
                     paste("7-day peak:\n", format(obs.max.c.gl.i()$Date, "%b %d"))))
    })
    obs.d.gl.i <- reactive({
        obs$obs.d.gl %>% ungroup %>%
            filter(Country==input$d.country & !is.na(Deaths.obs) &
                       Date >= input$d.dates.gl[1] & Date <= input$d.dates.gl[2]) %>%
            mutate(src="Average",
                   Deaths.obs=ifelse(rep(input$d.pK.gl, n()), 
                                     Deaths.obs/pop, Deaths.obs))
    })
    obs.max.d.gl.i <- reactive({
        obs$obs.d.gl.max %>% filter(Country==input$d.country) %>%
            filter(Date >= input$d.dates.gl[1] & Date <= input$d.dates.gl[2]) %>%
            mutate(obs=ifelse(input$d.pK.gl, obs/pop, obs))
    })
    d.gl.i <- reactive({
        obs$gl.df %>% 
            filter(Country==input$d.country & 
                       model_date %in% input$d.modDates.gl &
                       Date >= input$d.dates.gl[1] & Date <= input$d.dates.gl[2]) %>%
            group_by(model_date) %>% 
            filter(Date >= as.Date(model_date, format="%Y_%m_%d")) %>%
            mutate(Deaths.obs=ifelse(rep(input$d.pK.gl, n()), 
                                     Deaths.obs/pop, Deaths.obs),
                   Deaths=ifelse(rep(input$d.pK.gl, n()), Deaths/pop, Deaths))
    })
    d.mit.gl.i <- reactive({
        obs$mit.gl %>%
            filter(Country==input$d.country &
                       Date >= input$d.dates.gl[1] & Date <= input$d.dates.gl[2]) %>%
            mutate(Deaths=ifelse(rep(input$d.pK.gl, n()), Deaths/pop, Deaths),
                   modType="MIT")
    })
    d.gl.i.starts <- reactive({
        d.gl.i() %>% 
            filter(Date==as.Date(model_date, format="%Y_%m_%d") &
                       Date >= input$d.dates.gl[1] & Date <= input$d.dates.gl[2])
    })
    obs.lab.d.gl <- reactive({
        tibble(span=c("Total", "Daily"), lab.size=c(6,5),
               Date=c(input$d.dates.gl[1], obs.max.d.gl.i()$Date),
               Deaths=c(max(c(filter(d.gl.i(), span=="Total")$Deaths),
                            c(filter(obs.d.gl.i(), span=="Total")$Deaths.obs), 
                            na.rm=T),
                        max(c(filter(d.gl.i(), span=="Daily")$Deaths),
                            c(filter(obs.d.gl.i(), span=="Daily")$Deaths.obs), 
                            na.rm=T)*1.07),
               lab=c(paste0(input$d.country, "\n", 
                            c("", "(per million)")[input$d.pK.gl+1]),
                     paste("7-day peak:", format(obs.max.d.gl.i()$Date, "%b %d"))))
    })
    
    
    ###---- Reactives: States
    obs.comp.us <- reactive({
        bind_rows(
            obs$obs.c.us %>% ungroup %>%
                filter(State %in% input$comp.state & !is.na(Cases.obs) &
                           Date >= input$comp.dates.us[1] & 
                           Date <= input$comp.dates.us[2]) %>%
                mutate(Type="Cases",
                       obs=ifelse(rep(input$comp.pK.us, n()), 
                                  Cases.obs/pop, Cases.obs)) %>%
                select(-pop, -wDay, -Cases.obs),
            obs$obs.d.us %>% ungroup %>%
                filter(State %in% input$comp.state & !is.na(Deaths.obs) &
                           Date >= input$comp.dates.us[1] & 
                           Date <= input$comp.dates.us[2]) %>%
                mutate(Type="Deaths",
                       obs=ifelse(rep(input$comp.pK.us, n()), 
                                  Deaths.obs/pop, Deaths.obs)) %>%
                select(-pop, -wDay, -Deaths.obs)
        ) %>% arrange(desc(span), Type) %>%
            mutate(SpanType=factor(paste(span, Type), 
                                   levels=c(unique(paste(span, Type)))))
    })
    obs.lab.comp.us <- reactive({
        tibble(SpanType=factor(levels(obs.comp.us()$SpanType)[1], 
                               levels=levels(obs.comp.us()$SpanType)),
               Date=input$comp.dates.us[1], 
               obs=max(obs.comp.us()$obs),
               lab=ifelse(input$comp.pK.us, "per 10k", ""))
    })
    c.mit.us.i <- reactive({
        obs$mit.us %>%
            filter(State==input$c.state &
                       Date >= input$c.dates.us[1] & Date <= input$c.dates.us[2]) %>%
            mutate(Cases=ifelse(rep(input$c.pK.us, n()), Cases/pop, Cases),
                   modType="MIT")
    })
    obs.c.us.i <- reactive({
        obs$obs.c.us %>% ungroup %>%
            filter(State==input$c.state & !is.na(Cases.obs) &
                       Date >= input$c.dates.us[1] & Date <= input$c.dates.us[2]) %>%
            mutate(src="Average",
                   Cases.obs=ifelse(rep(input$c.pK.us, n()), 
                                    Cases.obs/pop, Cases.obs))
    })
    obs.max.c.us.i <- reactive({
        obs$obs.c.us.max %>% filter(State==input$c.state) %>%
            filter(Date >= input$c.dates.us[1] & Date <= input$c.dates.us[2]) %>%
            mutate(obs=ifelse(input$c.pK.us, obs/pop, obs))
    })
    obs.lab.c.us <- reactive({
        tibble(span=c("Total", "Daily"), lab.size=c(6,5),
               Date=c(input$c.dates.us[1], obs.max.c.us.i()$Date),
               Cases=c(max(c(filter(obs.c.us.i(), span=="Total")$Cases.obs), 
                           na.rm=T),
                       max(c(filter(obs.c.us.i(), span=="Daily")$Cases.obs), 
                           na.rm=T)*1.07),
               lab=c(paste0(input$c.state, "\n", 
                            c("", "(per 10k)")[input$c.pK.us+1]),
                     paste("7-day peak:\n", format(obs.max.c.us.i()$Date, "%b %d"))))
    })
    obs.d.us.i <- reactive({
        obs$obs.d.us %>% ungroup %>%
            filter(State==input$d.state & !is.na(Deaths.obs) &
                       Date >= input$d.dates.us[1] & Date <= input$d.dates.us[2]) %>%
            mutate(src="Average",
                   Deaths.obs=ifelse(rep(input$d.pK.us, n()), 
                                     Deaths.obs/pop, Deaths.obs))
    })
    obs.max.d.us.i <- reactive({
        obs$obs.d.us.max %>% filter(State==input$d.state) %>%
            filter(Date >= input$d.dates.us[1] & Date <= input$d.dates.us[2]) %>%
            mutate(obs=ifelse(input$d.pK.us, obs/pop, obs))
    })
    d.us.i <- reactive({
        obs$us.df %>%
            filter(State==input$d.state &
                       model_date %in% input$d.modDates.us &
                       Date >= input$d.dates.us[1] & Date <= input$d.dates.us[2]) %>%
            group_by(model_date) %>%
            filter(Date >= as.Date(model_date, format="%Y_%m_%d")) %>%
            mutate(Deaths.obs=ifelse(rep(input$d.pK.us, n()), 
                                     Deaths.obs/pop, Deaths.obs),
                   Deaths=ifelse(rep(input$d.pK.us, n()), Deaths/pop, Deaths))
    })
    d.mit.us.i <- reactive({
        obs$mit.us %>%
            filter(State==input$d.state &
                       Date >= input$d.dates.us[1] & Date <= input$d.dates.us[2]) %>%
            mutate(Deaths=ifelse(rep(input$d.pK.us, n()), Deaths/pop, Deaths),
                   modType="MIT")
    })
    d.us.i.starts <- reactive({
        d.us.i() %>%
            filter(Date==as.Date(model_date, format="%Y_%m_%d") &
                       Date >= input$d.dates.us[1] & Date <= input$d.dates.us[2]) 
    })
    obs.lab.d.us <- reactive({
        tibble(span=c("Total", "Daily"), lab.size=c(6, 5),
               Date=c(input$d.dates.us[1], obs.max.d.us.i()$Date),
               Deaths=c(max(c(filter(d.us.i(), span=="Total")$Deaths),
                            c(filter(obs.d.us.i(), span=="Total")$Deaths.obs),
                            na.rm=T),
                        max(c(filter(d.us.i(), span=="Daily")$Deaths),
                            c(filter(obs.d.us.i(), span=="Daily")$Deaths.obs),
                            na.rm=T)*1.07),
               lab=c(paste0(input$d.state, "\n", 
                            c("", "(per 10k)")[input$d.pK.us+1]),
                     paste("7-day peak:", format(obs.max.d.us.i()$Date, "%b %d"))))
    })
    
    
    ###---- Plot: Country cases
    output$country.c <- renderPlot({
        ggplot(obs.c.gl.i(), aes(Date)) +
            geom_hline(yintercept=0, colour="gray30", size=0.25) +
            {if("MIT" %in% input$c.modSource.gl) {
                geom_line(data=c.mit.gl.i(), aes(y=Cases, linetype=modType),
                          size=1, colour="red")
            }} +
            geom_line(data=obs.c.gl.i(), aes(y=Cases.obs, alpha=src), method="loess", 
                      stat="smooth", colour=1, size=1.5, span=0.6, formula=y~x) + 
            geom_vline(data=obs.max.c.gl.i(), aes(xintercept=Date), linetype=3) +
            geom_text(data=obs.lab.c.gl(), aes(y=Cases, label=lab), 
                      fontface=c("italic", "plain"), nudge_x=c(0,2),
                      size=obs.lab.c.gl()$lab.size, hjust=0, vjust=1, colour=1) +
            geom_point(aes(y=Cases.obs, fill=wDay), colour="black", 
                       size=2, shape=21) + 
            geom_rug(data=filter(obs.c.gl.i(), Date==last(Date) & span=="Total"), 
                     aes(y=Cases.obs), colour="black", sides="r") +
            scale_linetype_manual("MIT Model", labels="MIT", values=2, 
                                  guide=guide_legend(order=1)) +
            scale_fill_brewer("Observed", type="div", palette=1,
                              guide=guide_legend(order=2)) +
            scale_alpha_manual("", values=0.4,
                               guide=guide_legend(order=3,
                                                  title.position="bottom")) +
            xlim(as.Date(input$c.dates.gl[1]),
                 as.Date(input$c.dates.gl[2])) +
            scale_y_continuous(labels=pretty_numbers, position="right") + 
            labs(x="", y="") +
            facet_grid(factor(span, levels=c("Total", "Daily"))~., 
                       scales="free_y", switch="y") +
            theme(axis.text=element_text(size=14),
                  legend.text=element_text(size=14),
                  axis.title=element_text(size=16),
                  legend.title=element_text(size=16), 
                  title=element_text(size=18), 
                  strip.text=element_text(size=16))
    }, width=600, height=675)
    
    ###---- Plot: Countries vs. means
    output$country.d <- renderPlot({
        ggplot(d.gl.i(), aes(Date, Deaths, colour=model_date)) +
            geom_hline(yintercept=0, colour="gray30", size=0.25) +
            {if("MIT" %in% input$d.modSource.gl) {
                geom_line(data=d.mit.gl.i(), aes(y=Deaths, linetype=modType),
                          size=1, colour="red")
            }} +
            {if("IHME" %in% input$d.modSource.gl) {
                geom_text(data=d.gl.i.starts(), aes(group=model_date),
                          label="|", size=5, fontface="bold", family="mono")
            }} +
            {if("IHME" %in% input$d.modSource.gl) {
                geom_line(data=d.gl.i(), aes(group=model_date), size=1)
            }} +
            geom_line(data=obs.d.gl.i(), aes(y=Deaths.obs, alpha=src), 
                      method="loess", stat="smooth",
                      colour=1, size=1.5, span=0.6, formula=y~x) + 
            geom_vline(data=obs.max.d.gl.i(), aes(xintercept=Date), linetype=3) +
            geom_text(data=obs.lab.d.gl(), aes(label=lab), 
                      fontface=c("italic", "plain"), nudge_x=c(0,2),
                      size=obs.lab.d.gl()$lab.size, hjust=0, vjust=1, colour=1) +
            geom_rug(data=filter(obs.d.gl.i(), Date==last(Date) & span=="Total"), 
                     aes(y=Deaths.obs), colour="black", sides="r") +
            geom_point(data=obs.d.gl.i(), aes(y=Deaths.obs, fill=wDay), 
                       colour="black", size=2, shape=21) + 
            scale_linetype_manual("MIT Model", labels="MIT", values=2, 
                                  guide=guide_legend(order=1)) +
            scale_colour_viridis_d("IHME Model",
                                   labels=as.Date(unique(d.gl.i()$model_date), 
                                                  format="%Y_%m_%d") %>%
                                       format("%b %d"),
                                   guide=guide_legend(order=2)) +
            scale_fill_brewer("\n\n\nObserved", type="div", 
                              guide=guide_legend(order=3)) +
            scale_alpha_manual("", values=0.4,
                               guide=guide_legend(order=4,
                                                  title.position="bottom")) +
            xlim(as.Date(input$d.dates.gl[1]),
                 as.Date(input$d.dates.gl[2])) +
            scale_y_continuous(labels=pretty_numbers, position="right") + 
            labs(x="", y="") +
            facet_grid(factor(span, levels=c("Total", "Daily"))~., 
                       scales="free_y", switch="y") +
            theme(axis.text=element_text(size=14),
                  legend.text=element_text(size=14),
                  axis.title=element_text(size=16),
                  legend.title=element_text(size=16), 
                  title=element_text(size=18), 
                  strip.text=element_text(size=16))
    }, width=600, height=675)
    
    ###---- Plot: Country comparisons
    output$country.comp <- renderPlot({
        ggplot(obs.comp.gl(), 
               aes(Date, y=obs, colour=Country)) +
            geom_text(data=obs.lab.comp.gl(), aes(label=lab), colour=1,
                      hjust=0, size=5) +
            geom_hline(yintercept=0, colour="gray30", size=0.5) +
            geom_line(aes(group=Country), stat="smooth", method="loess", 
                      span=0.6, formula=y~x, size=1) +
            geom_text(data=filter(obs.comp.gl(), Date==last(Date)), 
                      aes(label=abbr), size=4, nudge_x=2, hjust=0, vjust=0.5) +
            geom_point(alpha=0.5, size=1) + 
            scale_colour_viridis_d("", end=0.8, option="plasma", direction=-1) +
            scale_y_continuous(labels=pretty_numbers, position="right",
                               limits=c(0,NA)) + 
            xlim(as.Date(input$comp.dates.gl[1]),
                 as.Date(input$comp.dates.gl[2])+4) +
            facet_wrap(~SpanType, scales="free_y") +
            labs(x="", y="") + 
            theme(axis.text=element_text(size=13),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=16), 
                  title=element_text(size=18), 
                  strip.text=element_text(size=16),
                  legend.box.margin=margin(-10,0,0,0),
                  legend.position="bottom",
                  legend.direction="horizontal")
    }, width=600, height=625)
    
    ###---- Plot: State cases
    output$state.c <- renderPlot({
        ggplot(obs.c.us.i(), aes(Date)) +
            geom_hline(yintercept=0, colour="gray30", size=0.25) +
            {if("MIT" %in% input$c.modSource.us) {
                geom_line(data=c.mit.us.i(), aes(y=Cases, linetype=modType),
                          size=1, colour="red")
            }} +
            geom_line(data=obs.c.us.i(), aes(y=Cases.obs, alpha=src), method="loess", 
                      stat="smooth", colour=1, size=1.5, span=0.6, formula=y~x) + 
            geom_vline(data=obs.max.c.us.i(), aes(xintercept=Date), linetype=3) +
            geom_text(data=obs.lab.c.us(), aes(y=Cases, label=lab), 
                      fontface=c("italic", "plain"), nudge_x=c(0,2),
                      size=obs.lab.c.us()$lab.size, hjust=0, vjust=1, colour=1) +
            geom_rug(data=filter(obs.c.us.i(), Date==last(Date) & span=="Total"), 
                     aes(y=Cases.obs), colour="black", sides="r") +
            geom_point(aes(y=Cases.obs, fill=wDay), colour="black", 
                       size=2, shape=21) + 
            scale_linetype_manual("MIT Model", labels="MIT", values=2, 
                                  guide=guide_legend(order=1)) +
            scale_fill_brewer("Observed", type="div", 
                              guide=guide_legend(order=2)) +
            scale_alpha_manual("", values=0.4,
                               guide=guide_legend(order=3,
                                                  title.position="bottom")) +
            xlim(as.Date(input$c.dates.us[1]),
                 as.Date(input$c.dates.us[2])) +
            scale_y_continuous(labels=pretty_numbers, position="right") + 
            labs(x="", y="") +
            facet_grid(factor(span, levels=c("Total", "Daily"))~., 
                       scales="free_y", switch="y") +
            theme(axis.text=element_text(size=14),
                  legend.text=element_text(size=14),
                  axis.title=element_text(size=16),
                  legend.title=element_text(size=16), 
                  title=element_text(size=18), 
                  strip.text=element_text(size=16))
        
    }, width=600, height=675)
    
    ###---- Plot: States vs. means
    output$state.d <- renderPlot({
        ggplot(d.us.i(), aes(Date, Deaths, colour=model_date)) +
            geom_hline(yintercept=0, colour="gray30", size=0.25) +
            {if("MIT" %in% input$d.modSource.us) {
                geom_line(data=d.mit.us.i(), aes(y=Deaths, linetype=modType),
                          size=1, colour="red")
            }} +
            {if("IHME" %in% input$d.modSource.us) {
                geom_text(data=d.us.i.starts(), aes(group=model_date),
                          label="|", size=5, fontface="bold", family="mono")
            }} +
            {if("IHME" %in% input$d.modSource.us) {
                geom_line(data=d.us.i(), aes(group=model_date), size=1)
            }} +
            geom_line(data=obs.d.us.i(), aes(y=Deaths.obs, alpha=src),
                      method="loess", stat="smooth",
                      colour=1, size=1.5, span=0.6, formula=y~x) +
            geom_vline(data=obs.max.d.us.i(), aes(xintercept=Date), linetype=3) +
            geom_text(data=obs.lab.d.us(), aes(label=lab), nudge_x=c(0,2),
                      fontface=c("italic", "plain"),
                      size=obs.lab.d.us()$lab.size, hjust=0, vjust=1, colour=1) +
            geom_rug(data=filter(obs.d.us.i(), Date==last(Date) & span=="Total"), 
                     aes(y=Deaths.obs), colour="black", sides="r") +
            geom_point(data=obs.d.us.i(), aes(y=Deaths.obs, fill=wDay),
                       colour="black", size=2, shape=21) +
            scale_linetype_manual("MIT Model", labels="MIT", values=2, 
                                  guide=guide_legend(order=1)) +
            scale_colour_viridis_d("IHME Model",
                                   labels=as.Date(unique(d.us.i()$model_date), 
                                                  format="%Y_%m_%d") %>%
                                       format("%b %d"),
                                   guide=guide_legend(order=2)) +
            scale_fill_brewer("\n\n\nObserved", type="div", 
                              guide=guide_legend(order=3)) +
            scale_alpha_manual(NULL, values=0.4,
                               guide=guide_legend(order=4,
                                                  title.position="bottom")) +
            xlim(as.Date(input$d.dates.us[1]),
                 as.Date(input$d.dates.us[2])) +
            scale_y_continuous(labels=pretty_numbers, position="right") +
            labs(x="", y="") +
            facet_grid(factor(span, levels=c("Total", "Daily"))~., 
                       scales="free_y", switch="y") +
            theme(axis.text=element_text(size=14),
                  legend.text=element_text(size=14),
                  axis.title=element_text(size=16),
                  legend.title=element_text(size=16),
                  title=element_text(size=18),
                  strip.text=element_text(size=16))
    }, width=600, height=675)
    
    ###---- Plot: State comparisons
    output$state.comp <- renderPlot({
        ggplot(obs.comp.us(), 
               aes(Date, y=obs, colour=State)) +
            geom_text(data=obs.lab.comp.us(), aes(label=lab), colour=1,
                      hjust=0, size=5) +
            geom_hline(yintercept=0, colour="gray30", size=0.5) +
            geom_line(aes(group=State), stat="smooth", method="loess", 
                      span=0.6, formula=y~x, size=1) +
            geom_text(data=filter(obs.comp.us(), Date==last(Date)), 
                      aes(label=abbr), size=4, nudge_x=2, hjust=0, vjust=0.5) +
            geom_point(alpha=0.5, size=1) + 
            scale_colour_viridis_d("", end=0.8, option="plasma", direction=-1) +
            scale_y_continuous(labels=pretty_numbers, position="right",
                               limits=c(0,NA)) + 
            xlim(as.Date(input$comp.dates.us[1]),
                 as.Date(input$comp.dates.us[2]+4)) +
            facet_wrap(~SpanType, scales="free_y") +
            labs(x="", y="") + 
            theme(axis.text=element_text(size=13),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=16), 
                  title=element_text(size=18), 
                  strip.text=element_text(size=16),
                  legend.box.margin=margin(-10,0,0,0),
                  legend.position="bottom",
                  legend.direction="horizontal")
    }, width=600, height=625)
}

# Run the application 
shinyApp(ui=ui, server=server)
