

library(shiny); library(tidyverse); 
theme_set(theme_bw() + theme(panel.grid=element_blank()))
source("fn.R")
obs <- load_obs()
refresh_peak_plots(obs)
latest.mod.Ymd <- max(obs$gl.df$model_date)
latest.mod.bd <- format(as.Date(latest.mod.Ymd, format="%Y_%m_%d"), "%b %d")



# Define UI for application
ui <- navbarPage("COVID-19 Data Trends", theme=shinythemes::shinytheme("yeti"),
                 
        navbarMenu("US States",
            tabPanel("Overview",
                tags$h4("Overview of", tags$b("US States")),
                sidebarLayout(
                    sidebarPanel(radioButtons(inputId="over.type",
                                              label="Choose metric",
                                              choices=c("Cases", "Deaths"),
                                              selected="Cases"),
                                 radioButtons(inputId="over.span",
                                              label="Choose span",
                                              choices=c("Daily", "Total"),
                                              selected="Daily"),
                                 dateRangeInput(inputId="over.dates",
                                                label="Choose dates to display",
                                                start="2020-03-01", end=Sys.Date(),
                                                min="2020-01-03", max=Sys.Date()),
                                 checkboxInput(inputId="over.pK",
                                               label="Display per 10k people",
                                               value=TRUE),
                                 checkboxInput(inputId="over.free",
                                               label="Free y-axis?",
                                               value=TRUE),
                                 tags$hr(),
                                 "The", tags$b("points"), "show reported cases or deaths, with the", tags$b("point color"), "indicating the day of the week (darkest = weekends), and the", tags$b("gray line"), "as the moving average. The", tags$b("vertical dotted lines"), "show the end of the worst 7-day period.",
                                 width=3),
                    mainPanel(plotOutput(outputId="state.overview"),
                              width=9)
                )
            ),
            tabPanel("Select",
                tags$h4("Select a", tags$b("US State")),
                sidebarLayout(
                    sidebarPanel(
                        selectInput(inputId="f.state",
                                    label="Choose a state",
                                    choices=sort(unique(obs$us.df$State)),
                                    selected="Colorado"),
                        checkboxGroupInput(inputId="f.modSource.us",
                                           label="Select models to show",
                                           choices=c("IHME", "MIT"),
                                           selected=c("IHME", "MIT"),
                                           inline=TRUE),
                        selectInput(inputId="f.modDates.us",
                                    label="Choose IHME model releases to show",
                                    choices=setNames(unique(obs$us.df$model_date),
                                                     as.Date(unique(obs$us.df$model_date),
                                                             format="%Y_%m_%d") %>%
                                                         format("%b %d")),
                                    selected=c("2020_03_25", "2020_04_16", 
                                               latest.mod.Ymd),
                                    multiple=TRUE),
                        dateRangeInput(inputId="f.dates.us",
                                       label="Choose dates to display",
                                       start="2020-03-01", end="2020-07-01",
                                       min="2020-01-03", max="2020-08-04"),
                        checkboxInput(inputId="f.pK.us",
                                      label="Display per 10,000 people",
                                      value=FALSE),
                        tags$hr(),
                        "The", tags$b("points"), "show reported cases or deaths, with the", tags$b("point color"), "indicating the day of the week (darkest = weekends), and the", tags$b("gray line"), "as the moving average. The", tags$b("model lines"), "show only the", tags$em("mean predictions,"), "starting from the date the model was released (i.e., the 'Apr 01' model starts on April 01). Currently, only the latest MIT model is available to compare. The", tags$b("vertical dotted lines"), "show the end of the worst 7-day period.",
                        tags$hr(),
                        "Mar 25: original release", tags$br(),
                        "Apr 16: most optimistic for US", tags$br(),
                        "May 10: most pessimistic for US", tags$br(),
                        paste0(latest.mod.bd, ":"), "most recent"
                    ),
                    mainPanel(plotOutput(outputId="state.focus"))
                )
            ),       
            tabPanel("Compare",
                     tags$h4("Compare multiple", tags$b("US States")),
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
                             "The", tags$b("points"), "show reported cases or deaths with the", tags$b("lines"), "as the moving averages."
                         ),
                         mainPanel(plotOutput(outputId="state.comp", width="100%"))
                     )
            )
        ),
        navbarMenu("Countries",
            tabPanel("Select",
                     tags$h4("Select a", tags$b("country")),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(inputId="f.country",
                                         label="Choose a country",
                                         choices=sort(unique(obs$obs.c.gl$Country)),
                                         selected="US"),
                             checkboxGroupInput(inputId="f.modSource.gl",
                                                label="Select models to show",
                                                choices=c("IHME", "MIT"),
                                                selected=c("IHME", "MIT"),
                                                inline=TRUE),
                             selectInput(inputId="f.modDates.gl",
                                         label="Choose IHME model releases to show",
                                         choices=setNames(unique(obs$gl.df$model_date),
                                                          as.Date(unique(obs$gl.df$model_date),
                                                                  format="%Y_%m_%d") %>%
                                                              format("%b %d")),
                                         selected=c("2020_03_25", "2020_04_16", 
                                                    latest.mod.Ymd),
                                         multiple=TRUE),
                             dateRangeInput(inputId="f.dates.gl",
                                            label="Choose dates to display",
                                            start="2020-03-01", end="2020-07-01",
                                            min="2020-01-03", max="2020-08-04"),
                             checkboxInput(inputId="f.pK.gl",
                                           label="Display per million people",
                                           value=FALSE),
                             tags$hr(),
                             "The", tags$b("points"), "show reported cases or deaths, with the", tags$b("point color"), "indicating the day of the week (darkest = weekends), and the", tags$b("gray line"), "as the moving average. The", tags$b("model lines"), "show only the", tags$em("mean predictions,"), "starting from the date the model was released (i.e., the 'Apr 01' model starts on April 01). Currently, only the latest MIT model is available to compare. The", tags$b("vertical dotted lines"), "show the end of the worst 7-day period.",
                             tags$hr(),
                             "Mar 25: original release", tags$br(),
                             "Apr 16: most optimistic for US", tags$br(),
                             "May 10: most pessimistic for US", tags$br(),
                             paste0(latest.mod.bd, ":"), "most recent"
                         ),
                         mainPanel(plotOutput(outputId="country.focus"))
                     )
            ),
            tabPanel("Compare",
                     tags$h4("Compare multiple", tags$b("countries")),
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
        navbarMenu(title="Peaks",
            tabPanel("Deaths",
                     tags$h4("Which weeks have had the", tags$b("most deaths"), "so far?"),
                     fluidRow(
                         column(6, tags$img(src="country_d_peak.png", width='95%')),
                         column(6, tags$img(src="states_d_peak.png", width='95%'))
                     )
            ),
            tabPanel("Cases",
                     tags$h4("Which weeks have had the", tags$b("most new cases"), "so far?"),
                     fluidRow(
                         column(6, tags$img(src="country_c_peak.png", width='95%')),
                         column(6, tags$img(src="states_c_peak.png", width='95%'))
                     )
            ),
            tabPanel("Compare",
                     tags$h4("How much time is between the peaks in", tags$b("new cases"), "and", tags$b("deaths"), "?"),
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
    
    ###---- Reactives: Countries -----------------------------------------------
    
    # Focus 
    obs.f.gl <- reactive({
        bind_rows(
            obs$obs.c.gl %>% ungroup %>%
                filter(Country == input$f.country & !is.na(Cases.obs) &
                           Date >= input$f.dates.gl[1] & 
                           Date <= input$f.dates.gl[2]) %>%
                mutate(Type="Cases",
                       obs=ifelse(rep(input$f.pK.gl, n()), 
                                  Cases.obs/pop, Cases.obs)) %>%
                select(-pop, -Cases.obs),
            obs$obs.d.gl %>% ungroup %>%
                filter(Country %in% input$f.country & !is.na(Deaths.obs) &
                           Date >= input$f.dates.gl[1] & 
                           Date <= input$f.dates.gl[2]) %>%
                mutate(Type="Deaths",
                       obs=ifelse(rep(input$f.pK.gl, n()), 
                                  Deaths.obs/pop, Deaths.obs)) %>%
                select(-pop, -Deaths.obs)
        ) %>% arrange(desc(span), Type) %>%
            mutate(src="Average",
                   SpanType=factor(paste(span, Type), 
                                   levels=c(unique(paste(span, Type)))))
    })
    
    
    obs.max.f.gl <- reactive({
        bind_rows(
            obs$obs.c.gl.max %>% filter(Country==input$f.country &
                                            Date >= input$f.dates.gl[1] & 
                                            Date <= input$f.dates.gl[2]) %>%
                mutate(Type="Cases", 
                       obs=ifelse(input$f.pK.gl, obs/pop, obs)),
            obs$obs.d.gl.max %>% filter(Country==input$f.country &
                                            Date >= input$f.dates.gl[1] & 
                                            Date <= input$f.dates.gl[2]) %>%
                mutate(Type="Deaths", 
                       obs=ifelse(input$f.pK.gl, obs/pop, obs))
        ) %>% mutate(span="Daily",
                     SpanType=factor(paste(span, Type), 
                                     levels=c(unique(paste(span, Type)))))
    })
    
    mit.f.gl <- reactive({
        bind_rows(
            obs$mit.gl %>% filter(Country==input$f.country &
                                      Date >= input$f.dates.gl[1] & 
                                      Date <= input$f.dates.gl[2]) %>%
                mutate(Type="Cases", 
                       pred=ifelse(rep(input$f.pK.gl, n()), Cases/pop, Cases)),
            obs$mit.gl %>% filter(Country==input$f.country &
                                      Date >= input$f.dates.gl[1] & 
                                      Date <= input$f.dates.gl[2]) %>%
                mutate(Type="Deaths",
                       pred=ifelse(rep(input$f.pK.gl, n()), Deaths/pop, Deaths))
        ) %>% mutate(modType="MIT",
                     SpanType=factor(paste(span, Type), 
                                     levels=c(unique(paste(span, Type)))))
    })
    
    ihme.f.gl <- reactive({
        obs$gl.df %>% filter(Country==input$f.country &
                                 model_date %in% input$f.modDates.gl &
                                 Date >= input$f.dates.gl[1] & 
                                 Date <= input$f.dates.gl[2]) %>%
            group_by(model_date) %>%
            filter(Date >= as.Date(model_date, format="%Y_%m_%d")) %>%
            mutate(Type="Deaths",
                   pred=ifelse(rep(input$f.pK.gl, n()), Deaths/pop, Deaths),
                   modType="IHME",
                   SpanType=factor(paste(span, Type), 
                                   levels=c(unique(paste(span, Type)))))
    })
    
    ihme.starts.f.gl <- reactive({
        ihme.f.gl() %>% filter(Date==as.Date(model_date, format="%Y_%m_%d") &
                                   Date >= input$f.dates.gl[1] & 
                                   Date <= input$f.dates.gl[2])
    })
    
    obs.lab.f.gl <- reactive({
        tibble(SpanType=factor(c("Total Cases", "Daily Cases", "Daily Deaths"),
                               levels=levels(obs.f.gl()$SpanType)),
               Date=c(input$f.dates.gl[1], obs.max.f.gl()$Date),
               obs=c(max(c(mit.f.gl()$pred[mit.f.gl()$SpanType=="Total Cases"], 
                           obs.f.gl()$obs[obs.f.gl()$SpanType=="Total Cases"]), 
                         na.rm=T),
                     max(c(mit.f.gl()$pred[mit.f.gl()$SpanType=="Daily Cases"], 
                           obs.f.gl()$obs[obs.f.gl()$SpanType=="Daily Cases"]), 
                         na.rm=T)*1.15,
                     max(c(mit.f.gl()$pred[mit.f.gl()$SpanType=="Daily Deaths"], 
                           ihme.f.gl()$pred[ihme.f.gl()$SpanType=="Daily Deaths"], 
                           obs.f.gl()$obs[obs.f.gl()$SpanType=="Daily Deaths"]), 
                         na.rm=T)*1.15),
               lab=c(paste0(input$f.country, "\n", 
                            c("", "(per million)")[input$f.pK.gl+1]),
                     paste0("7-day peak:\n", 
                            format(obs.max.f.gl()$Date[1],  "%b %d")),
                     paste0("7-day peak:\n", 
                            format(obs.max.f.gl()$Date[2],  "%b %d"))),
               lab.size=c(5,4,4))
    })
    
    
    # Comparison
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
    
    
    
    
    ###---- Reactives: US States -----------------------------------------------
    obs.us.all <- reactive({
        bind_rows(
            obs$obs.c.us %>% ungroup %>%
                mutate(Type="Cases",
                       obs=Cases.obs) %>%
                select(-Cases.obs),
            obs$obs.d.us %>% ungroup %>%
                mutate(Type="Deaths",
                       obs=Deaths.obs) %>%
                select(-Deaths.obs)
        ) %>% arrange(desc(span), Type) %>%
            mutate(src="Average",
                   SpanType=factor(paste(span, Type), 
                                   levels=c(unique(paste(span, Type))))) %>%
            filter(!State %in% c("Grand Princess", "Diamond Princess"))
    })
    
    obs.max.us <- reactive({
        bind_rows(
            obs$obs.c.us.max %>% mutate(Type="Cases"),
            obs$obs.d.us.max %>% mutate(Type="Deaths")
        ) %>% mutate(span="Daily",
                     SpanType=factor(paste(span, Type), 
                                     levels=c(unique(paste(span, Type)))))
    })
    
    
    # Focus 
    obs.f.us <- reactive({
        obs.us.all() %>% 
            filter(State == input$f.state & !is.na(obs) &
                       Date >= input$f.dates.us[1] & 
                       Date <= input$f.dates.us[2]) %>%
            mutate(obs=ifelse(rep(input$f.pK.us, n()), obs/pop, obs))
    })
    
    obs.max.f.us <- reactive({
        bind_rows(
            obs$obs.c.us.max %>% filter(State==input$f.state &
                                            Date >= input$f.dates.us[1] & 
                                            Date <= input$f.dates.us[2]) %>%
                mutate(Type="Cases", 
                       obs=ifelse(input$f.pK.us, obs/pop, obs)),
            obs$obs.d.us.max %>% filter(State==input$f.state &
                                            Date >= input$f.dates.us[1] & 
                                            Date <= input$f.dates.us[2]) %>%
                mutate(Type="Deaths", 
                       obs=ifelse(input$f.pK.us, obs/pop, obs))
        ) %>% mutate(span="Daily",
                     SpanType=factor(paste(span, Type), 
                                     levels=c(unique(paste(span, Type)))))
    })
    
    mit.f.us <- reactive({
        bind_rows(
            obs$mit.us %>% filter(State==input$f.state &
                                      Date >= input$f.dates.us[1] & 
                                      Date <= input$f.dates.us[2]) %>%
                mutate(Type="Cases", 
                       pred=ifelse(rep(input$f.pK.us, n()), Cases/pop, Cases)),
            obs$mit.us %>% filter(State==input$f.state &
                                      Date >= input$f.dates.us[1] & 
                                      Date <= input$f.dates.us[2]) %>%
                mutate(Type="Deaths",
                       pred=ifelse(rep(input$f.pK.us, n()), Deaths/pop, Deaths))
        ) %>% mutate(modType="MIT",
                     SpanType=factor(paste(span, Type), 
                                     levels=c(unique(paste(span, Type)))))
    })
    
    ihme.f.us <- reactive({
        obs$us.df %>% filter(State==input$f.state &
                                 model_date %in% input$f.modDates.us &
                                 Date >= input$f.dates.us[1] & 
                                 Date <= input$f.dates.us[2]) %>%
            group_by(model_date) %>%
            filter(Date >= as.Date(model_date, format="%Y_%m_%d")) %>%
            mutate(Type="Deaths",
                   pred=ifelse(rep(input$f.pK.us, n()), Deaths/pop, Deaths),
                   modType="IHME",
                   SpanType=factor(paste(span, Type), 
                                   levels=c(unique(paste(span, Type)))))
    })
    
    ihme.starts.f.us <- reactive({
        ihme.f.us() %>% filter(Date==as.Date(model_date, format="%Y_%m_%d") &
                                   Date >= input$f.dates.us[1] & 
                                   Date <= input$f.dates.us[2])
    })
    
    obs.lab.f.us <- reactive({
        tibble(SpanType=factor(c("Total Cases", "Daily Cases", "Daily Deaths"),
                               levels=levels(obs.f.us()$SpanType)),
               Date=c(input$f.dates.us[1], obs.max.f.us()$Date),
               obs=c(max(c(mit.f.us()$pred[mit.f.us()$SpanType=="Total Cases"], 
                           obs.f.us()$obs[obs.f.us()$SpanType=="Total Cases"]), 
                         na.rm=T),
                     max(c(mit.f.us()$pred[mit.f.us()$SpanType=="Daily Cases"], 
                           obs.f.us()$obs[obs.f.us()$SpanType=="Daily Cases"]), 
                         na.rm=T)*1.15,
                     max(c(mit.f.us()$pred[mit.f.us()$SpanType=="Daily Deaths"], 
                           ihme.f.us()$pred[ihme.f.us()$SpanType=="Daily Deaths"], 
                           obs.f.us()$obs[obs.f.us()$SpanType=="Daily Deaths"]), 
                         na.rm=T)*1.15),
               lab=c(paste0(input$f.state, "\n", 
                            c("", "(per 10k)")[input$f.pK.us+1]),
                     paste0("7-day peak:\n", 
                            format(obs.max.f.us()$Date[1],  "%b %d")),
                     paste0("7-day peak:\n", 
                            format(obs.max.f.us()$Date[2],  "%b %d"))),
               lab.size=c(5,4,4))
    })
    
    
    
    # Comparison
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
    
    
    
    
    
    
    ###---- Plots: US States ---------------------------------------------------
    output$state.overview <- renderPlot({
        obs.us.all() %>% filter(Type==input$over.type & 
                                    span==input$over.span &
                                    Date >= input$over.dates[1] & 
                                    Date <= input$over.dates[2] &
                                    State %in% obs.max.us()$State) %>%
            mutate(obs=ifelse(rep(input$over.pK, n()), obs/pop, obs)) %>%
            ggplot(aes(x=Date, y=obs)) +
            geom_hline(yintercept=0, colour="gray30", size=0.25) +
            geom_point(aes(fill=wDay), alpha=0.8, size=1, shape=21) +
            {if(input$over.span=="Daily") {
                # geom_ma(n=7, colour=1, size=1, linetype=1, alpha=0.8, ma_fun=EMA)
                geom_line(stat="smooth", method="loess", 
                          span=0.6, formula=y~x, size=1) 
            }} +
            geom_vline(data=filter(obs.max.us(), Type==input$over.type),
                       aes(xintercept=Date), linetype=3) +
            scale_fill_brewer("Observed", type="div",
                              guide=guide_legend(title.position="top", 
                                                 title.hjust=0, 
                                                 label.hjust=0,
                                                 nrow=1,
                                                 direction="horizontal",
                                                 override.aes=list(size=2))) +
            scale_y_continuous(labels=pretty_numbers, position="right",
                               limits=c(0,NA)) + 
            scale_x_date(date_labels="%b", date_breaks="1 month") +
            facet_wrap(~State, scales=ifelse(input$over.free, "free_y", "fixed")) +
            labs(x="", y="") + 
            theme(axis.text=element_text(size=7.5, angle=330, hjust=0, vjust=0),
                  strip.text=element_text(size=9),
                  legend.position=c(0.75, 0.065))
    }, width=850, height=750)
    
    output$state.focus <- renderPlot({
        ggplot(obs.f.us(), aes(Date, obs)) +
            geom_hline(yintercept=0, colour="gray30", size=0.25) +
            {if("MIT" %in% input$f.modSource.us) {
                geom_line(data=mit.f.us(), aes(y=pred, linetype=modType),
                          size=1, colour="red")
            }} +
            {if("IHME" %in% input$f.modSource.us) {
                geom_text(data=ihme.starts.f.us(), 
                          aes(y=pred, group=model_date, colour=model_date),
                          label="|", size=5, fontface="bold", family="mono")
            }} +
            {if("IHME" %in% input$f.modSource.us) {
                geom_line(data=ihme.f.us(), size=1,
                          aes(y=pred, group=model_date, colour=model_date))
            }} +
            geom_line(stat="smooth", method="loess", 
                      span=0.6, formula=y~x, size=1) +
            # geom_ma(data=filter(obs.f.us(), span=="Daily"),
            #         aes(alpha=src), n=7, colour=1, size=1.5, linetype=1) + 
            geom_vline(data=obs.max.f.us(), aes(xintercept=Date), linetype=3) +
            geom_text(data=obs.lab.f.us(), aes(label=lab), 
                      fontface=c("italic", "plain", "plain"), nudge_x=c(0,2,2),
                      size=obs.lab.f.us()$lab.size, hjust=0, vjust=1, colour=1) +
            geom_rug(data=filter(obs.f.us(), Date==last(Date) & span=="Total"), 
                     colour="black", sides="r") +
            geom_point(data=obs.f.us(), aes(fill=wDay), 
                       colour="black", size=2, shape=21) + 
            scale_linetype_manual("MIT Model", labels="MIT", values=2,
                                  guide=guide_legend(order=1,
                                                     title.position="top")) +
            scale_colour_viridis_d("IHME Model",
                                   labels=as.Date(unique(ihme.f.us()$model_date),
                                                  format="%Y_%m_%d") %>%
                                       format("%b %d"),
                                   guide=guide_legend(order=2,
                                                      title.position="top")) +
            scale_fill_brewer("Observed", type="div",
                              guide=guide_legend(order=3,
                                                 title.position="top",
                                                 keyheight=grid::unit(2, "mm"))) +
            scale_alpha_manual("", values=0.4,
                               guide=guide_legend(order=4,
                                                  title.position="top")) +
            xlim(as.Date(input$f.dates.us[1]),
                 as.Date(input$f.dates.us[2])) +
            scale_y_continuous(labels=pretty_numbers, position="right") + 
            labs(x="", y="") +
            facet_wrap(~SpanType, scales="free_y") +
            theme(axis.text=element_text(size=14),
                  legend.text=element_text(size=12),
                  axis.title=element_text(size=16),
                  legend.title=element_text(size=14), 
                  title=element_text(size=18), 
                  strip.text=element_text(size=16),
                  legend.box.margin=margin(-10,0,0,0),
                  legend.position="bottom",
                  legend.direction="vertical")
    }, width=700, height=725)
    
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
    }, width=700, height=725)
    
    
    ###---- Plots: Countries ---------------------------------------------------
    output$country.focus <- renderPlot({
        ggplot(obs.f.gl(), aes(Date, obs)) +
            geom_hline(yintercept=0, colour="gray30", size=0.25) +
            {if("MIT" %in% input$f.modSource.gl) {
                geom_line(data=mit.f.gl(), aes(y=pred, linetype=modType),
                          size=1, colour="red")
            }} +
            {if("IHME" %in% input$f.modSource.gl) {
                geom_text(data=ihme.starts.f.gl(), 
                          aes(y=pred, group=model_date, colour=model_date),
                          label="|", size=5, fontface="bold", family="mono")
            }} +
            {if("IHME" %in% input$f.modSource.gl) {
                geom_line(data=ihme.f.gl(), size=1,
                          aes(y=pred, group=model_date, colour=model_date))
            }} +
            geom_line(stat="smooth", method="loess", 
                      span=0.6, formula=y~x, size=1) +
            # geom_ma(data=filter(obs.f.gl(), span=="Daily"),
            #         aes(alpha=src), n=7, colour=1, size=1.5, linetype=1) + 
            geom_vline(data=obs.max.f.gl(), aes(xintercept=Date), linetype=3) +
            geom_text(data=obs.lab.f.gl(), aes(label=lab), 
                      fontface=c("italic", "plain", "plain"), nudge_x=c(0,2,2),
                      size=obs.lab.f.gl()$lab.size, hjust=0, vjust=1, colour=1) +
            geom_rug(data=filter(obs.f.gl(), Date==last(Date) & span=="Total"), 
                     colour="black", sides="r") +
            geom_point(data=obs.f.gl(), aes(fill=wDay), 
                       colour="black", size=2, shape=21) + 
            scale_linetype_manual("MIT Model", labels="MIT", values=2,
                                  guide=guide_legend(order=1,
                                                     title.position="top")) +
            scale_colour_viridis_d("IHME Model",
                                   labels=as.Date(unique(ihme.f.gl()$model_date),
                                                  format="%Y_%m_%d") %>%
                                       format("%b %d"),
                                   guide=guide_legend(order=2,
                                                      title.position="top")) +
            scale_fill_brewer("Observed", type="div",
                              guide=guide_legend(order=3,
                                                 title.position="top",
                                                 keyheight=grid::unit(2, "mm"))) +
            scale_alpha_manual("", values=0.4,
                               guide=guide_legend(order=4,
                                                  title.position="top")) +
            xlim(as.Date(input$f.dates.gl[1]),
                 as.Date(input$f.dates.gl[2])) +
            scale_y_continuous(labels=pretty_numbers, position="right") + 
            labs(x="", y="") +
            facet_wrap(~SpanType, scales="free_y") +
            theme(axis.text=element_text(size=14),
                  legend.text=element_text(size=12),
                  axis.title=element_text(size=16),
                  legend.title=element_text(size=14), 
                  title=element_text(size=18), 
                  strip.text=element_text(size=16),
                  legend.box.margin=margin(-10,0,0,0),
                  legend.position="bottom",
                  legend.direction="vertical")
    }, width=700, height=725)
    
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
    }, width=700, height=725)
}

# Run the application 
shinyApp(ui=ui, server=server)
