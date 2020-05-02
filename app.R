

library(shiny); library(tidyverse)
theme_set(theme_bw() + theme(panel.grid=element_blank()))
source("fn.R")
obs <- load_obs()
refresh_peak_plots(obs)
latest.mod.Ymd <- max(obs$gl.df$model_date)
latest.mod.bd <- format(as.Date(latest.mod.Ymd, format="%Y_%m_%d"), "%b %d")



# Define UI for application
ui <- navbarPage("COVID-19 Models", theme=shinythemes::shinytheme("yeti"),
    
    ###---- Tab: Countries vs. means
    tabPanel("Countries", 
    
        # Some text
        tags$h3("How well have the IHME models predicted mortality for countries?"),
        # tags$hr(),
        sidebarLayout(
            sidebarPanel(
                selectInput(inputId="country", 
                            label="Choose a country",
                            choices=sort(unique(obs$gl.df$Country)),
                            selected="US"),
                selectInput(inputId="modDates.gl", 
                            label="Choose model releases to show",
                            choices=setNames(unique(obs$gl.df$model_date),
                                             as.Date(unique(obs$gl.df$model_date),
                                                     format="%Y_%m_%d") %>%
                                                 format("%b %d")),
                            selected=c("2020_03_25", "2020_04_01",
                                       latest.mod.Ymd),
                            multiple=TRUE),
                dateRangeInput(inputId="dates.gl", 
                               label="Choose dates to display",
                               start="2020-03-01", end="2020-06-01",
                               min="2020-01-03", max="2020-08-04"),
                tags$hr(),
                "The", tags$b("points"), "show reported deaths, with the", tags$b("point color"), "indicating the day of the week (lightest = Sunday), and the", tags$b("gray line"), "as the smoothed average. The", tags$b("model lines"), "show only the mean predictions, starting from the date the model was released (i.e., the 'Apr 01' model starts on April 01). The vertical", tags$b("dotted line"), "shows the end of the deadliest 7-day period.",
                tags$hr(),
                "'Mar 25' was the original model release. The 'Apr 01' update was the most pessimistic overall for the United States. The", paste0("'", latest.mod.bd, "'"), "update is the most recent."
            ),
            mainPanel(plotOutput(outputId="country", width="100%")),
        )
    ),
    
    ###---- Tab: States vs. means
    tabPanel("US States",
             tags$h3("How well have the IHME models predicted mortality for US states?"),
             # tags$hr(),
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId="state", 
                                 label="Choose a state",
                                 choices=sort(unique(obs$us.df$State)),
                                 selected="Colorado"),
                     selectInput(inputId="modDates.us", 
                                 label="Choose model releases to show",
                                 choices=setNames(unique(obs$us.df$model_date),
                                                  as.Date(unique(obs$us.df$model_date),
                                                          format="%Y_%m_%d") %>%
                                                      format("%b %d")),
                                 selected=c("2020_03_25", "2020_04_01",
                                            latest.mod.Ymd),
                                 multiple=TRUE),
                     dateRangeInput(inputId="dates.us", 
                                    label="Choose dates to display",
                                    start="2020-03-01", end="2020-06-01",
                                    min="2020-01-03", max="2020-08-04"),
                     tags$hr(),
                     "The", tags$b("points"), "show reported deaths, with the", tags$b("point color"), "indicating the day of the week (lightest = Sunday), and the", tags$b("gray line"), "as the smoothed average. The", tags$b("model lines"), "show only the mean predictions, starting from the date the model was released (i.e., the 'Apr 01' model starts on April 01). The vertical", tags$b("dotted line"), "shows the end of the deadliest 7-day period.",
                     tags$hr(),
                     "'Mar 25' was the original model release. The 'Apr 01' update was the most pessimistic overall for the United States. The", paste0("'", latest.mod.bd, "'"), "update is the most recent."
                 ),
                 mainPanel(plotOutput(outputId="state", width="100%")),
             )
    ),
    
    ###---- Tab: Peaks
    tabPanel("Peaks",
             tags$h3("Which weeks have been the worst so far?"),
             fluidRow(
                 column(6, tags$img(src="country_peak.png",
                                    width=500, height=900)),
                 column(6, tags$img(src="states_peak.png",
                                    width=500, height=900))
             )
    ),
    
    ###---- Tab: About
    tabPanel("About",
             "How successful have the models been at predicting the COVID-19 pandemic? The models produced by the", tags$a("Institute for Health Metrics and Evaluation", href="http://www.healthdata.org/covid/data-downloads"), "(IHME) have been used for planning purposes by many US states. This site let's you compare the mortality predicted by their models with the mortality we've actually seen so far. The observed data come from", tags$a("Johns Hopkins.", href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data"), "IHME releases updated models every few days, and Johns Hopkins releases new data each day.",
             tags$hr(),
             "Code is available on", 
             tags$a("GitHub.", href="https://github.com/Sz-Tim/COVID19-IHME"),
             "Created by ", 
             tags$a("Tim Szewczyk", href="https://sz-tim.github.io/about/")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ###---- Reactives: Countries
    obs.gl.i <- reactive({
        obs$obs.gl %>% ungroup %>%
            filter(Country==input$country & !is.na(Deaths.obs) &
                       Date >= input$dates.gl[1] & Date <= input$dates.gl[2]) %>%
            mutate(src="Average")
    })
    obs.max.gl.i <- reactive({
        obs$obs.gl.max %>% filter(Country==input$country) %>%
            filter(Date >= input$dates.gl[1] & Date <= input$dates.gl[2])
    })
    gl.i <- reactive({
        obs$gl.df %>% 
            filter(Country==input$country & 
                       model_date %in% input$modDates.gl &
                       Date >= input$dates.gl[1] & Date <= input$dates.gl[2]) %>%
            group_by(model_date) %>% 
            filter(Date >= as.Date(model_date, format="%Y_%m_%d"))
    })
    gl.i.starts <- reactive({gl.i() %>% 
            filter(Date==as.Date(model_date, format="%Y_%m_%d") &
                       Date >= input$dates.gl[1] & Date <= input$dates.gl[2])
    })
    obs.lab.gl <- reactive({
        tibble(span="Daily", Date=obs.max.gl.i()$Date,
               Deaths=max(c(filter(gl.i(), span=="Daily")$Deaths),
                          c(filter(obs.gl.i(), span=="Daily")$Deaths.obs), 
                          na.rm=T)*1.07,
               lab=paste(" 7-day peak:", format(obs.max.gl.i()$Date, "%b %d")))
    })
    gl.cols <- reactive({
        mod.seq.gl <- seq(0,1,length.out=n_distinct(gl.i()$model_date))
        scales::seq_gradient_pal("#543005", "#fec44f")(mod.seq.gl)
    })
    
    ###---- Reactives: States
    obs.us.i <- reactive({
        obs$obs.us %>% ungroup %>%
            filter(State==input$state & !is.na(Deaths.obs) &
                       Date >= input$dates.us[1] & Date <= input$dates.us[2]) %>%
            mutate(src="Average")
    })
    obs.max.us.i <- reactive({
        obs$obs.us.max %>% filter(State==input$state) %>%
            filter(Date >= input$dates.us[1] & Date <= input$dates.us[2])
    })
    us.i <- reactive({
        obs$us.df %>%
            filter(State==input$state &
                       model_date %in% input$modDates.us &
                       Date >= input$dates.us[1] & Date <= input$dates.us[2]) %>%
            group_by(model_date) %>%
            filter(Date >= as.Date(model_date, format="%Y_%m_%d"))
    })
    us.i.starts <- reactive({us.i() %>%
            filter(Date==as.Date(model_date, format="%Y_%m_%d") &
                       Date >= input$dates.us[1] & Date <= input$dates.us[2])
    })
    obs.lab.us <- reactive({
        tibble(span="Daily", Date=obs.max.us.i()$Date,
               Deaths=max(c(filter(us.i(), span=="Daily")$Deaths),
                          c(filter(obs.us.i(), span=="Daily")$Deaths.obs),
                          na.rm=T)*1.07,
               lab=paste(" 7-day peak:", format(obs.max.us.i()$Date, "%b %d")))
    })
    us.cols <- reactive({
        mod.seq.us <- seq(0,1,length.out=n_distinct(us.i()$model_date))
        scales::seq_gradient_pal("#543005", "#fec44f")(mod.seq.us)
    })
    
    ###---- Plot: Countries vs. means
    output$country <- renderPlot({
        ggplot(gl.i(), aes(Date, Deaths, colour=model_date)) +
            geom_hline(yintercept=0, colour="gray30", size=0.25) +
            geom_line(aes(group=model_date), size=1) + 
            geom_point(data=obs.gl.i(), aes(y=Deaths.obs, fill=wDay), 
                       colour="black", size=2, shape=21) + 
            geom_text(data=gl.i.starts(), aes(group=model_date),
                       label="|", size=5, fontface="bold", family="mono") +
            geom_line(data=obs.gl.i(), aes(y=Deaths.obs, alpha=src), 
                      method="loess", stat="smooth",
                      colour=1, size=1.5, span=0.5, formula=y~x) + 
            geom_vline(data=obs.max.gl.i(), aes(xintercept=Date), linetype=3) +
            geom_text(data=obs.lab.gl(), aes(label=lab), hjust=0, 
                      size=5, colour=1) +
            scale_colour_manual("Model Date", values=gl.cols(),
                                labels=as.Date(unique(gl.i()$model_date), 
                                               format="%Y_%m_%d") %>%
                                    format("%b %d"),
                                guide=guide_legend(order=1)) +
            scale_alpha_manual("", values=0.4,
                               guide=guide_legend(order=3,
                                                  title.position="bottom")) +
            scale_fill_brewer("\n\n\nObserved", type="seq",
                              guide=guide_legend(order=2)) +
            scale_y_continuous(labels=pretty_numbers, position="right") + 
            labs(x="", y="") +
            facet_grid(span~., scales="free_y", switch="y") + 
            theme(axis.text=element_text(size=14),
                  legend.text=element_text(size=14),
                  axis.title=element_text(size=16),
                  legend.title=element_text(size=16), 
                  title=element_text(size=18), 
                  strip.text=element_text(size=16))
        
    }, width=600, height=675)
    
    ###---- Plot: States vs. means
    output$state <- renderPlot({
        ggplot(us.i(), aes(Date, Deaths, colour=model_date)) +
            geom_hline(yintercept=0, colour="gray30", size=0.25) +
            geom_line(aes(group=model_date), size=1) +
            geom_point(data=obs.us.i(), aes(y=Deaths.obs, fill=wDay),
                       colour="black", size=2, shape=21) +
            geom_text(data=us.i.starts(), aes(group=model_date),
                      label="|", size=5, fontface="bold", family="mono") +
            geom_line(data=obs.us.i(), aes(y=Deaths.obs, alpha=src),
                      method="loess", stat="smooth",
                      colour=1, size=1.5, span=0.5, formula=y~x) +
            geom_vline(data=obs.max.us.i(), aes(xintercept=Date), linetype=3) +
            geom_text(data=obs.lab.us(), aes(label=lab), hjust=0, size=5, colour=1) +
            scale_colour_manual("Model Date", values=us.cols(),
                                labels=as.Date(unique(us.i()$model_date),
                                               format="%Y_%m_%d") %>%
                                    format("%b %d"),
                                guide=guide_legend(order=1)) +
            scale_alpha_manual(NULL, values=0.4,
                               guide=guide_legend(order=3,
                                                  title.position="bottom")) +
            scale_fill_brewer("\n\n\nObserved", type="seq",
                              guide=guide_legend(order=2)) +
            scale_y_continuous(labels=pretty_numbers, position="right") +
            labs(x="", y="") +
            facet_grid(span~., scales="free_y", switch="y") +
            theme(axis.text=element_text(size=14),
                  legend.text=element_text(size=14),
                  axis.title=element_text(size=16),
                  legend.title=element_text(size=16),
                  title=element_text(size=18),
                  strip.text=element_text(size=16))

    }, width=600, height=675)
    
    # ###---- Plot: Country peaks
    # output$peaks.countries <- renderImage({
    #     list(src="figs/states_peak.pdf",
    #          width=500, 
    #          height=900) 
    # })
    # 
    # ###---- Plot: State peaks
    # output$peaks.states <- renderImage({
    #    list(src="figs/states_peak.pdf",
    #         width=500, 
    #         height=900) 
    # })
    
}

# Run the application 
shinyApp(ui=ui, server=server)
