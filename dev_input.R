input <- list(over.type="CFR",
              over.span="Total",
              over.dates=c("2020-03-01", end=Sys.Date()),
              over.pK=T,
              
              f.country="US",
              f.modSource.gl=c("IHME", "MIT"),
              f.modDates.gl=c("2020_03_25", "2020_05_08",
                              "2020_04_16"),
              f.dates.gl=c("2020-03-01", "2020-07-01"),
              f.pK.gl=F,
              
              comp.region=c("US", "Switzerland", "US: Missouri"),
              comp.dates=as.Date(c("2020-03-01", "2020-08-08")),
              comp.pK=TRUE,
              
              f.state="Colorado",
              f.modSource.us=c("IHME", "MIT"),
              f.modDates.us=c("2020_03_25", "2020_05_08",
                              "2020_04_16"),
              f.dates.us=c("2020-03-01", "2020-07-01"),
              f.pK.us=F)
