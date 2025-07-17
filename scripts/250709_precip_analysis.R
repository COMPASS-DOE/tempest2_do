## Look at long-term patterns in precipitation

require(pacman)
p_load(AOI, climateR, 
       tictoc,
       tidyverse, 
       plotly,
       janitor)

theme_set(theme_bw())

yesterday <- Sys.Date() - days(2)

## Parameters listed here: https://docs.hyriver.io/examples/notebooks/gridmet.html
tic("read data")
precip_raw <- aoi_ext("Annapolis", units = "km", bbox = TRUE) %>% 
  getGridMET(AOI = .,
             varname   = c("tmmn", "tmmx", "pr"),
             startDate = "1995-01-01", 
             endDate = yesterday) %>% 
  as_tibble()
toc()
 
ggplot(precip_raw, aes(x = date, y = pr)) + 
  geom_line(color = "gray") +   
  geom_point(data = precip_raw %>% filter(as_date(date) == "2012-10-29"), color = "red") + 
  annotate(geom = "text", 
           x = as.POSIXct("2012-11-29"), 
           y = 175, 
           label = "Hurricane Sandy", 
           color = "red")
ggsave("figures/")






