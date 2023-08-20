#extract the data from zillow.com
library(data.table)
library(dplyr)
library(echarts4r)
process_file <- function(filename, hometype) {
  mydata_all <- data.table::fread(filename)
  mycols <- grep("((2007)|(2008)|(2009)|(201.)|(202.))-12-", names(mydata_all), value = T)
  mydata_all[, RegionID := as.character(RegionID)]
  myregionids <- c("38128", "10221", "24043", "44269", "6181", "20330", "17426", "40326", "41176")
  mydata <- mydata_all[RegionID %chin% myregionids, c("RegionName", mycols), with = FALSE]
  setnames(mydata, "RegionName", "City")
  mydt <- melt(mydata, id.vars = "City", variable.name = "Month", value.name = "Value")
  mydt[, Region := fcase(
    City %chin% c("New York", "Boston"), "Northeast",
    City %chin% c("Dallas", "Austin", "Charlotte", "Tampa"), "South",
    default = "Other"
  )]
  setorder(mydt, City)
  mydt[, HomeType := hometype]
  return(mydt)
}
houses <- process_file("/Users/nandinibhusanurmath/Documents/R_files/R_basics/sfr.csv", "SingleFamily")
houses_wide <- dcast(houses[, c("City", "Month", "Value")], Month ~ City)
condos <-process_file("/Users/nandinibhusanurmath/Documents/R_files/R_basics/condo.csv", "Condo")




head(houses_wide)
library(echarts4r)
library(dplyr)



houses_wide %>%
  e_charts(x = Month)




houses_wide %>%
  e_charts(x = Month) %>%
  e_line(serie = `San Francisco`)



houses_wide %>%
  e_charts(x = Month) %>%
  e_line(serie = `San Francisco`) %>%
  e_line(serie = Boston) 




p <- houses_wide %>%
  e_charts(x = Month) %>%
  e_line(serie = `San Francisco`) %>%   
  e_line(serie = Boston) %>%  
  e_tooltip(trigger = "axis")
p

p %>%
  e_theme("bee-inspired")


p %>%
  e_theme_custom("Untitled.json") %>%
  e_color(background = "ivory")
p


my_colors <- c("darkblue", "#03925e", "purple")
p <- houses_wide %>% 
  e_charts(x = Month) %>%
  e_line(serie = `San Francisco`) %>%
  e_line(serie = Boston) %>%
  e_line(serie = Austin) %>%
  e_tooltip(trigger = "axis") %>%
  e_color(my_colors)
p

library(RColorBrewer)
my_colors <- brewer.pal(3, "Dark2")
my_colors
p %>%
  e_color(my_colors)




library(paletteer)
paletteer_d("ggthemes::Color_Blind")
my_colors <- paletteer_d("ggthemes::Color_Blind")[c(1,2,5)] 
p %>%
  e_color(my_colors)



myplot <- condos %>%
  group_by(City) %>% #<<
  e_charts(x = Month) %>%
  e_line(serie = Value) %>%
  e_tooltip(trigger = "axis") |>
  e_legend(  
    selector = list(  
      list(type = 'inverse', title = 'Invert'),
      list(type = 'all', title = 'Reset')
    )
  )
myplot


#custom legend
myplot %>%
  e_grid(right = '15%') %>%  
  e_legend(orient = 'vertical', 
           right = '5', top = '15%')

#Adding title to myplot
myplot %>% 
  e_title(text = "Monthly Median Single-Family Home Prices", 
          subtext = "Source: Zillow.com", 
          sublink = "https://www.zillow.com/research/data/",
          left = "center"
  )




#Animations with echarts (Covid Data)
mydata <- readr::read_csv("https://gist.githubusercontent.com/smach/194d26539b0d0deb9f6ac5ca2e7d49d0/raw/f0d3362e06e3cb7dbfc0c9df67e259f1e9dfb898/timeline_data.csv")

str(mydata)
summary(mydata)
mydata

mydata %>% 
  group_by(ReportDate) %>% #<<
  e_charts(State, timeline = TRUE) %>% #<<
  e_timeline_opts(autoPlay = TRUE, top = 40) %>% #<<
  e_bar(PctUsed, itemStyle = list(color = "#0072B2"))  %>% 
  e_legend(show = FALSE) %>% 
  e_labels(position = 'insideTop') %>%
  e_title("Percent Received Covid-19 Vaccine Doses Administered", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) %>%
  e_grid(top = 100)

#time delay
mydata %>%
  group_by(State) %>%
  e_charts(ReportDate) %>%
  e_line(PctUsed) %>%
  e_animation(duration = 8000) 


#Racing bar graph
mydata %>%
  group_by(ReportDate) %>%
  e_charts(State, timeline = TRUE) %>%
  e_bar(PctUsed, realtimeSort = TRUE, itemStyle = list(
    borderColor = "black", borderWidth = '1')
  ) %>%
  e_legend(show = FALSE) %>%
  e_flip_coords() %>%
  e_y_axis(inverse = TRUE)  %>%
  e_labels(position = "insideRight", 
           formatter = htmlwidgets::JS("
      function(params){
        return(params.value[0] + '%')
      }
    ") ) %>%
  e_add("itemStyle", color) %>%
  e_timeline_opts(autoPlay = TRUE, top = "55")  %>%
  e_grid(top = 100) %>%
  e_title(paste0("Percent Vaccine Dose Administered "), 
          subtext = "Source: Analysis of CDC Data", 
          sublink = "https://covid.cdc.gov/covid-data-tracker/#vaccinations", 
          left = "center", top = 10)
