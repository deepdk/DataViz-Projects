library(tidyverse)
library(scales)
library(MetBrewer)
library(janitor)
library(showtext)
library(ggflags)
library(ggpubr)
library(ggbeeswarm)
library(stringr)

font_add(family = "Roboto", regular = "D:/HP laptop/Fonts/RobotoCondensed-Regular.ttf")
showtext_auto()

finisher <- read_csv("D:/tour de france/tdf_finishers_copy.csv")
stage <- read_csv("D:/tour de france/tdf_stages_copy.csv")
winner <- read_csv("D:/tour de france/tdf_winners_copy.csv")
tour <- read_csv("D:/tour de france/tdf_tours_copy.csv")

c_code <- read_csv("D:/country-codes.csv")

c_code <- c_code |> 
  clean_names()
head(c_code)

c_code <- c_code |> 
  rename(Country=english_short_name_lower_case)
head(c_code)

c_code |> 
  filter(Country == "United Kingdom")

my_theme <- function() {
  
  # Colors
  color.background = "#F5F5F5"
  color.text = "black"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks       = element_blank()) +
# Format the legend
    theme(legend.position = "none") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 15, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "Roboto",
                                    color = "#030303",
                                    size = 15, face = "bold"))+
# Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_text(size=15, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=15, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=15, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=15, color = color.text, face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=15, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

palette <- ("#FDB000","#FFFFFF","#FFFFFF","#FFFFFF","#000000")

# Starters and finishers by year
tour |> 
 pivot_longer(cols = c('Starters','Finishers'),
              names_to = 'type',
              values_to = 'value') |> 
  ggplot(aes(Year, value, fill = type))+
  geom_col(color = "black")+
  scale_fill_manual(values = c("#FDB000","#8B5A00"))+
  my_theme()+
  theme(legend.position = "left")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())


winner_by_country <- winner |> 
  mutate(Country= recode(Country, "Great Britain"="United Kingdom")) |> 
  count(Country, sort = TRUE)
winner_by_country

flag <- left_join(winner_by_country,c_code, by = "Country")
flag

#Winners by country
flag %>%
mutate(country = str_to_lower(alpha_2_code))%>%
arrange(desc(n))%>%
ggplot(aes(reorder(Country, n),n))+
geom_bar(stat = 'identity', width = 0.02,fill = '#8B5A00') +
geom_flag(aes(country = country), size = 12)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
my_theme()+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.y = element_blank())+
  theme(axis.text.x = element_blank())

# Distance by Year
tour |> 
  mutate(Distance_km_mean = mean(Distance_km))

tour |> 
  ggplot(aes(Year, Distance_km))+
  geom_area(fill = "#FDB000")+
  geom_point()+
  geom_hline(aes(yintercept = mean(Distance_km)))+
  my_theme()+
  theme(axis.title.x = element_blank())

stage |> 
  count(Type, sort = TRUE)

# Winning time of winners
winner |> 
  filter(!is.na(time_Hour)) |> 
  ggplot(aes(Year, time_Hour))+
  geom_line()+
  my_theme()+
  theme(axis.title.x = element_blank())

top_finisher <- finisher |> 
  count(Country, sort = TRUE)

stage |> 
  mutate(Distance_km = str_trim(Distance_km)) |> 
  mutate(Distance_km = as.numeric(Distance_km)) |>
  filter(Type == "Flat stage") |> 
  filter(Distance_km == max(Distance_km))

stage |> 
  mutate(Distance_km = str_trim(Distance_km)) |> 
  mutate(Distance_km = as.numeric(Distance_km)) |>
  filter(Type == "Mountain stage") |> 
  filter(Distance_km == max(Distance_km))

stage |> 
  mutate(Distance_km = str_trim(Distance_km)) |> 
  mutate(Distance_km = as.numeric(Distance_km)) |>
  filter(Type == "Hilly stage") |> 
  filter(Distance_km == max(Distance_km))

stage |> 
  filter(Type %in% c("Flat stage","Mountain stage","Hilly stage") )|> 
  mutate(Distance_km = str_trim(Distance_km)) |> 
  mutate(Distance_km = as.numeric(Distance_km)) |> 
  ggplot(aes(x = Type, y = Distance_km, color = Type)) +
  geom_quasirandom(method = "pseudorandom", size = 5, alpha = 0.3)+
  scale_color_manual(values = c("#FDB000","#8B5A00","#CD8500"))+
  coord_flip()+
  my_theme()

finisher |> 
  count(Country, sort = TRUE) |> 
  mutate(total = sum(n)) |> 
  mutate(per = (n/total)*100) |> 
  arrange(desc(per)) |> 
  head(5)

winner |> 
  select(Year, Rider) |> 
  group_by(Year, Rider) |> 
  count(Rider, sort = TRUE)
