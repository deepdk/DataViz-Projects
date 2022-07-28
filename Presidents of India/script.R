library(tidyverse)
library(rvest)
library(XML)
library(RCurl)
library(stringr)
library(showtext)
library(ggtext)
library(ggimage)
library(readxl)
library(png)
library(magick)


#Scraping the data

h <- "https://en.wikipedia.org/wiki/List_of_presidents_of_India"

df <- h %>% 
  read_html()%>%
  html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "wikitable", " " ))]') %>%    # select enclosing table node
  html_table()
tbl_df()  

df1 <- read.csv("G:/presidents.csv")

# Setting theme and font

font_add_google('Fira Sans', 'firasans')
font_add(family = "Sci Fi", regular = "../input/emeraldbeacon-font/emeraldbeacon.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#F0E68C"
  color.text = "#2F4F4F"
  
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
    theme(legend.title = element_text(family = "Serif",
                                    color = "#030303",
                                    size = 15, face = "bold"))+
                                    
     # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=90, face = "bold", family = 'Sci Fi'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", family = 'firasans'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.y      = element_text(size=25, color =color.text, face = "bold", family = 'firasans')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Serif')) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# Reading the data
df <- read_excel("../input/dataset/presidents of inida.xlsx")

# Cleaning
df1 <- df%>%
clean_names()
df1 %>%
mutate(start_of_term = as.Date(start_of_term),
      end_of_term = as.Date(end_of_term))
      
# Adding Images
final <- df1%>%
mutate(image = case_when(name =="Rajendra Prasad"~"../input/images-of-presidents/16presi-rajendra-prasad.png",
                         name =="Sarvepalli Radhakrishnan"~"../input/sarvapalli-radhakishnan/16presi-radhakrishnan1.png",
                         name == "Zakir Husain"~"../input/images-of-presidents/16presi-zakir-husain.png",
                         name =="V. V. Giri"~"../input/images-of-presidents/VV_Giri_1974_stamp_of_India_(cropped).jpg",
                         name == "Mohammad Hidayatullah"~"../input/images-of-presidents/Hidayatullah.png",
                         name == "Fakhruddin Ali Ahmed"~"../input/images-of-presidents/Fakhruddin_Ali_Ahmed_1977_stamp_of_India_(cropped).jpg",
                         name == "B. D. Jatti"~"../input/bdjatti/B.D_Jatti_(enhanced).jpg",
                         name == "Neelam Sanjiva Reddy"~"../input/images-of-presidents/Neelam_Sanjiva_Reddy_(cropped).jpg",
                         name =="Zail Singh"~"../input/images-of-presidents/Giani_Zail_Singh_1995_stamp_of_India_(cropped).png",
                         name == "Ramaswamy Venkataraman"~"../input/images-of-presidents/R_Venkataraman_(cropped).jpg",
                         name == "Shankar Dayal Sharma"~"../input/images-of-presidents/Shankar_Dayal_Sharma_36.jpg",
                         name == "K. R. Narayanan"~"../input/images-of-presidents/President_Clinton_with_Indian_president_K._R._Narayanan_(cropped).jpg",
                         name == "A. P. J. Abdul Kalam"~"../input/images-of-presidents/A._P._J._Abdul_Kalam.jpg",
                         name == "Pratibha Patil"~"../input/images-of-presidents/800px-The_President_of_India_Smt._Pratibha_Patil.jpg",
                         name == "Pranab Mukherjee"~"../input/images-of-presidents/800px-Pranab_Mukherjee_Portrait_(cropped).jpg",
                         name == "Ram Nath Kovind"~"../input/images-of-presidents/Ram_Nath_Kovind_official_portrait.jpg"))
                         
 final$middle<- 2000
 
 p1 <- final %>% 
ggplot(aes(reorder(name, middle),total_days_served))+
geom_hline(aes(yintercept = middle), color = "#8B0A50", size = 0.8)+
geom_segment(aes(x = name, xend = name,y = middle, yend = total_days_served), size = 0.8, color = "#8B0A50")+
geom_point(aes(x=name, y=middle), color="black", size = 0.5)+
geom_image(aes(x = name, y = total_days_served, image = image), asp = 0.8)+
annotate("text", x = 15.5, y = 950, label = "13-05-1967 to 13-05-1969", size = 6,  family = "firasans",color = "#8B0A50", fontface= "bold")+
annotate("text", x = 15, y = 2500, label = "25-07-1982 to 25-07-1987", size = 6, family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 13.4, y = 500, label = "03-05-1969 to 20-07-1969", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 14, y = 2500, label = "24-08-1969 to 24-08-1974", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 13, y = 2500, label = "25-07-1992 to 25-07-1997", size = 6, family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 12, y = 2500, label = "13-05-1962 to 13-05-1967", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 11, y = 2500, label = "25-07-1987 to 25-07-1992", size = 6, family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 10, y = 2500, label = "25-07-2017 to 25-07-2022", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 8.4, y = 4000, label = "26-01-1950 to 13-05-1962", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 8, y = 2500, label = "25-07-2007 to 25-07-2012", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 7, y = 2500, label = "25-07-2012 to 25-07-2017", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 6, y = 2500, label = "25-07-1977 to 25-07-1982", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 4.4, y = 400, label = "20-07-1969 to 24-08-1969", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 4, y = 2500, label = "25-07-1997 to 25-07-2002", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 2.4, y = 1000, label = "24-08-1974 to 11-02-1977", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 1.4, y = 300, label = "11-02-1977 to 25-07-1977", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
annotate("text", x = 1, y = 2500, label = "25-07-2002 to 25-07-2007", size = 6,  family = "firasans",color = "#8B0A50",fontface= "bold")+
coord_flip()+
my_theme()+
theme(axis.title.y = element_blank())+
labs(x = "Total Days",
title = "Presidents of India",
    subtitle = "The president of India is the head of state of the Republic of India. \n The president is the nominal head of the executive as well as the \n commander-in-chief of the Indian Armed Forces. \n The office of president was created when India became a republic on 26 January 1950, \n when its constitution came into force. \n The president is indirectly elected by an electoral college comprising both houses of \n the Parliament of India and the legislative assemblies of each of India's states and \n territories, who themselves are all directly elected.",
    caption = "Data Source : https://en.wikipedia.org/wiki/List_of_presidents_of_India")
p1
