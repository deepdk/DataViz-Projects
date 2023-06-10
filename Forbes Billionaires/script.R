library(tidyverse)
library(janitor)
library(ggpattern)
library(showtext)
library(ggpattern)

font_add(family = "rubik", regular = "D:/HP laptop/Fonts/theboldfont.ttf")
showtext_auto()

df <- read_csv("billonaires.csv")

Bernard_Arnault_and_Family <- "https://1187604958.rsc.cdn77.org/data/images/full/293070/bernard_arnault_-3-_-_2017_-cropped-jpg.jpg"
Elon_Musk <- "https://api.time.com/wp-content/uploads/2014/11/elon-musk.jpg"
Jeff_Bezos <- "https://media.newyorker.com/photos/5c5daf63ae58262aa8c4a1b6/master/w_2560%2Cc_limit/Cassidy-Bezos.jpg"
Larry_Ellison <- "https://api.time.com/wp-content/uploads/2014/09/larry-ellison.jpg"
Warren_Buffet <- "https://advice4finance.com/wp-content/uploads/2018/12/Warren-Buffet.jpg"
Bill_gates <- "https://www.itreseller.ch/imgserver/Artikel/Personen/2020/mid/Bill_Gates_200316_090318.jpg"
Michael_Bloomberg <- "https://media.vanityfair.com/photos/5bfc230249a5732cd95df343/master/w_2560%2Cc_limit/l-Wall-Street-Pro-Bloomberg.jpg"
Carlos_Slim_Helu_and_Family <- "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f6/Carlos_Slim_2012.jpg/1200px-Carlos_Slim_2012.jpg"
Mukesh_Ambani <- "https://vz.cnwimg.com/wp-content/uploads/2013/11/GettyImages-187455378.jpg"
Steve_Ballmer <- "https://myce.wiki/wp-content/images_posts/2017/05/Steve_Ballmer_at_CES_2010_cropped.jpg"

ggplot(data = df, aes(reorder(name, -net_worth), net_worth,pattern_filename = as.factor(net_worth))) +
  geom_col_pattern(pattern = "image",
    color = "black",
    alpha = 0.8,
    pattern_type = "expand"
  ) +
  scale_pattern_filename_manual(
    values = c(`211` = Bernard_Arnault_and_Family, `180` = Elon_Musk, `114` = Jeff_Bezos, `107` = Larry_Ellison, `106` = Warren_Buffet, `104` = Bill_gates, `94.5` = Michael_Bloomberg, `93` = Carlos_Slim_Helu_and_Family, `83.4` = Mukesh_Ambani, `80.7` = Steve_Ballmer)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(labels = function(x) paste0("$", x, "B"))+
  annotate("text", x=6.5, y=180, label= "Forbes Billionaires", color = '#663333', size = 35, family = 'rubik', fontface = "bold")+
  annotate("text", x=7, y=150, label= "The Richest in 2023", color = '#663333', size = 20, family = 'rubik', fontface = "bold")+
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 20, face = "bold", family = "rubik",color = '#663333'))+
  theme(axis.text.y = element_text(size = 20, face = "bold",family = "rubik",color = '#663333'))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(plot.background  = element_rect(fill="#FFE4E1", color="#FFE4E1"))

