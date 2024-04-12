
rm(list = ls())

library(tidyverse)
library(ggtext)
library(ggimage)

list <- list.files(path = "../rmd/resultados/graficos/30DayChartChallenge/stockPrices", pattern = "*.csv")

bases <- list()

list1 <- list

for(i in 1:length(list)){
  
  list[i] <- str_c("../rmd/resultados/graficos/30DayChartChallenge/stockPrices/", list[i], sep = "")
  
  bases[[i]] <- read.csv(file = list[i])
  
  bases[[i]]$name <- list1[i]
  
}

bases <- bases %>% 
  map(.f = ~{
    
    .x %>% 
      select(Date, Adj.Close, name)
    
  })


crec <- bases %>% 
  map(.f = ~{
    
    p1 <-  .x$Adj.Close[[1]]
    
    p2 <- .x$Adj.Close[[nrow(.x) - 1]]
    
    t <- length(.x$Adj.Close-1) - 1
    
    crec <- (1 + (p2/p1 - 1))^(1/t) - 1 
    
  })


bases <- bases %>% bind_rows()

bases <- bases %>% 
  mutate(Date = as.Date(Date),
         name = str_sub(name, 1, 4))

crec <- crec %>% unlist()

lis1 <- str_sub(list1, 1, 4)


baseTasas <- tibble(names = lis1,
                    crec = crec*100)

bases <- bases %>% 
  left_join(y = baseTasas, by = c("name" = "names"))

bases <- bases %>% 
  filter(name != "SNAP")

base_logos <- tribble(
  ~name, ~Adj.close, ~logo_url,
  lis1[1], 175.04, "https://upload.wikimedia.org/wikipedia/commons/f/fa/Apple_logo_black.svg",
  lis1[2], 189.05, "https://upload.wikimedia.org/wikipedia/commons/a/a9/Amazon_logo.svg",
  lis1[3], 523.16, "https://upload.wikimedia.org/wikipedia/commons/7/7b/Meta_Platforms_Inc._logo.svg",
  lis1[4], 427.93, "https://upload.wikimedia.org/wikipedia/commons/9/96/Microsoft_logo_%282012%29.svg"
  # list1[5], 11.36, "https://upload.wikimedia.org/wikipedia/commons/8/88/Snap_Inc_logo.jpg"
)

images <- map(base_logos$logo_url, magick::image_read_svg)

map2(.x = images, .y = lis1[-5],  .f = ~{
  
  magick::image_write(.x, path = paste("E:\\Documents\\Escritorio\\FLACSO\\Tesis\\petroleo\\rmd\\resultados\\graficos\\30DayChartChallenge\\logos\\", .y, ".png", sep = ""))
  
})

values_y <- bases$Adj.Close[bases$Date == "2024-04-01"]

grafico <- bases %>% 
  ggplot(mapping = aes(x = Date, y = Adj.Close)) +
  geom_line(aes(color = name), 
            linewidth = 0.8,
            show.legend = F) +
  geom_text(data = bases %>% 
              filter(Date == "2024-04-01"),
            mapping = aes(color = name,
                          x = Date + 210, y = Adj.Close, label = ifelse(crec > 0, str_c("+", round(crec, 2), "%", sep = " "),
                                                                  str_c(round(crec, 2), "%", sep = " "))),
            size = 3.5,
            nudge_y = c(-5, 10, 0, 0),
            show.legend = F) +
  scale_color_manual(values = c("black", "orange", "blue","darkgreen")) +
  scale_x_date(
    breaks = as.Date(c("2012-04-01",
                       "2015-12-01",
                       "2018-12-01",
                       "2021-12-01",
                       "2024-04-01"))) +
  scale_y_continuous(n.breaks = 10) +
  geom_image(data = tibble(Date = as.Date("2026-01-01"), Adj.Close = values_y, ),
           aes(image = c("AAPL.png", "AMZN.png", "META.png", "MSFT.png")
               ),
           size = c(0.07, 0.15, 0.15, 0.15),
           nudge_y = c(-5, 10, 0, 0)) +
  labs(title = "**Comportamiento de las acciones de 4 GRANDES tecnológicas**",
       subtitle = "Los porcentajes corresponden al rendimiento equivalente mensual entre **04-2012** y **04-2024**",
       caption = "Fuente: elaboración propia en base a Yahoo! Finance<br>**#30DayChartChallenge #Day11** @GEstebanGomez",
       x = NULL,
       y = "US Dollar") +
  theme_bw() +
  theme(plot.title = element_markdown(hjust = 0.5, size = 13),
        plot.subtitle = element_markdown(hjust = 0.0, size = 11),
        axis.text.x = element_text(size = 9), 
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 8),
        plot.caption = element_markdown(size=8, hjust=0.0, face="italic", color="black"))

ggsave(filename = "../rmd/resultados/graficos/30DayChartChallenge/11Day_mobile_friendly.png",
       plot = grafico,
       dpi = 500,
       width = 8.66,
       height = 4.83
)

  

