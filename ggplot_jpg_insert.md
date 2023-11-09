rm(list=ls())
.libPaths()
library(tidyverse)
getwd()
setwd("D:/Non_Documents/ai/r")
library(patchwork)
library(showtext)

showtext_auto()


df <-tibble::tribble(
   ~SPLIT, ~SD폭, ~CLOSE, ~Half_tone_slit, ~투과율,    ~단차,  ~LOSS,
  "SP1_1",  2.5,      0,               1,   15,  0.068, -0.025,
  "SP1_2",   NA,      1,             4.8,   10,  0.062, -0.019,
  "SP1_3",   NA,    1.6,             2.6,  7.5,  0.071, -0.028,
  "SP1_4",   NA,    1.8,             2.2,  6.1,   0.06, -0.017,
  "SP1_5",   NA,      1,               0,    0,  0.043,      0,
  "SP2_1",    5,      0,               1,   15, -0.021,   0.18,
  "SP2_2",   NA,      1,             4.8,   10,  0.114,  0.045,
  "SP2_3",   NA,    1.6,             2.6,  7.5,  0.137,  0.022,
  "SP2_4",   NA,    1.8,             2.2,  6.1,  0.151,  0.008,
  "SP2_5",   NA,      1,               0,    0,  0.159,      0,
  "SP3_1",  7.5,      0,               1,   15, -0.023,  0.205,
  "SP3_2",   NA,      1,             4.8,   10,  0.081,  0.101,
  "SP3_3",   NA,    1.6,             2.6,  7.5,  0.122,   0.06,
  "SP3_4",   NA,    1.8,             2.2,  6.1,  0.148,  0.034,
  "SP3_5",   NA,      1,               0,    0,  0.182,      0,
  "SP4_1",   10,      0,               1,   15, -0.292,  0.511,
  "SP4_2",   NA,      1,             4.8,   10, -0.139,  0.358,
  "SP4_3",   NA,    1.6,             2.6,  7.5,  0.202,  0.017,
  "SP4_4",   NA,    1.8,             2.2,  6.1,  0.204,  0.015,
  "SP4_5",   NA,      1,               0,    0,  0.219,      0,
  "SP5_1", 12.5,      0,               1,   15,  0.066,  0.216,
  "SP5_2",   NA,      1,             4.8,   10,  0.182,    0.1,
  "SP5_3",   NA,    1.6,             2.6,  7.5,  0.226,  0.056,
  "SP5_4",   NA,    1.8,             2.2,  6.1,   0.27,  0.012,
  "SP5_5",   NA,      1,               0,    0,  0.282,      0,
  "SP6_1",   15,      0,               1,   15,  0.054,  0.271,
  "SP6_2",   NA,      1,             4.8,   10,   0.15,  0.175,
  "SP6_3",   NA,    1.6,             2.6,  7.5,  0.225,    0.1,
  "SP6_4",   NA,    1.8,             2.2,  6.1,  0.274,  0.051,
  "SP6_5",   NA,      1,               0,    0,  0.325,      0,
  "SP7_1", 17.5,      0,               1,   15,  0.101,  0.174,
  "SP7_2",   NA,      1,             4.8,   10,  0.183,  0.092,
  "SP7_3",   NA,    1.6,             2.6,  7.5,  0.242,  0.033,
  "SP7_4",   NA,    1.8,             2.2,  6.1,  0.255,   0.02,
  "SP7_5",   NA,      1,               0,    0,  0.275,      0,
  "SP8_1",   20,      0,               1,   15, -0.086,  0.377,
  "SP8_2",   NA,      1,             4.8,   10,  0.164,  0.127,
  "SP8_3",   NA,    1.6,             2.6,  7.5,  0.223,  0.068,
  "SP8_4",   NA,    1.8,             2.2,  6.1,  0.254,  0.037,
  "SP8_5",   NA,      1,               0,    0,  0.291,      0
  )




df %>% fill(SD폭, .direction = "down") %>% 
  filter(투과율 == 0) %>% 
  ggplot(aes(x=SD폭, y=단차, col=as.factor(투과율), shape = as.factor(Half_tone_slit), size=2))+geom_point()+
  #geom_smooth(span = 0.9) +
  scale_x_continuous(limits = c(2.5, 20), breaks = c(2:20))+
  scale_y_continuous(limits = c(0, 0.3), breaks = c(-0.1, 0, 0.1, 0.2, 0.3))+
  labs(title = "VIA 1.6㎛에서 SD 폭에 따른 단차",
       subtitle = "Confocal Microscope 계측(투명막 filter 적용)", 
       caption = "Auto PA Choi Pro/Jang Pro")+
  theme_bw()+
  theme(legend.position = "none")



p1 <- df %>% fill(SD폭, .direction = "down") %>% 
  filter(SD폭 %in% c(2.5, 10, 15, 17.5))%>% 
  filter(SD폭 %in% c(2.5, 10, 15, 17.5))%>% 
  mutate(SD폭 = as.factor(SD폭)) %>% 
  ggplot(aes(x=투과율, y=단차, col=SD폭, shape = SD폭, size = 2))+geom_point()+
  scale_x_continuous(limits = c(0, 15), breaks = c(0:15))+
  geom_hline(yintercept=0.1, linetype='dashed', color='blue', size=1)+
  geom_hline(yintercept=-0.1, linetype='dashed', color='blue', size=1)+
  geom_hline(yintercept=0,  color='blue', size=0.5)+
  labs(title = "VIA 1.6㎛에서 slit 투과율에 따른 단차",
       x = "Slit 투과율(%)", y= "metal 위 단차(㎛)",
       subtitle = "Confocal Microscope 계측(투명막 filter 적용)", 
       caption = "Auto PA Choi Pro/Jang Pro")+
  theme_bw()+
  scale_x_reverse()


p1+annotation_custom(rasterGrob(img1), xmin = -8, xmax = 0, ymin = -0.3, ymax = 0)

p2 <- df %>% fill(SD폭, .direction = "down") %>% 
  filter(SD폭 %in% c(2.5, 10, 15, 17.5))%>% 
  filter(SD폭 %in% c(2.5, 10, 15, 17.5))%>% 
  mutate(SD폭 = as.factor(SD폭)) %>% 
  ggplot(aes(x=투과율, y=LOSS, col=SD폭, shape = SD폭, size = 2))+geom_point()+
  scale_x_continuous(limits = c(0, 15), breaks = c(0:15))+
  geom_hline(yintercept=0.1, linetype='dashed', color='blue', size=1)+
  geom_hline(yintercept=-0.1, linetype='dashed', color='blue', size=1)+
  geom_hline(yintercept=0,  color='blue', size=0.5)+
  labs(title = "VIA 1.6㎛에서 slit 투과율에 Loss량(두께감소량)",
       x = "Slit 투과율(%)", y= "Slit에 의해 VIA두께 감소량(㎛)",
       subtitle = "Confocal Microscope 계측(투명막 filter 적용)", 
       caption = "Auto PA Choi Pro/Jang Pro")+
  theme_bw()

library(jpeg) 
library(grid)
img1 <- readJPEG("D:/Non_Documents/!2023_AUTO/양산/VIA 평탄화/3d.jpg") 

p2
p2+annotation_custom(rasterGrob(img1), xmin = 0, xmax = 8, ymin = 0.2, ymax = 0.5)



