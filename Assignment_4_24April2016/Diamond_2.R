library(ggplot2)
library(dplyr)
require(methods)


data("diamonds")
names(diamonds)
diamonds$color

cat("\014")

setwd("/Users/david/Documents/R_Training/Assignments/Assignment_4_24April2016")

#1. Price versus "x", "y" and "z"
#=================================

ggplot(aes(x=x,y=price), data = diamonds) + 
  geom_point(alpha=1/100)
  
ggsave("price_vs_x.png")

#Lowest value in "x" from which price is recorded in average is 
#3. Lowest recorded value in "x" = 0. 
#Cost of diamonds with respect to "X" seems to be between $5,000 and
#$7,500
ggplot(aes(x=y,y=price), data = diamonds) + 
  geom_point(alpha=1/100) 

ggsave("price_vs_y.png")

ggplot(aes(x=z,y=price), data = diamonds) + 
  geom_point(alpha=1/100) 

ggsave("price_vs_z.png")


# The three graphs present the same distribution. Difference is 
# that with respect to "y" and "z" the prices have more outliers

#2. Price vs. Depth
ggplot(aes(x=depth,y=price), data = diamonds) +
  geom_point()

ggsave("price_vs_depth.png")

# Price seems to have a normal distribution with respect to the 
# depth at which the diamonds are found.
  
ggplot(aes(x=depth,y=price), data = diamonds) +
    geom_point(alpha=1/100) 
ggsave("price_vs_depth_alpha1over100.png")

# Most depths vary from 58 to 65
# For a single depth the price of the diamonds vary from 
# 10 up to 7,500 (depth of 62.5). Therefore the use of depth 
# as a key psaramreter seems irrelevant

#3. Plot price vs. carat

colMax <- function(data) sapply(data, max, na.rm = TRUE)

diamonds.max <- colMax(diamonds)

max_price <- 0.99*diamonds.max[7]
max_carat <- 0.99*diamonds.max[1]

ggplot(aes(x=carat,y=price), data = diamonds) +
  coord_cartesian(xlim = c(0, max_carat), 
                  ylim = c(0, max_price)) + 
       geom_point(alpha=1/5, 
                 position = position_jitter(h = 0),
                 color = 'orange')
ggsave("90pctprice_vs_99pctcarat.png")


#4. Plot price as a function of volume
diamonds <- diamonds %>%
  mutate(volume = x*y*z)

ggplot(aes(x=volume,y=price), data = diamonds) +
  coord_cartesian(xlim = c(0, 600), 
                  ylim = c(0, max_price)) +
  geom_point(alpha=1/100, 
             position = position_jitter(h = 0),
             color = 'orange') 
ggsave("price_vs_volume.png")
#It can be observed that the price vs volume is linearly related
#to the X, Y and Z components. Most diamods analyzed have a volume
#between 10 and 200. Price seems to behave linearly with respect to 
#volume between 10 and 200cubic units

#5. Price for diamonds with less than 800 cubic units



price <- c()
volume<- c()
j<-1
for (i in 1: length(diamonds$volume))
{
  if ((diamonds$volume[i] < 800) & (diamonds$volume[i] > 0))
  {
    price[j] <- diamonds$price[i]
    volume[j] <- diamonds$volume[i]
    j <- j + 1
  }
}

length(price)
length(volume)


diamonds_price_volume <- data.frame(price, volume)

require(methods)
ggplot(diamonds_price_volume, aes(x=volume,y=price))+
  geom_point(alpha=1/10, color="orange")+ 
  stat_smooth(method = "lm", formula = y - x, size = 1)

is.numeric(volume)
is.numeric(price)

  coord_cartesian(xlim = c(0, 800)) +
  geom_point(alpha=1/10, 
             position = position_jitter(h = 0),
             color = 'orange') 

