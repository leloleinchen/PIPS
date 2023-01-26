####################################
###  Assignment 3 - Mandatory R  ###
###  Leonie Sindern              ###
###  12696781                    ###
####################################

library("readxl")
library("ggplot2")
library("titanic")
library("ggstatsplot")
library("plotly")
library("gganimate")
library("cranlogs")
library("gifski")
library("quantmod")
library("devtools")
library("memer")
library("styler")
library("lintr")
library("patchwork")


################
## QUESTION 1 ##
################

mygrades <- rnorm(n = 60, mean = 7.5, sd = 1.5)
mygrades[mygrades > 10] <- 10

hist(mygrades,
     breaks = 15,
     xlab = "Grades",
     xlim = c(0, 10))
abline(v = 5.5, col = "blue", lwd = 3)



################
## QUESTION 2 ##
################

mydata2 <-
  read_excel("~/MASTER 1/3 - programming/A3 Question 2.xlsx")
View(mydata2)

plot(
  x = mydata2$DATE,
  y = mydata2$TMAX,
  xlab = "Year",
  ylab = "Temperature",
  xlim = c(1951, 2021),
  col = "purple",
  main = "Tempeature Schipol Airport: 1951 - 2021",
  frame.plot = TRUE
)



################
## QUESTION 3 ##
################

mydata3 <- titanic_train
head(mydata3)

mydata3 %>%
  ggplot(aes(x = Sex, fill = factor(Survived))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(labels = c("dead", "alive"), name = "How did it go?")



################
## QUESTION 4 ##
################

mydata4 <- mydata3

mydata4 %>%
  ggplot(aes(x = Sex, fill = factor(Survived))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(labels = c("dead", "alive"), name = "How did it go?") +
  theme_bw()


# The best one is theme_bw() because there is no distracting background colors
# and the gridlines are more visible, the worst one is theme_dark() because the
# background is too dark.



################
## QUESTION 5 ##
################

plot(
  ToothGrowth$supp,
  ToothGrowth$len,
  ylab = "Length of Odontoblasts",
  xlab = "Supplement",
  # label the axes
  main = "Effect of Vitamin C on Tooth Growth in Guinea Pigs",
  # give the plot a title
  col = "lightblue",
  # change the color
  ylim = c(0, 35)
) # specify limits of the y axis



################
## QUESTION 6 ##
################

mydata6 <- ChickWeight
head(mydata6)

mymax <-
  aggregate(
    x = c(mydata6$weight),
    by = list(mydata6$Chick),
    FUN = max
  )

which(mymax$Group.1 == 1)
which(mymax$Group.1 == 3)
which(mymax$Group.1 == 5)
which(mymax$Group.1 == 20)
which(mymax$Group.1 == 40)

mymax2 <- mymax[c(15, 6, 14, 38, 18),]

plot6 <- mymax2 %>%
  ggplot(aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = mymax2$Group.1)
plot6



################
## QUESTION 7 ##
################

plot7 <- mydata6 %>%
  ggplot(aes(x = Time, y = weight)) +
  geom_smooth(method = "lm", se = TRUE)

plot7



################
## QUESTION 8 ##
################

mydata8 <- mydata6[mydata6$Chick %in% c(1, 20, 3, 40, 5),]

mydata8$Chick <- mydata8$Chick %>% factor(levels = c("1", "20", "3", "40", "5"),
                                          ordered = TRUE)

plot8 <- mydata8 %>%
  ggplot(aes(x = Time, y = weight, color = Chick)) +
  geom_line()

plot7 + plot8

################
## QUESTION 9 ##
################

ggbetweenstats(data = mydata6, x = Diet, y = weight)

# Diet 3 seems to be the best the chicks on this diet weight the most. The worst
# one appears to be diet 1.



#################
## QUESTION 10 ##
#################

mydata10 <-
  read.csv(
    paste0(
      "https://raw.githubusercontent.com/hannesrosenbusch/",
      "schiphol_class/master/",
      "Body%20Measurements%20_%20original_CSV.csv"
    )
  )

head(mydata10)

fig <- plot_ly(
  mydata10,
  x = ~ TotalHeight,
  z = ~ ShoulderToWaist,
  y = ~ LegLength,
  type = "scatter3d",
  mode = "markers",
  opacity = 0.4
)
fig




#################
## QUESTION 11 ##
#################

mydata11 <- cran_downloads(
  packages = c("caret", "tidymodels"),
  from = "2014-01-01",
  to = "2022-12-31"
)
head(mydata11)



mydata11 %>%
  ggplot(aes(x = date, y = count, color = package)) +
  labs(title = "Package Popularity over time", y = "Package Downloads") +
  geom_line() +
  theme_bw() +
  transition_reveal(date)




#################
## QUESTION 12 ##
#################

getSymbols("SHEL",
           from = "2022-01-01",
           to = "2022-12-31",
           reload.Symbols = TRUE)

mydata12 <- SHEL
chartSeries(mydata12, TA = NULL, type = "line")



#################
## QUESTION 13 ##
#################

plotstocks <-
  function(stock = "SHEL",
           year = 2022,
           name = "Assignment3-Shell") {
    
    getSymbols(
      stock,
      from = paste(year, "01", "01", sep = "-"),
      to = paste(year, "12", "31", sep = "-")
    )
    
    myname <- as.character(name)
    mynamefile <- paste(name, "png", sep = ".")
    
    
    png(mynamefile)
    chartSeries(get(stock), TA = NULL, type = "line")
    dev.off()
  }

plotstocks()



#################
## QUESTION 14 ##
#################

mystock %>%
  getSymbols(from = paste(year, "01", "01", sep = "-"),
             to = paste(year, "12", "31", sep = "-"))



#################
## QUESTION 15 ##
#################

a <- "dog"
b = "cat"
v = function(x, y) {
  xx <- strsplit(x, "",)[[1]]
  yy <- strsplit(y, "",)[[1]]
  lxx = length(xx)
  lyy = length(yy)
  v <- lxx == lyy
  if (v)
    return(T)
}
v(a, b)

# WHAT DOES THE CODE DO?
# - two string vectors are created named "a" and "b"
# - a function is created that returns true if the two characters that are
#   put into it are of the same length. If they are not, nothing happens.
# - the function is run with the previously created vectors "a" and "b" which
#   returns true

# WHY IS THE CODE BAD?
# This code is bad because:
# - the variable names are confusing
# - assigning values to vectors is sometimes done with "<-" and
#   sometimes with "="
# - it should have more spaces, eg. before opening a curly brace
# - you should write TRUE instead of only T



#################
## QUESTION 16 ##
#################

matrix(data = (1:9),
       nrow = 3,
       byrow = TRUE) * rep(seq(3), 3)



#################
## QUESTION 17 ##
#################

# Ctrl + Shift + A



#################
## QUESTION 18 ##
#################

meme_list()
meme_get("HotlineDrake") %>%
  meme_text_drake("Starting the assigment on time",
                  "doing it last minute with constant death wish")

meme_get("HotlineDrake") %>% meme_text_drake(
  "Writing a perfectly acceptable code with more than 6 numbers",
  "Writing an equally acceptable code with less than 6 numbers",
  size = 30
)

meme_get("SuezExcavator") %>% meme_text_suez("my coding issues", "Hannes")

meme_get("OffRamp") %>% meme_text_("hello")

meme_get("DistractedBf") %>% meme_text_distbf(newgirl = "PYTHON",
                                              guy = "ME",
                                              oldgirl = "R")

meme_get("FirstWorldProbs") %>% meme_text_bottom("mood")
