
# 3.2.1b

# Leonie Sindern
# 12696781


cheat <- function(exercise){
  
  if (exercise == 1){
    print(noquote("RESULTS QUESTION 1"))
    print(noquote("mygrades <- rnorm(n = 60, mean = 7.5, sd = 1.5)"))
    print(noquote("mygrades[mygrades > 10] <- 10"))
    print(noquote("hist(mygrades, breaks = 15, xlab = 'Grades', xlim = c(0, 10))"))
    print(noquote("abline(v = 5.5, col = 'blue, lwd = 3)"))
  }
  
  else if (exercise == 2){
    print(noquote("RESULTS QUESTION 2"))
    print(noquote("mydata2 <-  "))
    print(noquote("read_excel('~/MASTER 1/3 - programming/A3 Question 2.xlsx')"))
    print("View(mydata2)")
    
    print(noquote("plot(x = mydata2$DATE, y = mydata2$TMAX"))
    print(noquote('xlab = "Year", ylab = "Temperature"'))
    print(noquote("xlim = c(1951, 2021), col = 'purple'"))
    print(noquote("main = 'Tempeature Schipol Airport: 1951 - 2021'"))
    print(noquote("frame.plot = TRUE"))
  }
  
  else if (exercise == 3) {
    print(noquote("mydata3 <- titanic_train"))
    print(noquote("mydata3 %>%"))
    print(noquote("ggplot(aes(x = Sex, fill = factor(Survived))) +"))
    print(noquote("geom_bar(stat = 'count') +"))
    print(noquote("scale_fill_discrete(labels = c('dead', 'alive'), name = 'How did it go?')"))
  }
} 


# this function gives the answer to questions of the first part of this week's
# assignment. The input is the question number you wish to know the answer to. 

# eg. cheat(exercise = 1) gives the the answer to the first question

