library(shiny)
library(datasets)
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)    # alternative, this also loads %>%
library(ggplot2)

dementiaData = read.csv("State-Territory-Dementia.csv",header = T)
demantiaData2 = read.csv("State-Territory-Dementia-2011-2020_4.csv",header=T)
cost = read.csv("costComparison.csv",header=T)




graph <- ggplot(dementiaData,aes(Year,Number,colour=State)) + 
  geom_line()  

graph

graph2 <- ggplot(cost, aes(Reason)) +  
  coord_flip()+ geom_bar(aes(fill = status), position = position_stack(reverse = TRUE))

graph2

graph3 = ggplot(data=cost, aes(x=Reason, y=cost)) +
  geom_bar(fill=status) +
  guides(fill=FALSE) +  
  coord_flip()

graph3

graph4 = ggplot(data=cost, aes(x=Reason, y=cost, fill=status)) +
  geom_bar(stat="identity", position=position_dodge()) +  
  coord_flip()


graph4

