#Find a plot in a published paper or scientific book. Evaluate it based on the principles Tufte lays out in the required reading.

##I chose a plot from a 2012 paper on neutron radiography "Average Soil Water Retention Curves Measured by Neutron Radiography"
##This plot overall does a good job in reducing the amount of ink used. The most amount of ink is on data.
##They could have gotten rid of the top and right sides of the graph outline. 
##They used three distinct lines for their data, the lines are very difficult to distinguish and they did a poor job of helping the viewer know what they are looking at.
##There isn't too much chartjunk on this plot, perhaps the different style lines could be considered chartjunk.
##They could have made the box outline a little bit thinner and the data thicker.
##In all, they do a pretty good job on following tufte

library(ggplot2)
diamonds
nrow(diamonds)
#The diamonds data set has 53,940 rows

set.seed(1410) #the set.seed() sets a random number generated sequence
dsmall <- diamonds[sample(nrow(diamonds), 100), ]

#this is saying to make a dataset named dsmall. dsmall is represented as a sample of the diamonds dataset using 100 rows of observations.

#Use dsmall to create a scatterplot of y vs x, colored by z values and faceted by cut
ggplot(dsmall, aes(x, y, colour = z)) +
  geom_point() +
  facet_wrap(~cut)

#A scatterplot of price vs carat, colored by cut and smoothed (using the "lm" method, without standard error bars)
ggplot(dsmall, aes(carat, price, colour = cut)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) #lm method fits a linear model, giving the line of best fit

#A density plot of carat, faceted and colored by clarity 
ggplot(dsmall, aes(carat, colour = clarity))+
  geom_density()+
  facet_wrap(~clarity)

#A boxplot of price as a function of cut 
ggplot(dsmall, aes(cut, price))+
  geom_boxplot()

#A scatterplot of y versus x. The points should be red (colour = "red"), the color of the smoothing line should be blue (colour = "blue"), and the line should be dashed with fat dashes (linetype=2). The x and y labels should be set manually as well. The trickiest part of this may be to figure out where colour = "red" etc should go in the code. Think about mapped vs static aesthetic values. 
ggplot(dsmall, aes(x,y, colour = "red"))+
  geom_point()+
  geom_smooth(method = "lm", colour = "blue", linetype = 2)+
  xlab("x, in mm")+
  ylab("y, in mm")
 
#Make the worst plot you possibly can in ggplot2. This plot should be awful in two independent respects: * It should represent the data misleadingly (this can sometimes be difficult with ggplot2, but be creative) * It should be as ugly as possible. (theme will be helpful here.) **Print this plot out and bring it in on Friday. We'll make a gallery of bad plots and the 'winner' will get a prize.
library(MASS)
data(Animals)
animal_df <- data.frame(Animals)

library(ggplot2)
ggplot(animal_df, aes(rownames(animal_df), brain))+
  geom_col()+
  theme_dark()+
  theme(panel.background = element_rect(color = "Pink", linetype = 5))+
  theme(panel.grid.major.x = )+
  theme(rect = element_rect())
  
#choice of bad graph, 7,17, 19

#best practices for plots
# data are meaningless| so do good science
# smoothing is misleading | be judicious with smoothing and be able to back it up
# didn't put number of observations | put the number of observations
# do we really need to plot this | think if it could be a sentence or a table
# too visually complex | keep it simple but meaningful
# blurry or bad resolution | think of the size that you actually want it to be make it that size

