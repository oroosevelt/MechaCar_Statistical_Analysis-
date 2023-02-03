x <- 3
numList <- c(0,1,2,3,4,5,6,7,8,9)
hello_world <- function(name, exclaim=TRUE){
  if(exclaim == TRUE){
    return(paste("hello", name, "!"))
  } else {
  return(paste("hello", name))
  }
}

demo_table <- read.csv(file='demo.csv',check.names=F,stringsAsFactors = F)

library(jsonlite)
?fromJSON
demo_table2 <- fromJSON(txt='demo.json')
x <- c(3, 3, 2, 2, 5, 5, 8, 8, 9)
x[3]
demo_table2[3, "year"]
demo_table[3,3]
demo_table$Vehicle_Class
demo_table$Vehicle_Class[3]
filter_table <- demo_table2[demo_table2$price > 10000,]
?subset()
filter_table3 <- demo_table2[("clean" %in% demo_table2$title_status) & (demo_table2$price > 10000) & (demo_table2$drive == "4wd"),]

x <- 5

?sample()
sample(c("cow", "deer", "pig", "chicken", "duck", "sheep", "dog"), 4)
num_rows <- 1:nrow(demo_table)
sample_rows <- sample(num_rows, 3)
demo_table[sample_rows,]
demo_table[sample(1:nrow(demo_table), 3),]

library(tidyverse)
?mutate()
demo_table <- demo_table %>% mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE) #add columns to original data frame
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer), .groups = 'keep') #create summary table
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer),Maximum_Price=max(price),Num_Vehicles=n(), .groups = 'keep') #create summary table with multiple columns

?gather()
install.packages("readxl")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

demo_table3 <- read.csv('demo2.csv',check.names = F,stringsAsFactors = F)
long_table <- gather(demo_table3,key="Metric",value="Score",buying_price:popularity)
long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)

?spread()
wide_table <- long_table %>% spread(key="Metric",value="Score")
all.equal(demo_table3, wide_table)
table <-demo_table3[, order(colnames(wide_table))]
# table <- demo_table3[,(colnames(wide_table))]
?ggplot()
# bar plot
head(mpg)
plt <- ggplot(mpg,aes(x=class)) #import dataset into ggplot2
plt + geom_bar() #plot a bar plot

?geom_bar()
#bar plot
mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n(), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count)) #import dataset into ggplot2
plt + geom_col() #plot a bar plot

# bar plot with labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") #plot bar plot with labels
theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees

# line plot with labels
mpg_summary <- subset(mpg,manufacturer=="toyota") %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=cyl,y=Mean_Hwy)) #import dataset into ggplot2
plt + geom_line()
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(16:30)) #add line plot with labels

# scatter with labels
plt <- ggplot(mpg,aes(x=displ,y=cty)) #import dataset into ggplot2
plt + geom_point() + xlab("Engine Size (L)") + ylab("City Fuel-Efficiency (MPG)") #add scatter plot with labels

# scatter plot with labels/color
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class") #add scatter plot with labels

#add scatter plot with multiple aesthetics
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive") #add scatter plot with multiple aesthetics

# box plot with labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") #plot a boxplot with labels
theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees


# create summary table
mpg_summary <- subset(mpg,manufacturer=="toyota") %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=cyl,y=Mean_Hwy)) #import dataset into ggplot2
# generate line plot
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(16:30)) #add line plot with labels

# SCATTER PLIT
plt <- ggplot(mpg,aes(x=displ,y=cty)) #import dataset into ggplot2
plt + geom_point() + xlab("Engine Size (L)") + ylab("City Fuel-Efficiency (MPG)") #add scatter plot with labels
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class") #add scatter plot with labels

#SCATTER PLOT WITH COLOR
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive") #add scatter plot with multiple aesthetics

# BOX AND WHISKER PLOT 
plt <- ggplot(mpg,aes(y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() #add boxplot

# BOX AND WHISKER PLOT
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot and rotate x-axis labels 45 degrees

# HEATMAP
mpg_summary <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary, aes(x=class,y=factor(year),fill=Mean_Hwy))
plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle Year",fill="Mean Highway (MPG)") #create heatmap with labels

# HEATMAP
mpg_summary <- mpg %>% group_by(model,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary, aes(x=model,y=factor(year),fill=Mean_Hwy)) #import dataset into ggplot2
plt + geom_tile() + labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") #add heatmap with labels > theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) #rotate x-axis labels 90 degrees

# BOX AND WHISKER AND A PLOT LAYER 
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() + #add boxplot
theme(axis.text.x=element_text(angle=45,hjust=1)) + #rotate x-axis labels 45 degrees
geom_point() #overlay scatter plot on top

# PLOT GRAPH
mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") #add scatter plot

# LAYERING PLOT 
mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ),SD_Engine=sd(displ), .groups = 'keep')
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") + #add scatter plot with labels
geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine)) #overlay with error bars

# CREATE SUMMARY TABLE  
mpg_long <- mpg %>% gather(key="MPG_Type",value="Rating",c(cty,hwy)) #convert to long format
head(mpg_long)

# BOX-WHISKER AND PLOT WITH COLORS
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot with labels rotated 45 degrees
theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels

#GEOM_DENSITY - QUALITATIVE TEST
ggplot(mtcars,aes(x=wt)) + geom_density() #visualize distribution using density plot

#LIBRARY TO PERFOMR OUR QUANTITATIVE TESTING 
?shapiro.test()

shapiro.test(mtcars$wt)

# LEFT SKEW OR NEGATIVE SKE IS WHEN THE LEFT TAIL IS LONGER THAN THE RIGHT 
# EXTREME NEGATIVE VALUES EXIST IN THE DAAT - MEDIAN IS MORE THE CENTEAL TENDENCY OF DATA

# RIGHT SKEW OR POSTIVE SKEW IS WHEN THE RIGHT TAI IS LONGER THAN THE LEFT 
# EXTREME POSTITVE VALUE EXIST - MEDIAN IS USED TO DESCRIBE THE CENTRAL TENDENCY OF THE DATA 

#  null hypothesis is also known as H0 and is generally the hypothesis that can be explained by random chance.
#  alternate hypothesis is also known as Ha and is generally the hypothesis that is influenced by non-random events.
#  PH REPRESENT THE PROBABILITY OF FLIPPING HEADS

# hypothesis testing uses the same five steps:

# 1. Generate a null hypothesis, its corresponding alternate hypothesis, and the significance level. 
# 2. Identify a statistical analysis to assess the truth of the null hypothesis.
# 3. Compute the p-value using statistical analysis.
# 4. Compare p-value to the significance level.
# 5. Reject (or fail to reject) the null hypothesis and generate the conclusion.

# Type I Error = FALSE POSITIVE
# Type II Error = FALSE NEGATIVE 

# 16.6.1
?sample_n()
# should not introduce biases when sampling function is used

population_table <- read.csv('used_car_data.csv',check.names = F,stringsAsFactors = F) #import used car dataset
plt <- ggplot(population_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

sample_table <- population_table %>% sample_n(50) #randomly sample 50 data points
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot


?t.test()
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven))) #compare sample versus population means

sample_table <- population_table %>% sample_n(50) #generate 50 randomly sampled data points
sample_table2 <- population_table %>% sample_n(50) #generate another 50 randomly sampled data points

# welch t test in console
t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven)) #compare means of two samples


# filtere by 1999 and 2008
mpg_data <- read.csv('mpg_modified.csv') #import dataset
mpg_1999 <- mpg_data %>% filter(year==1999) #select only data points where the year is 1999
mpg_2008 <- mpg_data %>% filter(year==2008) #select only data points where the year is 2008

t.test(mpg_1999$hwy,mpg_2008$hwy,paired = T) #compare the mean difference between two samples


# 16.6.5
# ANOVA Test - hypothesis
# H0 : The means of all groups are equal, or µ1 = µ2 = … = µn.
# Ha : At least one of the means is different from all other groups.

# Paired T-Test - hypothesis
# H0 : The difference between our paired observations (the true mean difference, or "μd") is equal to zero.
# Ha : The difference between our paired observations (the true mean difference, or "μd") is not equal to zero.

?aov()
# FORMULA  specila statement in R that tells the aov() function how to interpret the different variables
# formula Y ~ A or Y ~ A + B
# Y column name of dependent variable A/B column names of independent variable
# DATA name of our input data frame. contain columns for each variable

mtcars_filt <- mtcars[,c("hp","cyl")] #filter columns from mtcars dataset
mtcars_filt$cyl <- factor(mtcars_filt$cyl) #convert numeric column to factor
# aov() in console
aov(hp ~ cyl,data=mtcars_filt) #compare means across multiple levels
summary(aov(hp ~ cyl,data=mtcars_filt))

# 16.7.1
?cor()
head(mtcars)

# point plot with r=0
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() #create scatter plot

# Correlation, Variance and Covariance
#calculate correlation coefficient
cor(mtcars$hp,mtcars$qsec) 

#read in dataset
used_cars <- read.csv('used_car_data.csv',stringsAsFactors = F) 
head(used_cars)
#import dataset into ggplot2
plt <- ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price)) 
plt + geom_point() #create a scatter plot
# positive correlation 

#calculate correlation coefficient
cor(used_cars$Miles_Driven,used_cars$Selling_Price) 

#convert data frame into numeric matrix
used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")]) 
cor(used_matrix)

# LINEAR REGRESSION
# y = mx + b
# y = dependent variable
# m = slope 
# x = independent variable 
# b = the y intercept

# Linear Regression - hypothesis
# H0 : The slope of the linear model is zero, or m = 0
# Ha : The slope of the linear model is not zero 

# r^2 (r-squared) coefficient of determination and represents how well the regression model approx real world data

# linear models
?lm()

lm(qsec ~ hp,mtcars) #create linear model
summary(lm(qsec~hp,mtcars)) #summarize linear model
model <- lm(qsec ~ hp,mtcars) #create linear model
yvals <- model$coefficients['hp']*mtcars$hp +
model$coefficients['(Intercept)'] #determine y-axis values from linear model

plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model

lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars) #generate multiple linear regression model

summary(lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars)) #generate summary statistics

?chisq.test()
# Chi-squared test - hypothesis
# H0 : There is no difference in frequency distribution between both groups.
# Ha : There is a difference in frequency distribution between both groups

table(mpg$class,mpg$year) #generate contingency table
tbl <- table(mpg$class,mpg$year) #generate contingency table
chisq.test(tbl) #compare categorical distributions



















