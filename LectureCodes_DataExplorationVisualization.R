####loading data and manipulating data
#set working directory
#this is just a comment
setwd("C:/specify_your_path")

#local (load)
custdata <- read.table("custdata.csv",sep=',', header = TRUE)
#note: read.delim() for .txt files

#local (save)
write.table(custdata, "custdata.csv", sep=',', row.names= FALSE)

#online source
custdata <- read.table(
  file = "https://raw.githubusercontent.com/WinVector/zmPDSwR/master/Custdata/custdata.tsv",
  sep = "\t",
  header = TRUE
)

#inspecting data type
dim(custdata)
class(custdata)
head(custdata)

str(custdata)  #We observe that columns sex, marital.stat, housing.type,

custdata$sex= factor(custdata$sex)
custdata$marital.stat= factor(custdata$marital.stat)
custdata$housing.type= factor(custdata$housing.type)
custdata$state.of.res= factor(custdata$state.of.res)

levels(custdata$housing.type)

summary(custdata)


summary(custdata[c("sex","income","age")])

#apply fct. 
#mapply, apply, tapply
tapply(custdata$income, custdata$sex,summary)


###transforming data 
#adding and manipulating entries
#income in 1000$ units
custdata["income"] <- custdata["income"]/1000

#reverting
custdata["income"] <- custdata["income"]*1000

#adding a new column
custdata["income1000"] <- custdata["income"]/1000

#changing labels (1 entry)
colnames(custdata)[12] <- "incomeNEW"
#multiple labels
colnames(custdata)[c(1,12)] <- c("ID","income1000")

#remove 12th column
custdata = subset(custdata, select = -income1000)

#Summary Statistics + Spotting Problems
#Outliers, Missing Data, Range, Sentinels

colSums(is.na(custdata))


#Other useful functions for data analysis:
#attach()
summary(custdata$age)
attach(custdata)
summary(age)

#Visualization
#packages
install.packages("ggplot2")
library(ggplot2)

#custdata should be loaded with read.table 


### PLOTS ####
##single variables
#bar charts (discrete data chart)
ggplot(custdata) + geom_bar(aes(x=marital.stat))

#export plots
v1 <- ggplot(custdata) + 
  geom_bar(aes(x=marital.stat))
typeof(v1)
ggsave("vis_hist1.pdf", plot = v1, path = "C:/mypath/figures")


#horizontal bar chart
ggplot(custdata) + 
  geom_bar(aes(state.of.res)) +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.7)))


#sorted by size
#Important: ggplot only takes dataframe input!
statedata <- as.data.frame( table(custdata["state.of.res"]) )
colnames(statedata) <- c("state","count")
summary(statedata) 
#note: alphabetical 
statedata <- transform(statedata, state=reorder(state,count))
summary(statedata) #now: by counts

v2 <- ggplot(statedata) + 
  geom_bar(aes(x=state,y=count), 
           stat="identity") + #stat preserve x,y combination
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.7)))

ggsave("vis_hist2.pdf", plot = v2,
       path = "./Slides/figures")



#histograms (continuous/integer)
ggplot(custdata) +
  geom_histogram(aes(x=income1000), binwidth = 10, fill = "gray")


#density plots
library(scales) #library for manipulating axis labels
#reguar $-scale
ggplot(custdata) +
  geom_density(aes(x=income)) +
  scale_x_continuous(labels=dollar)

#log10-scale (equivalent plot density of log10(x) )
v3 <- ggplot(custdata) + 
  geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,100000), labels = dollar) +
  annotation_logticks(sides = "b")
ggsave("vis_density1.pdf", plot = v3,
       path = "C:/mypath/figures")



###mutliple variables
#line plots
x <- seq(from = -1, to = 1, by = 0.01)
y1 <- x^2 - 1
y2 <- sin(pi*x)
dat <- data.frame(x,y1,y2)
v0 <- ggplot(dat, aes(x=x)) + 
  geom_line(aes(y=y1), color = "darkred") +
  geom_line(aes(y=y2), color = "blue", linetype = "twodash") 

ggsave("vis_lines1.pdf", plot = v0,
       path = "C:/mypath/figures")

ggplot(xydat, aes(x=x,y=y)) + geom_line()





#scatter plots
#look at income/age relationship + remove irreasonable values
summary(custdata[,c("age","income")])
custdata2 <- subset(custdata,
                    (custdata["age"] > 0 & custdata["age"] < 100& custdata["income"] > 0))                   
ggplot(custdata2, aes(x=age,y=income)) +
  geom_point() 

ggsave("vis_scatter1.pdf", plot = v4,
       path = "C:/mypath/figures")

#linear and smooth fit plots
#linear regression line


v51 <- ggplot(custdata2, aes(x=age,y=income)) +
  geom_point() + 
  ylim(0,200000) +
  stat_smooth(method="lm")
ggsave("vis_scatter2.pdf", plot = v51,
       path = "C:/mypath/figures")
#smooth local linear regression
v52 <- ggplot(custdata2, aes(x=age,y=income)) +
  geom_point() + 
  ylim(0,200000) +
  geom_smooth()

ggsave("vis_scatter3.pdf", plot = v52,
       path = "C:/mypath/figures")
#bar chart by categories
#without category (see above)
ggplot(custdata) + 
  geom_bar(aes(x=marital.stat))
#type1: stacked (default)
ggplot(custdata) + 
  geom_bar(aes(x=marital.stat,fill=health.ins))
#type2: side-by-side
v6 <- ggplot(custdata) + 
  geom_bar(aes(x=marital.stat,fill=health.ins), position="dodge")


ggsave("vis_Bar2.pdf", plot = v6,
       path = "C:/mypath/figures")
#type3: filled/porportions
ggplot(custdata) + 
  geom_bar(aes(x=marital.stat,fill=health.ins), position="fill")

#boxplot (simple)
ggplot(custdata) + geom_boxplot(aes(y=income))
#boxplot over categories
v7 <- ggplot(custdata, aes(x=marital.stat, y=income)) + 
  geom_boxplot()


ggsave("vis_boxplot.pdf", plot = v7,
       path = "C:/mypath/figures")






  
  