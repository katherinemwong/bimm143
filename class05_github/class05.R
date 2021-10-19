#' ---
#' title: Class 05 Data Visualization
#' author: "Katherine Wong PID (A16162648)"
#' ---

#Let's start with a scatterplot
#Before we can use it we need to load it up!

#install.packages("ggplot2) 
library(ggplot2)

# Every ggplot has a data + aes + geoms
ggplot(data=cars) +
  aes(x=speed, y=dist) +
  geom_point() +
  geom_smooth()

#Change to a linear model
p <- ggplot(data=cars) +
  aes(x=speed, y=dist) +
  geom_point() +
  geom_smooth(method="lm")

p + labs(title="My nice plot",
         x="Speed (MPH)", y="Distance")

#Base graphics is shorter
plot(cars)

#BW theme & extra labels!
ggplot(cars) + 
  aes(x=speed, y=dist) +
  geom_point() +
  labs(title="Speed and Stopping Distances of Cars",
       x="Speed (MPH)", 
       y="Stopping Distance (ft)",
       subtitle = "Your informative subtitle text here",
       caption="Dataset: 'cars'") +
  geom_smooth(method="lm", se=FALSE) +
  theme_bw()

#Anti-viral drug
#First read the dataset
url <-  "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

#Q. How many genes
nrow(genes)

#Q. How to access State col
table(genes$State)

#Q. What % are up/down
round( table(genes$State)/nrow(genes) * 100, 2 )

#Time to plot
ggplot(genes) + 
  aes(x=Condition1, y=Condition2) +
  geom_point()

p <- ggplot(genes) + 
    aes(x=Condition1, y=Condition2, col=State) +
    geom_point() 

#change colors, add labels
p + scale_colour_manual( values=c("blue","gray","red") ) + 
    labs(title="Gene Expresion Changes Upon Drug Treatment",
    x="Control (no drug) ",
       y="Drug Treatment")

#OPTIONAL: going further
# File location online
url <- "https://raw.githubusercontent.com/jennybc/gapminder/master/inst/extdata/gapminder.tsv"
gapminder <- read.delim(url)
library(dplyr)
gapminder_2007 <- gapminder %>% filter(year==2007)

#Adding more variables to aes()
ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp, color=continent, size=pop) +
  geom_point(alpha=0.5)

ggplot(gapminder_2007) + 
  aes(x = gdpPercap, y = lifeExp, color = pop) +
  geom_point(alpha=0.8)

#Adjusting point size
ggplot(gapminder_2007) + 
  aes(x = gdpPercap, y = lifeExp, size = pop) +
  geom_point(alpha=0.5)

ggplot(gapminder_2007) + 
  geom_point(aes(x = gdpPercap, y = lifeExp,
                 size = pop), alpha=0.5) + 
  scale_size_area(max_size = 10)

gapminder_1957 <- gapminder %>% filter(year==1957)

#1957 Gapminder Scatterplot
ggplot(gapminder_1957) + 
  aes(x = gdpPercap, y = lifeExp, color=continent,
      size = pop) +
  geom_point(alpha=0.7) + 
  scale_size_area(max_size = 10) 

#1957 and 2007 
gapminder_1957 <- gapminder %>% filter(year==1957 | year==2007)

ggplot(gapminder_1957) + 
  geom_point(aes(x = gdpPercap, y = lifeExp, color=continent,
                 size = pop), alpha=0.7) + 
  scale_size_area(max_size = 10) +
  facet_wrap(~year)
#Bar plots
gapminder_top5 <- gapminder %>% 
  filter(year==2007) %>% 
  arrange(desc(pop)) %>% 
  top_n(5, pop)

gapminder_top5
#generating a bar plot
ggplot(gapminder_top5) + 
  geom_col(aes(x = country, y = pop))

#adding color to bar plot
ggplot(gapminder_top5) + 
  geom_col(aes(x = country, y = pop, fill = continent))
