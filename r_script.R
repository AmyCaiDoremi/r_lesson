gapminder <- read.csv(file = "data/gapminder_data.csv", stringsAsFactors = TRUE) 

x <- "xc"

# creating cats data frame 
cats <- data.frame(coat = c("calico", "black", "tabby"), 
                   weight = c(2.1, 5.0, 3.2), 
                   likes_string = c(1,0,1)) #c is the combine function 

cats[1]

# to show the column of the data frame 
cats$weight

# column-wide operations 
(cats$weight <- cats$weight + 2) 
cats$weight

# paste two things together 
paste("My cat is, ", cats$coat)

# Experiment 
#cats$coat + cats$weight

# data types 
typeof(cats$weight)
typeof(cats$coat)
typeof(cats$likes_string)

typeof(TRUE)

# changing types of variables 
cats$likes_string <- as.logical(cats$likes_string)
cats

# check structure of data frame 
str(cats)
str(gapminder)

# factors are categorical variables for qualitative data 

typeof(cats$coat)

# Change to factor 
as.factor(cats$coat)

cats$coat <- as.factor((cats$coat))
str(cats)

# Can you convert numeric data to factors? 
as.factor(cats$weight)
typeof(str(cats))

# show the spreadsheet of the data frame 
View(cats)

#check levels of a factor 
levels(gapminder$continent)

# creating vectors 
combine_vector <- c(2, 6 ,3)

combine_vector * 100

quiz_vector <- c(2, 6, "barber")

ab_vector <- c("a", "b")
ab_vector 

combine_example <- c(ab_vector, "swc")

sequence <- 1:100

seq(100)

big_vector <- 1:1000
head(big_vector)
tail(big_vector, n = 4)
length(big_vector)
length(cats$weight)

pizza_price <- c(pizzasubito = 5.64, pizzafresh = 6.60, callapizza = 4.50)
names(pizza_price)

pizza_price["pizzasubito"]

names(pizza_price)[3] <- "call-a-pizza"
names(pizza_price)[2] <- "pizza-fresh"
names(pizza_price)[1] <- "pizza-subito"
pizza_price

#list 

# collection of values that can have different data types 

list_example <- list(1, "a", TRUE)
list_example

complex_list <- list("a", 1:100, FALSE)
complex_list

str(complex_list)

# double brakets pulling out elements, single bracket pull out value 
# indexing with brakets notation 
complex_list[[2]][30]
complex_list[[1]][3] # this does not exist, NA 

complex_list[[2]]

cats$coat

cats[1]

gapminder[4]
str(gapminder)

cats[2, 3]
cats[1, 2]

cats$coat[2]

cats[1:2, 3]

cats[3, "coat"]

#[rows, columns]
cats[cats$coat != "tabby", ]

cats[ , "weight"]

cats[-1, "coat"]

nrow(cats)
ncol(cats)

test_run = gapminder[gapminder$continent %in% c("Europe", "Oceania"), ]
str(test_run)

levels(gapminder$continent)
dim(gapminder)
colnames(gapminder)

summary(gapminder)

mean(gapminder[gapminder$continent == "Africa", "gdpPercap"])

library(dplyr)

# calculate the group mean 
gapminder %>% # ctrl shift m 
  group_by(continent) %>% 
  summarize(mean = mean(gdpPercap))

year_country_gdp <- gapminder %>% 
  select(year, country, gdpPercap) # you can change the order of the columns here 

year_country_gdp <- select(gapminder, country, year, gdpPercap)

# - means omitting the columns 
small_dataset <- select(gapminder, -continent, -country, -year)

tidy_gdp <- gapminder %>% 
  rename(gdp_per_cap = gdpPercap)

head(tidy_gdp)


help(rename)

gapminder_Africa <- gapminder %>% 
  filter(continent == "Africa")

head(gapminder_Africa)
tail(gapminder_Africa)

summary(gapminder_Africa)


gapminder %>% 
  filter(year == 2010) %>% 
  select(-year) 

summary(gapminder_Africa) 

# this will print the output 
(lifeExp_bycountry <- gapminder %>% 
  group_by(country))

# summary for the groups 
lifeExp_bycountry <- gapminder %>% 
  group_by(country) %>% 
  summarise(mean_lifeExp = mean(lifeExp)) %>% 
  print(n=Inf)

lifeExp_bycountry


# summary for the groups 
# %>%  is shortcut so that we don't need to include the dataset in every function 
mean_lifeExp_by_country <- gapminder %>% 
  group_by(country) %>% 
  summarise(mean_lifeExp = mean(lifeExp)) %>% 
  arrange(desc(mean_lifeExp))%>% 
  #print(n=Inf)
  head(n = 10)

mean_lifeExp_by_country


# count(): good for frequency data 
gapminder %>% 
 filter(year == 2002) %>% 
  count(continent, sort = TRUE)

gapminder %>% 
  group_by(continent) %>% 
  summarise(se_le = sd(lifeExp)/sqrt(n()), n=n())

# it is okay in summarize to use the name of the character as the variable 
# n() is the number of rows 
# tibble is a mini dataframe 
gapminder %>% 
  group_by(continent) %>% 
  summarise(mean =mean(lifeExp),
            sd = sd(lifeExp),
            se_le = sd(lifeExp)/sqrt(n()), 
            n=n())


# use mutate() to create new variables 
gdp_total <- gapminder %>% 
  mutate(gdp = gdpPercap * pop)

gdp_total <- gapminder %>% 
  mutate(gdp = gdpPercap * pop)

head(gdp_total)


gapminder <- gapminder %>% 
  mutate(gdp = gdpPercap * pop, log_pop = log(pop))

head(gapminder)




library(ggplot2)

# ggplot is based on the grammer of graphics, any graphs of data can be broken in to key elements 

# scatter plot 
# add dimensions of plots 
# change attributes of plots 
# transformations 
# create different categorical dimensions 


ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(alpha = 0.1) # attribute: alpha is the transparency of the dot

#change of the lifeExp over time 
ggplot(data = gapminder, 
       mapping = aes(x = year, y =lifeExp, color = continent)) + 
  geom_point(alpha = 0.3) # attribute: alpha is the transparency of the dot

# geom_smooth(): line plots for discrete variables 
ggplot(data = gapminder, 
       mapping = aes(x = year, y =lifeExp, color = continent)) + 
  geom_smooth() 


#seperate line for country 
ggplot(data = gapminder, 
       mapping = aes(x = year, y =lifeExp, group = country, color = continent)) + 
  geom_smooth() 

ggplot(data = gapminder, 
       mapping = aes(x = year, y =lifeExp, group = country, color = continent)) + 
  geom_point() 

colnames(gapminder)

ggplot(data = gapminder, 
       mapping = aes(x = gdp, y = pop, group = country, color = continent)) + 
  geom_point(color = 'plum', alpha = 0.5)

ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(alpha = 0.7, color = 'skyblue') + 
  scale_x_log10() + 
  geom_smooth(method = 'lm', se = TRUE, linewidth = 0.8,  color = 'orange') # lm - linear model 


# ggplot and dplyr, adding facets 
gapminder %>% 
  filter(continent == "America") %>% 
  ggplot(data = gapminder, mapping = aes(x=year, y=lifeExp)) + 
  geom_smooth() + 
  facet_wrap(~ country) + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Year", y = "Life Expectancy", 
       title = "Figure 1: life Expectancy in the Americas")


# color sets the boundary, so here we need to change color to fill 
# bar charts 
# save the plot in data 
pop_continent_plot <-  ggplot(data = gapminder, mapping = aes(x = continent, y = pop, fill = continent)) + 
  geom_col()+
  scale_fill_brewer(palette = "Set3")

# save the plot to a file 
ggsave(filename = "continent_population.png", 
       plot = pop_continent_plot, width = 12, 
       height = 10, dpi = 300, units = "cm")















