---
  title: "Demographic Trend analysis on Suicide rate and it's relation with Happiness"
author: "Sai Vineeth K R"
date: "12/3/2019"
output:
  html_document:
  code_folding: hide
highlight: haddock
theme: readable
toc: yes
word_document:
  toc: yes
pdf_document:
  toc: yes

---
  
  # **Purpose**
  
  As a part of my visualization project work, I started to explore on suicide rates globally and the chance of it's relation with the happiness score(Data set is availble in the kaggle itself and has been attached to the data section for reference). 
   I worked as a data engineer with honeywell for two years, and currently am pursuing my masters in Data Analytics. [Please find my research work here](https://ieeexplore.ieee.org/document/7831697)
   
[I am always active on my linkedin](https://www.linkedin.com/in/vineeth-reddy-b1802499/)


# **Introduction**
      
  This project started with the paradox of "Suicide in happy places". The economists 'explanation for this paradox was that people tended to compare themselves to those around them—if you're an unhappy person in a happy place, your negative feelings might be exacerbated by your positive surroundings, which could lead to suicide'
The other key aspects of this report is to use high level packages available in R to build visualizations of suicide rates globally and explaining the thought process went behind in generating a suitable visualization.

```{r message = FALSE, warning = FALSE}
library(dplyr)
library(ggplot2)
library(gganimate)
library(gapminder)
library(countrycode)
library(ggplot2)
library(gganimate)
library(grid)
library(ggrepel)
library(scales)
library(ggcorrplot)
```
# **Data Cleansing and Transformation **
## **Data Cleansing**

* Generation Feature has been removed as it is merely serves the purpose of age feature and it has many conflicts within itself.
* Countries which have less propotion of data are removed 
* Human Development index is removed from suicide data set as there are many missing values.
* Age feature format was corrected throughout the data

## **Data Transformation**

* Continent was added to the dataset using the 'countrycode' package
* For the year 2015 Happiness and suicide data are merged and even HDI is included from happiness dataset.
* As we are maily dealing with the suicide rates we have eliminated the additional countries which are tagged from World happiness data.
* categorical variables are turned to factors.
```{r}
#Data Cleaning
data_s<- read.csv("/Users/vineeth/Desktop/master.csv") 
data_h<- read.csv("/Users/vineeth/Desktop/Happiness.csv") 
data_s<-data_s[-c(8,9)] # Drropping  columns which has missing values almost everywhere.

data_s<-data_s %>% 
  rename(
    Country = country,
    Year = year,
    Sex= sex,
    Age = age,
    Pop = population,
    S_N = suicides_no,
    GDP = gdp_for_year....,
    GDP_C = gdp_per_capita....,
    Gen = generation
  )

#Reordering age as aa Ordinal variable:
data_s$Age <- factor(data_s$Age,
                     levels = c("5-14 years", 
                                "15-24 years", 
                                "25-34 years", 
                                "35-54 years", 
                                "55-74 years", 
                                "75+ years"))

#We can see from data that 2016 column is mostly NA so filtering the same
data_s <- data_s %>%
  filter(Year != 2016)

# 3) TIDYING DATAFRAME
data_s$Age <- gsub(" years", "", data_s$Age)
data_s$Sex <- ifelse(data_s$Sex == "male", "Male", "Female")

# Adding continent to data:
data_s$Continent <- countrycode(sourcevar = data_s[, "Country"],
                                origin = "country.name",
                                destination = "continent")
```








```{r include=FALSE}
#Extracting 2015 data

S_2015<-data_s[ which(data_s$Year== 2015), ]

S_2015<- S_2015 %>%
  group_by(Country) %>%
  summarize(population = sum(Pop), 
            suicides = sum(S_N), 
            suicides_per_100k = (suicides / population) * 100000)


combine3 = merge(S_2015, data_h, by="Country", 
                 all.x = TRUE)
combine3$Continent <- countrycode(sourcevar = combine3[, "Country"],
                                  origin = "country.name",
                                  destination = "continent")

```

# **Trend Analysis**
## **World Trend**
```{r echo=FALSE}
worldwide_suc_avg<- (sum(as.numeric(data_s$S_N)) / sum(as.numeric(data_s$Pop))) * 100000

data_s %>%
  group_by(Year) %>%
  summarize(population = sum(Pop), 
            suicides = sum(S_N), 
            suicides_per_100k = (suicides / population) * 100000)%>%
  ggplot(aes(x = Year, y = suicides_per_100k)) + 
  geom_line(col = "red", size = 1) + 
  geom_point(col = "darkcyan", size = 2) + 
  geom_hline(yintercept = worldwide_suc_avg, linetype = 2, color = "green", size = 1) +
  labs(title = "World Suicides Rate",
       subtitle = "1985 - 2015.",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))+
  transition_reveal(as.numeric(Year))+theme_gray()

```
***Insights***
  
  * Througout the years the mean years are recorded with higher suicide rates
* The rate declines globally which is a positive sign

## **Gender-Sucide Trend Over the years**

```{r}
# Create tibble for sex so we can use it when creating our line plot.  
Gender_filter<- data_s %>%
  select(Year, Sex, S_N, Pop) %>%
  group_by(Year, Sex) %>%
  summarise(S_C = round((sum(S_N)/sum(Pop))*100000, 2))

Gender_filter$Sex<- as.factor(Gender_filter$Sex)

ggplot(Gender_filter, aes(x = Year, y = S_C)) + 
  geom_line(aes(color = Sex)) + 
  scale_color_manual(values = c("darkred", "steelblue"))+
  geom_hline(yintercept = worldwide_suc_avg, linetype = 2, color = "antiquewhite4", size = 1) +
  labs(title = "Global Suicides (per 100k)",
       subtitle = "Trend over time, 1985 - 2015.",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20)) +
  transition_reveal(as.numeric(Year))
```


```{r}
Age_time <- data_s %>%
  group_by(Year, Age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(S_N)) / sum(as.numeric(Pop))) * 100000)

#continent_time$Continent <- factor(continent_time$Continent, ordered = T, levels = data_s$Continent)

Age_time_plot <- ggplot(Age_time, aes(x = Year, y = suicide_per_100k, col = factor(Age))) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Age wise Sucide rate trend by year", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1985, 2015,4), minor_breaks = F) +
  transition_reveal(as.numeric(Year))
Age_time_plot+theme_dark()

```
***Insights***
  
  * Men are at higher risk through out the years and their trend is similar to the global trend.
* Proportion of fall or increase lies almost similar with men and women.


## **Multivariate Trend Analysis Over the years**

``` {r}
Global_data <- data_s %>%
  select(Year, S_N, Pop,Continent,GDP_C) %>%
  group_by(Year,Continent) %>%
  summarise(S_C = round((sum(S_N)/sum(Pop))*100000, 2), Pop = mean(Pop), GDP_C = mean(GDP_C)) 
Global_data$Continent <- as.factor(Global_data$Continent)

p <- ggplot(
  Global_data, 
  aes(x = Global_data$GDP_C, y=Global_data$S_C, size = Pop, col = factor(Continent)
  )) +
  geom_point() +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Sucide per capita")
p + transition_time(Global_data$Year) +
  labs(title = "Year: {frame_time}",
       size = "Population",
       color = "Continent") + shadow_wake(0.5)
```


***Insights***
  
  * Throughout the year we can notice the shadows during middle years are alarming but to the end we can see the decline in the suicide rate.
* We can see that population size and suicide rate is highly correlated.
* As we removed few countries from africa we can see the trend is affected and not appropriate.


## **Continents-Sucide Trend Over the years**

```{r}
continent_time <- data_s %>%
  group_by(Year, Continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(S_N)) / sum(as.numeric(Pop))) * 100000)

#continent_time$Continent <- factor(continent_time$Continent, ordered = T, levels = data_s$Continent)

continent_time_plot <- ggplot(continent_time, aes(x = Year, y = suicide_per_100k, col = factor(Continent))) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Continental Sucide rate trend by year", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Continent") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1985, 2015,4), minor_breaks = F) +
  transition_reveal(as.numeric(Year))
continent_time_plot + theme_dark()
```


## **Countries-Sucide Trend Over the years**

```{r}
country_time <- data_s %>%
  group_by(Year, Country) %>%
  summarize(suicide_per_100k = (sum(as.numeric(S_N)) / sum(as.numeric(Pop))) * 100000)

p3 <- ggplot(country_time,
             aes(x = Country,
                 y = suicide_per_100k)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6),
        axis.text.x = element_text(angle = 90))

p4 <- p3 + geom_point(aes(color = Year),
                      alpha = 0.5,
                      size = 1.5,
                      position = position_jitter(width = 0.25, height = 0))

p4 +
  scale_color_continuous(name="",
                         breaks = c(1985, 1995, 2005, 2015),
                         labels = c("'85", "'95", "'05","'15"),
                         low = muted("blue"), high = muted("red")) +transition_reveal(as.numeric(Year)) + shadow_trail() 
```
***Insights***
  
  *  Lithuania's rate has been highest by a large margin
*  We can notice the Europe and Americans Suicide rate is increasing linearly recently which is alarming.
* Most of the countries suicide rate is declining as we can see from their traces.
* As we removed few countries from africa we can see the trend is affected and not appropriate.

# **Relation between Suicide and Happiness**

## **Happiness vs Suicide Rate Over the years**

```{r}

combine4<-combine3[ which(combine3$Happiness.Rank < 21), ]

p <- ggplot(combine4, aes(Happiness.Rank, suicides_per_100k, fill = suicides_per_100k)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
p + transition_states(Happiness.Rank, wrap = FALSE) +
  shadow_mark()
combine3<-na.omit(combine3)
x<-cor(combine3$Happiness.Rank,combine3$suicides_per_100k)
```
## **Happiness vs Suicide Rate with the countries**
```{r}
pc1 <- ggplot(combine3, aes(x=Happiness.Score, y =suicides_per_100k, color = Continent ))

pc2 <- pc1 +
  geom_smooth(mapping = aes(linetype = "Trend line"),
              method = "lm",
              formula = y ~ x + log(x), se = FALSE,
              color = "red")
pc3 <- pc2 + geom_point(shape = 1, size = 2.5, stroke = 1.25)
pc4 <- pc3 +
   geom_text_repel(aes(label = Country),
                   color = "gray20",
                   force = 10)

pc5 <- pc4 +
  scale_x_continuous(name = "Happiness Score Index 2015",
                     limits = c(4, 8),
                     breaks = seq(4, 8,by = 0.5)) +
  scale_y_continuous(name = "Suicides per 100k population 2015",
                     limits = c(0, 25),
                     breaks = seq(1, 20,by = 2)) +
  ggtitle("Suicide and Happiness Index 2015")
pc5
```


***Insights***

* We retrieved the top 21 countries in Happiness rank and compared their suicide rate with the mean suicide rate.
* We can notice as the Happiness rank is increasing there is decline in suicide rate, but they are not enough to prove the paradox we assumed.
* Below we can find the feautures like population, corruption are linked with the suicide rate.
* We can see that most of the countries from Americans and Asia lie above the trend line which constitute the account of suicide rate more even though their happiness rank is less.


```{r}
combine3<-combine3[-c(1,5,16)] 
corr <- round(cor(combine3), 1)


ggcorrplot(corr, hc.order = TRUE, type = "lower")

```

