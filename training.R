9 %/% 4
pi
pi^2

pi > 3

1:5 <3


'a' %in% LETTERS


"dog" <- 77

#equivalent to broom
rm(list=ls())

fifty <- 1:50

words <- c("blah", "bleurgh")

combined <- c(fifty, words)

incidences <- c(101, 500, 700)
ageweightings <- c(0.1, 1.2, 1.5)

incDF <- data.frame(incidences, ageweightings)
largeABC <-data.frame(id=(1:26), abc=letters)


my_dataframe <- data.frame(words=words, numbers=fifty)

lydiaslist <- list(dataframe1 = my_dataframe, somewords = words, somenumbers = fifty)


letters[letters < "x"]


iris
iris
iris[iris$Sepal.Width < mean(iris$Sepal.Width),]

iris[,-5]
myIris <- iris[1:100,]
myIris$Species <- "Unknown"
myIris <- myIris[myIris$Sepal.Length<=5.5,] 

cancerdata <- read_excel("~/myfirstproject/adultcancersurvivalcorrectionfinal.xls", 
                         sheet = "Table 5", skip = 2, na =":")
library(tidyverse)
library(DBI)
library(odbc)

dbConn<-dbConnect(odbc(),
                  driver="SQL Server",
                  server="rea-inf-dsql08",
                  database="cancerstatsr",
                  trusted_connection=TRUE)

incidence<-dbGetQuery(dbConn, 
                      "select *         
                      from [national].incidence i         
                      where baseyearkey=2016
                      and incidenceyearkey>=2000")

betterTblsList <-dbGetQuery(dbConn, "SELECT * FROM information_schema.tables")
betterTblsList

cancers <- dbGetQuery(dbConn, "SELECT * FROM dim.cancersite")

baseyear <- 2016
sqltorun <-sqlInterpolate(dbConn, "select TOP 100* 
               from [national].incidence
               where baseyearkey =?baseyear",
               baseyear=baseyear)
newincidences <- dbGetQuery(dbConn, sqltorun)

##################pipes#########################################
library(tidyverse)

incidence %>% 
  filter(IncidenceYearKey == 2000)

incidence >%>
  lm(IncidenceCount ~ IncidenceYearKey, data = . )

iris %>% colnames() %>%  toupper()

LETTERS %>% sub('S', 'Z', . ) -> LETTERZ

###to do 'or'
incidence %>% 
  filter(IncidenceYearKey == 2000 | GenderKey =='M',
         AgeRangeKey =="0509")

iris %>% filter(Sepal.Length < mean(iris$Sepal.Length) |
                Sepal.Width < mean(Sepal.Width))

iris %>% filter(Species != "setosa")

iris %>%  select(-Sepal.Length)

iris %>%  select(starts_with("S"))

iris %>%  select(starts_with("P"), everything())


###mutate to add column or change existing

iris %>% mutate(Sepal.Length = Sepal.Length/2.5,
                        Sepal.Width = Sepal.Width/2.5,
                        Petal.Length = Petal.Length/2.5,
                        Petal.Width = Petal.Width/2.5,
                Species = toupper(Species)) -> irisImperial

#programmaticaly
iris %>% 
  mutate_if(is.numeric, ~./2.5)

iris %>%
  summarise(min(Petal.Length), max(Petal.Length))

min(iris$Petal.Length)
max(iris$Petal.Length)

iris %>% 
  group_by(Species) %>% 
  mutate(speciesavgpetalwidth = mean(Petal.Width))

iris %>% 
  group_by_if(is.factor) %>% 
  summarise_all(mean)

iris %>% 
  mutate(belowavg = Sepal.Length < mean(Sepal.Length), "belowavg")

iris %>% 
  group_by_if(Sepal.Length < mean(Sepal.Length))


incidence %>% 
  filter(IncidenceYearKey ==2000) %>% 
  group_by(GenderKey, CancerSiteKey) %>% 
  summarise(IncidenceCount = sum(IncidenceCount)) %>% 
  spread(GenderKey, IncidenceCount)

incidence %>% 
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>% 
  group_by(AgeRangeKey, CountryKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  spread(CountryKey, IncidenceCount)

incidence %>% 
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>% 
  group_by(AgeRangeKey, CountryKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  group_by(CountryKey) %>% 
  mutate(Prop= IncidenceCount/sum(IncidenceCount)) %>% 
  select(-IncidenceCount) %>% 
  mutate(Prop= scales::percent(Prop)) %>% 
  spread(CountryKey, Prop)

incidence %>% 
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>% 
  group_by(AgeRangeKey, GenderKey, CountryKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>%
  tidyr::unite("Gender-Age", c("GenderKey", "AgeRangeKey")) %>% 
  spread(CountryKey, IncidenceCount) ->
  gender_age_country

gender_age_country %>% 
  gather(CountryKey, IncidenceCount, -'Gender-Age') %>% 
  separate('Gender-Age', c("GenderKey", "AgeRangeKey"))
  View()
  
  who %>%
    gather(starts_with("new") ,-iso2, -iso3, -year)
    
  
  
population %>%
  group_by(country) %>% 
  spread(year, population)
    
who %>% 
  gather(Measure, Value, -(country:year), na.rm = TRUE) %>% 
  separate(Measure, c("Type", "GenderAge"), extra="merge")
  
  group_by(Measure) %>% 
  summarise(n())

  
  
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>% 
  group_by(AgeRangeKey, CountryKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  group_by(CountryKey) %>% 
  mutate(Prop= IncidenceCount/sum(IncidenceCount)) %>% 
  select(-IncidenceCount) %>% 
  mutate(Prop= scales::percent(Prop)) %>% 
  spread(CountryKey, Prop)  
  
  incidence %>% 
    inner_join(cancers, by = c("CancerSiteKey" ="CancerSiteKey"))
  
iris %>%
  mutate(bAvg=Sepal.Length < mean(Sepal.Length)) %>% 
  group_by(Species, bAvg) %>%
  summarise_all(max)

iris %>%
  mutate(bAvg=Sepal.Length < mean(Sepal.Length)) %>% 
  group_by_if(~!is.numeric(.)) %>%
  summarise_all(max)

library(datasauRus)
library(ggplot2)

datasaurus_dozen %>% 
  filter(dataset == "dino") %>% 
  ggplot(aes(x=x, y=y, colour= dataset)) +
  geom_point() +
  theme_minimal()

datasaurus_dozen %>% 
  ggplot(aes(x=x, y=y, colour= dataset)) +
  geom_point() +
  facet_wrap(~dataset) +
  theme_minimal() +
  theme(legend.position = "none")

### %+% allows you to create a template and use it on new graphs



iris %>% 
  ggplot(aes(x = Sepal.Width, y = Sepal.Length, colour = Species))+
  geom_point()+
  theme_minimal()+
  labs(x= "Sepal Width (cm)", y = "Sepal Length (cm)",
       title = "Size Relationship", subtitle = "In cm, 150 observations")
  
incidence %>% 
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>% 
  inner_join(cancers,  by ="CancerSiteKey") %>% 
  group_by(CancerSiteLevel2Desc, AgeRangeKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  group_by(CancerSiteLevel2Desc) %>% 
  mutate(Prop=scales::percent(IncidenceCount/sum(IncidenceCount))) %>% 
  mutate(Prop=ifelse(Prop=='NaN%', "-%", Prop)) %>% 
  select(-IncidenceCount) %>% 
  spread(AgeRangeKey, Prop) %>%
  View()

library(forcats)

#position = fill turns bars into relative frequencies and everything goes to 100
incidence %>% 
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>% 
  inner_join(cancers,  by ="CancerSiteKey") %>% 
  group_by(CancerSiteLevel2Desc, AgeRangeKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  #mutate(AgeRangeKey =fct_reorder(AgeRangeKey, .desc))
  ggplot(aes(CancerSiteLevel2Desc, IncidenceCount, fill = fct_rev(AgeRangeKey)))+
  geom_col(position = "fill")+
  theme_minimal() +
  theme(legend.position = "none")+
  coord_flip()

incidence %>% 
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>% 
  inner_join(cancers,  by ="CancerSiteKey") %>% 
  group_by(CancerSiteLevel2Desc, AgeRangeKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>%
  ggplot(aes(x=AgeRangeKey, y=IncidenceCount))+
  geom_col(width = 1, fill = rgb(242, 76, 174, maxColorValue = 255))+
  facet_wrap(~CancerSiteLevel2Desc, scales="free")+
  coord_flip()+
  theme_void() 

->plot2

#ggplotly(plot1)

library(plotly)

#example transofrming plotly soruce code to workable code in r
iris %>% 
  plot_ly() %>% 
  add_markers(x=~Sepal.Length, y=~Sepal.Width, z=~Petal.Length, color =~Species)
  
#linear regression
iris %>%  lm(Sepal.Length~., data = .) %>% 
  fitted() -> lm.fit

#fitted gives you the specific predicions for the observations used 
#to create the model

iris %>%
  ggplot(aes(x=1:nrow(.), y = Sepal.Length)) +
  geom_point()+
  geom_point(aes(y=lm.fit), color = "red")+
  geom_abline()
  geom_point(aes())

iris %>% 
  lm(Sepal.Length~., data =.) %>% 
  plot()

iris %>% 
  ggplot(aes(x=Sepal.Length-lm.fit))+
  geom_histogram()+
  geom_histogram(aes(x=rnorm(150, mean =0, sd(Sepal.Length-lm.fit))), 
                 fill='red', alpha = .1)

iris %>% 
  lm(Sepal.Length~., data =.) %>% 
  predict(iris[5,])


##broom:glance() readily gives you AIC and BIC !!!!

iris %>% 
  lm(Sepal.Length~., data =.) ->
  lmmodel

lmmodel %>% 
  glance() %>% 
  gather(Measure, Value)

#augment automatically adds residual, fitted value etc.
lmmodel %>% 
  augment() %>% 
  ggplot(aes(x=.fitted, y=.std.resid))+
  geom_point()+
  geom_smooth(se=FALSE)+
  theme_minimal()

library(modelr)
iris %>% 
  sample_n(50) %>% 
  modelr::add_predictions(model=lmmodel)

incidence %>% 
  group_by(CancerSiteKey) %>% 
  lm(IncidenceCount ~CountryKey + AgeRangeKey + GenderKey)

###odds to logits or probs
library("optiRum")
logit.prob(1)

### fft for fast frugal trees

#function for sampling
incidence %>% 
  resample_partition(c("training"=0.7, "testing"=0.3)) ->
  incidence2

#must do as.data.frame to view in a normal format

library(caret)
confusionMatrix()

#how values distributed in relation to outcome variable
featurePlot()

library(PASWR)

titanic<- titanic3
titanic %>% 
  ggplot(aes(x=age))+
  geom_histogram() ->p

p
p + aes(x=sibsp)

titanic %>% 
  mutate(age= fct_explicit_na(quantile(age, breaks = 10))) %>%
  group_by(age) %>% 
  summarise(n(), mean(survived)) %>% 
  View()

install.packages("Hmisc")

titanic %>% 
  mutate(age= fct_explicit_na(cut2(age, g = 10))) %>%
  group_by(age) %>% 
  summarise(n(), mean(survived)) %>% 
  View()

#pre-process() from the caret package helps transform data
varImp()

mtcars %>% 
  mutate(car=rownames(.)) %>% 
  View()

#for gather you provide column for 

who %>% 
  gather(Measure, Value, -(country:year)) %>% 
  group_by(country, Measure) %>% 
  summarise(Avg=mean(Value, na.rm = TRUE)) %>% 
  mutate(Avg=ifelse(is.nan(Avg), -99, Avg))

library(modelr)
movies %>% 
  resample_partition(c(train =.7, test =.3)) ->
  movies_split

movies_split$train %>% 
  as_data_frame() %>% 
  mutate(year=year-min(year)) %>% 
  lm(rating ~ year + length + Action + Animation
     + Comedy + Drama + Documentary + Romance + Short,
     data = .) ->
  movies_ratings

movies_split$test %>% 
  as_data_frame %>% 
  mutate(year=year-min(as_data_frame(movies_split$train)$year)) %>% 
  add_predictions(model = movies_ratings) %>% 
  mutate(residuals= rating - pred) %>% 
  ggplot(aes(x=residuals)) +
  geom_density()

movies_split$test %>% 
  as_data_frame %>% 
  mutate(year=year-min(as_data_frame(movies_split$train)$year)) %>% 
  add_predictions(model = movies_ratings) %>% 
  mutate(residuals= rating - pred) %>% 
  filter(residuals<(-5)) %>% 
  View()

library(stringr)
simple <- "This IS HOrribly typed!"
numbers <- c("02", "11", "10", "1")
str_to_lower(simple)
str_sort(numbers, numeric = TRUE)

str_split(simple, boundary("word"))

who %>% 
  mutate_if(is_character, str_to_upper)

colnames(who) <- str_to_lower(colnames(who))

every_letter <- "the quick brown fox jumps over the lazy dog" 
every_letter %>% str_to_upper() %>% 
  str_split(boundary("word")) %>% 
  .[[1]] %>% 
  str_length()

library(forcats)
myFactor <- as.factor(c("red", "blue", "yellow", NA, "red"))
fct_count(myFactor)
myFactor %>% 
  

#fct_explicit_na change NA to missing

#fct_infreq changes axis to most common value
  
#fct_lump lumps infrequent categories together  
  ##***do explicit_na after as this fUnCtion doesn't apply to NAS
  
#EXAMPLE
myFactor %>% 
  fct_lump(n=1) %>% 
  fct_explicit_na() %>% 
  fct_count()

###fct_anon() function randomly seelcts numbers for factors

gss_cat %>% 
  group_by(year) %>% 
  summarise(sum = sum(year))

#####?????

gss_cat %>% 
  group_by(year) %>% 
  summarise(sum = sum())

%>% 
  
gss_cat$marital %>%   
  fct_lump(n =2) 


  
gss_cat %>%
  mutate(marital=fct_lump(marital, n=2),
           race=fct_anon(race),
           partyid=fct_anon(partyid),
           relig=fct_anon(relig))


  fct_anon()

library(glue)

age <- 40  
gender <- "Male"
location <- "England"
cancerlocation <- "Thyroid"
  
glue("The most common cancer for {ifelse (gender=='Female', 'women', 'men')} in {location} aged {round(age, 01)} to {round(age,-1)+10} is cancer of the {cancerlocation}.")  

cancers %>% 
  glue_data("The code for {CancerSiteLevel6Desc} is {CancerSiteKey}")

incidence %>% 
  group_by(IncidenceYearKey, CountryKey, AgeRangeKey, GenderKey) %>% 
  summarise(IncidenceCount = sum(IncidenceCount)) %>% 
  group_by(IncidenceYearKey, CountryKey, GenderKey) %>% 
  mutate(prop=IncidenceCount/sum(IncidenceCount)) %>% 
  glue_data("In {IncidenceYearKey}, for {GenderKey} aged {AgeRangeKey} in {CountryKey} there were {format(IncidenceCount, trim = TRUE)} incidences of cancer. This is {scales::percent(prop)} of all cancers {GenderKey} aged {AgeRangeKey} in {CountryKey} that year.")

install.packages("pivottabler")
library(pivottabler)

date <- "07/09/2017"
glue("Today's date is {date}")


pt <- PivotTable$new()
pt$addData(incidence)
pt$addRowDataGroups("AgeRangeKey")
pt$addColumnDataGroups("GenderKey")
pt$defineCalculation(calculationName = "Incidences", summariseExpression =  "sum(IncidenceCount)")
pt$renderPivot()

quick_table <-qhpvt(dataFrame = incidence,
      rows = c("AgeRangeKey"), 
      columns = c("CountryKey", "GenderKey"),
      calculations = "sum(IncidenceCount)"
      )

table_latex <- pt$getLatex()
writeLines(table_latex, "pivot.tex")

cruk <- list(
  headerBackgroundColor = '#f24cae',
  headerColor = "rgb(255, 255, 255)",
  cellBackgroundColor = "rgb(255, 255, 255)",
  cellColor = "#7f7f7f",
  totalBackgroundColor = "rgb(255, 255, 255)",
  totalColor = "#7f7f7f"
) 

theme <- getSimpleColoredTheme(pt, "cruk", colors=cruk, fontName = "Roboto, Arial")
pt$theme <-theme
pt$renderPivot()

s <- PivotStyle$new(pt, styleName = "cellHighlight", declarations=list("color"="red"))
g <-pt$getCells(specifyCellsAsList = TRUE,
                columnNumbers = 2)
gc <- lapply(g, function(cell) {cell$style <- s})
pt$renderPivot()

s <- PivotStyle$new(pt, styleName = "cellBold", declarations=list("font-weight"="bold"))
g <-pt$findCells(minValue = 456500)
gc <- lapply(g, function(cell) {cell$style <- s})
pt$renderPivot()

