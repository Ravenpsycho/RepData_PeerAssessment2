---
title: "Storms"
subtitle: "A NOAA database analysis for Health and Economic consequences"
author: "Mathieu C."
date: "2 octobre 2019"
output: 
  html_document:
    keep_md: true
---

## Synopsis
The file "Storm Data" comes from the U.S. National Oceanic and Atmospheric  
Administration's (NOAA) and was accessible at [this url](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) on oct. 2nd 2019.  
  
The goal of the assessment is to produce a report that points at the most  
harmful and most costly meteorological events across the US for the years we  
have access to (1950 - 2011).  
  
More details can be found in the README.md file at [this GitHub url](https://github.com/Ravenpsycho/RepData_PeerAssessment2)

## Data Processing

### Data Loading  
We will begin by taking a look at the data:

```r
# Downloading file (47Mb, might take a while depending on the connection and speeds)
# note that this step is cached and will be executed much faster if repeated:
myurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!file.exists("Storm_Data.csv.bz2")){
        download.file(myurl, destfile = "Storm_Data.csv.bz2")
}
# Reading in the dataframe, again, might take a while.
if (!exists("StormData")){
        StormData <- read.csv("Storm_Data.csv.bz2")
}
str(StormData)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels ""," Christiansburg",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels ""," CANTON"," TULIA",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","%SD",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels "","\t","\t\t",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```
  
### Check for NA's
  
From the above call to `str()` function, the database seems pretty organized.  
Let's check for missing values:

```r
na_check <- lapply(StormData, is.na)
na_check <- lapply(na_check, sum)
sort(unlist(na_check), decreasing = T)
```

```
## COUNTYENDN          F   LATITUDE LATITUDE_E    STATE__   BGN_DATE 
##     902297     843563         47         40          0          0 
##   BGN_TIME  TIME_ZONE     COUNTY COUNTYNAME      STATE     EVTYPE 
##          0          0          0          0          0          0 
##  BGN_RANGE    BGN_AZI BGN_LOCATI   END_DATE   END_TIME COUNTY_END 
##          0          0          0          0          0          0 
##  END_RANGE    END_AZI END_LOCATI     LENGTH      WIDTH        MAG 
##          0          0          0          0          0          0 
## FATALITIES   INJURIES    PROPDMG PROPDMGEXP    CROPDMG CROPDMGEXP 
##          0          0          0          0          0          0 
##        WFO STATEOFFIC  ZONENAMES  LONGITUDE LONGITUDE_    REMARKS 
##          0          0          0          0          0          0 
##     REFNUM 
##          0
```
  
We can see that the database is an "all or nothing" type: either a variable  
has (almost) all it's values filled in, or almost all the data is missing.  
  
Fortunately, in the "missing" camp, we have only two variables `F` and  
`COUNTYENDN`. Since these variables don't seem to be relevant to the questions  
at hands, we won't take further actions in their regard.  
  
### Data modifications

This will require some packages, let's load them:

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(reshape2)
library(pdftools)
library(stringr)
library(stringdist)
```
  
Let's first create (...)
<<<<<<< HEAD

```r
testdate <- as.POSIXlt(
                as.character(StormData$BGN_DATE),
                format = "%m/%d/%Y %H:%M:%S") > "1996-01-01"
conditions <- (StormData$CROPDMG > 0 | StormData$PROPDMG > 0 |
        StormData$INJURIES > 0 | StormData$FATALITIES > 0) & testdate 
StormData_mod <- subset(StormData, subset = conditions)
StormData_mod <- StormData_mod[,c("EVTYPE","INJURIES", "FATALITIES", "CROPDMG", "CROPDMGEXP",
                              "PROPDMG", "PROPDMGEXP")]
```
  
After some exploration, we can notice something about `EVTYPE`. There are  
duplicates. For example: `THUNDERSTORM WIND` appears a whole lot of times under  
slightly different names.    
This is a problem. Events will need to be grouped under the official  
48 names below (found in the pdf file in code):  
  

```r
pdf_url <- paste0("https://d396qusza40orc.cloudfront.net/",
                  "repdata%2Fpeer2_doc%2Fpd01016005curr.pdf")
my_pdf <- 
        paste0(pdf_text(pdf_url)[c(2:4)])
my_str <- unlist(strsplit(my_pdf, "\\r\\n"))
my_str <- my_str[47:128]
my_str_short <- str_match(string = my_str, pattern = "7.([0-9]+)[.]? +(.*?) *(?:[(][A-Z][)])? *[.]{3,}")
my_str_short <- toupper(my_str_short[!is.na(my_str_short[,3]),3])
rm(my_pdf, my_str)
print(my_str_short)
```

```
## character(0)
```

After (*lots*) of trial and error, one of the main issues to match is the fact  
that some of the terms are abbreviated, like `TSTM` and `FLD`.  
  
<<<<<<< HEAD
We will try to fix them before the attempt. 
  
While we're at it, there's a few strings that start with `NON` something.  

We'll try to catch them as well.  


```r
patterns <- c("FLD", "NON-? ?\\w*", "TSTM", "URBAN/SML STREAM", "Ice jam flood [(]minor")
replacements <-  c("FLOOD", "", "THUNDERSTORM", "", "FLOOD")

for (i in 1:length(patterns)){
        StormData_mod$EVTYPE <- 
                gsub(patterns[i], replacements[i],
                     as.character(StormData_mod$EVTYPE), ignore.case = T)
}
StormData_mod$EVTYPE <- factor(StormData_mod$EVTYPE, levels = unique(StormData_mod$EVTYPE))
grep("STREAM FLOOD", StormData_mod$EVTYPE)
```

```
## integer(0)
```




```r
## The matching attempt:
matches <- amatch(toupper(StormData_mod$EVTYPE), my_str_short, method = "lv", maxDist = 16)
df_test_amatch <- data.frame(EVTYPE = StormData_mod$EVTYPE, 
                             RESULT = my_str_short[matches])
sum(is.na(df_test_amatch$RESULT))
```

```
## [1] 201313
```

```r
amatch_14 <- which(is.na(df_test_amatch$RESULT))
```


This is also a problem because it appears pretty high in the list of harmful  
and costly events. I will now modify them to gather all these events under a  
new name, `TSTM WIND COMBINED`.


## Data Analysis
  
Now we have the tools to create a new dataframe, with data grouped by `EVTYPE`
(event type) and we'll take a look at the highest means of fatalities and injuries.

```r
health_by_event <- StormData_mod %>%
        group_by(EVTYPE) %>%
        summarize(mean.fatalities = mean(FATALITIES, na.rm = T),
                  mean.injuries = mean(INJURIES, na.rm = T),
                  total.means = mean.fatalities + mean.injuries,
                  occurences = n()) %>%
        arrange(desc(total.means))
head(health_by_event, 10)
```

```
## # A tibble: 10 x 5
##    EVTYPE             mean.fatalities mean.injuries total.means occurences
##    <fct>                        <dbl>         <dbl>       <dbl>      <int>
##  1 Heat Wave                   0              70           70            1
##  2 WINTER WEATHER MIX          0              34           34            2
##  3 WINTRY MIX                  0.333          25.7         26            3
##  4 BLACK ICE                   1              24           25            1
##  5 HURRICANE/TYPHOON           0.889          17.7         18.6         72
##  6 GLAZE                       0.0714         15.1         15.2         14
##  7 COLD AND SNOW              14               0           14            1
##  8 SNOW SQUALL                 0.667          11.7         12.3          3
##  9 EXCESSIVE HEAT              2.62            9.33        12.0        685
## 10 TSUNAMI                     2.36            9.21        11.6         14
```
  
In our question we want to know how harmful an event is to population.  
My first thought was to sort the events by their total means (injuries  
and fatalities) combined as in the top 10 above.  
  
However, we first have to make sure that these events occur regularly to avoid  
putting too much weight into a deadly, harmful but also very unlikely event  
  
Let's see what the top most frequent events a.k.a. `occurences` looks like:

```r
head(health_by_event[order(health_by_event$occurences, decreasing = T),], 10)
```

```
## # A tibble: 10 x 5
##    EVTYPE            mean.fatalities mean.injuries total.means occurences
##    <fct>                       <dbl>         <dbl>       <dbl>      <int>
##  1 THUNDERSTORM WIND        0.00354         0.0480      0.0515     104872
##  2 HAIL                     0.000309        0.0314      0.0317      22679
##  3 FLASH FLOOD              0.0467          0.0881      0.135       19011
##  4 TORNADO                  0.122           1.67        1.79        12365
##  5 LIGHTNING                0.0583          0.371       0.430       11151
##  6 FLOOD                    0.0435          0.710       0.754        9514
##  7 HIGH WIND                0.0435          0.200       0.244        5402
##  8 STRONG WIND              0.0306          0.0826      0.113        3367
##  9 WINTER STORM             0.131           0.886       1.02         1459
## 10 HEAVY RAIN               0.0898          0.220       0.309        1047
```
This is interesting. We can see that some newcomers like `HAIL` or  
`TSTM WIND COMBINED` occur lots of times and they **are** harmful, not top  
10 harmful but still.  
  
Let's try creating an `index` variable. It will be the product of `occurences`  
by `total.means`.  

*Why?*  
  
Because this way, if an event is occuring a very low amount of time but has  
a very harmful outcome, it will be on par with a less harmful event that  
happens much more frequently. We will log(10) this index to avoid working  
with huge numbers.  
  
Let's do this and have a look:

```r
health_by_event <- transform(health_by_event, 
                         index = log((occurences * total.means), 10))
head(health_by_event[order(desc(health_by_event$index)),], 10)
```

```
##                EVTYPE mean.fatalities mean.injuries total.means occurences
## 42            TORNADO     0.122199757    1.67141124  1.79361100      12365
## 9      EXCESSIVE HEAT     2.623357664    9.32992701 11.95328467        685
## 89              FLOOD     0.043514820    0.71032163  0.75383645       9514
## 129 THUNDERSTORM WIND     0.003537646    0.04795370  0.05149134     104872
## 104         LIGHTNING     0.058290736    0.37126715  0.42955789      11151
## 121       FLASH FLOOD     0.046657198    0.08805428  0.13471148      19011
## 60       WINTER STORM     0.130911583    0.88553804  1.01644962       1459
## 11               HEAT     1.445121951    7.45121951  8.89634146        164
## 5   HURRICANE/TYPHOON     0.888888889   17.70833333 18.59722222         72
## 112         HIGH WIND     0.043502407    0.20048130  0.24398371       5402
##        index
## 42  4.345922
## 9   3.913178
## 89  3.855640
## 129 3.732394
## 104 3.680336
## 121 3.408410
## 60  3.171141
## 11  3.164055
## 5   3.126781
## 112 3.119915
```



```r
plotpop <- head(health_by_event[order(desc(health_by_event$index)),], 20)
plotpop$EVTYPE <- factor(plotpop$EVTYPE, levels = as.character(plotpop$EVTYPE))

warncolors <- colorRampPalette(c("red", "gold3"))
g1 <- ggplot(plotpop[c(1:6)],
             aes(x=EVTYPE, y = index, fill = EVTYPE))
g1 + geom_bar(stat = "identity", position = "dodge") +
        ylab("Health issues index")+
        xlab(element_blank())+
        theme(legend.position = "none")+
        scale_fill_manual(values = warncolors(20))+
        theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
        labs(title = paste0("Top 20 events in decreasing order of Health issues",
                            " index (HII).\n HII = mean of injuries + mean ",
                            " of fatalities * total occurences\nData for ",
                            "years 1996 to 2011"))
```

![](Storm_Data_Main_Report_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
  
Let's do the same with cost, using two new variables, `PROPDMG` and `CROPDMG`,  
holding numbers for *property damage* and *crop damage*, respectively, they  
first have to be ajusted with their exponent `EXP` counteparts:

```r
v_pattern <- c("[0-9]", "^$", "[+]", "[?-]", "[hH]", "[kK]", "[mM]", "[Bb]") 
v_repl <- c("10", "0", "1", "0", "100", "1000", "1000000", "1000000000")
## Modifying CROPDMGEXP
StormData_mod$CROPDMGEXP <- as.character(StormData_mod$CROPDMGEXP)
StormData_mod$PROPDMGEXP <- as.character(StormData_mod$PROPDMGEXP)

for (i in 1:8){
        StormData_mod$CROPDMGEXP <-
                gsub(v_pattern[i], v_repl[i], StormData_mod$CROPDMGEXP)
        StormData_mod$PROPDMGEXP <- 
                gsub(v_pattern[i], v_repl[i], StormData_mod$PROPDMGEXP)
} 
sum(!is.numeric(as.numeric(StormData_mod$CROPDMGEXP)))
```

```
## [1] 0
```

```r
sum(!is.numeric(as.numeric(StormData_mod$PROPDMGEXP)))
```

```
## [1] 0
```

```r
StormData_mod$CROPDMGEXP <- as.numeric(StormData_mod$CROPDMGEXP)
StormData_mod$PROPDMGEXP <- as.numeric(StormData_mod$PROPDMGEXP)
```





```r
cost_by_event <- StormData_mod %>%
        group_by(EVTYPE) %>%
        summarise(mean.prop = mean(PROPDMG * PROPDMGEXP, na.rm = T),
                  mean.crop = mean(CROPDMG * CROPDMGEXP, na.rm = T),
                  mean.combi = mean.crop + mean.prop,
                  occurences = n(),
                  index = log(mean.combi*occurences, 10)) %>%
        arrange(desc(index))
head(cost_by_event[order(desc(cost_by_event$mean.combi)),])
```

```
## # A tibble: 6 x 6
##   EVTYPE             mean.prop  mean.crop mean.combi occurences index
##   <fct>                  <dbl>      <dbl>      <dbl>      <int> <dbl>
## 1 HURRICANE/TYPHOON 962581111. 36220456.  998801567.         72 10.9 
## 2 STORM SURGE       255583053.       29.6 255583083.        169 10.6 
## 3 HURRICANE          93752532. 21757222.  115509754.        126 10.2 
## 4 STORM SURGE/TIDE   98748681.    18085.   98766766.         47  9.67
## 5 TYPHOON            66692222.    91667.   66783889.          9  8.78
## 6 DROUGHT             4054655. 51812271.   55866926.        258 10.2
```
If we arrange the data by `mean.combi` (as seen above), we can see that the  
most costly events have a very low occurence. Since the goal of this analysis  
is to produce a report supposedly for preparation towards events, the formerly  
used method of creating an index to weight events by `occurences` seems about  
right.

```r
head(cost_by_event, 20)
```

```
## # A tibble: 20 x 6
##    EVTYPE             mean.prop  mean.crop mean.combi occurences index
##    <fct>                  <dbl>      <dbl>      <dbl>      <int> <dbl>
##  1 FLOOD              15129791.   522890.   15652682.       9514 11.2 
##  2 HURRICANE/TYPHOON 962581111. 36220456.  998801567.         72 10.9 
##  3 STORM SURGE       255583053.       29.6 255583083.        169 10.6 
##  4 TORNADO             1990854.    22922.    2013775.      12365 10.4 
##  5 HAIL                 643553.   109177.     752730.      22679 10.2 
##  6 FLASH FLOOD          800705.    70217.     870922.      19011 10.2 
##  7 HURRICANE          93752532. 21757222.  115509754.        126 10.2 
##  8 DROUGHT             4054655. 51812271.   55866926.        258 10.2 
##  9 THUNDERSTORM WIND     74955.     9080.      84035.     104872  9.95
## 10 TROPICAL STORM     18640184.  1652954.   20293138.        410  9.92
## 11 HIGH WIND            971466.   117283.    1088749.       5402  9.77
## 12 WILDFIRE            5618261.   348846.    5967107.        847  9.70
## 13 STORM SURGE/TIDE   98748681.    18085.   98766766.         47  9.67
## 14 ICE STORM           5772185.    24818.    5797003.        631  9.56
## 15 WILD/FOREST FIRE    7878694.   280269.    8158963.        381  9.49
## 16 WINTER STORM        1050537.     8186.    1058723.       1459  9.19
## 17 HEAVY RAIN           558610.   695482.    1254092.       1047  9.12
## 18 EXTREME COLD         120490.  7859591.    7980082.        164  9.12
## 19 FROST/FREEZE          81724.  9431776.    9513500         116  9.04
## 20 LIGHTNING             66638.      619.      67256.      11151  8.88
```


```r
plotpop2 <- head(cost_by_event[order(desc(cost_by_event$index)),], 20)
plotpop2$EVTYPE <- factor(plotpop2$EVTYPE, levels = as.character(plotpop2$EVTYPE))

prettycolors2 <- colorRampPalette(c("darkgreen", "springgreen"))
g2 <- ggplot(plotpop2[c(1:6)],
             aes(x=EVTYPE, y = index, fill = EVTYPE))
g2 + geom_bar(stat = "identity", position = "dodge") +
        ylab("Cost Index")+
        xlab(element_blank())+
        theme(legend.position = "none")+
        scale_fill_manual(values = prettycolors2(20))+
        theme(axis.text.x = element_text(angle = 65, hjust = 0.95)) +
        labs(title = paste0("Top 20 events in decreasing order of Cost",
                            " Index (CI).\n CI = mean of propriety damage +",
                            " mean of crop damage ",
                            "* total occurences\nData for ",
                            "years 1996 to 2011"))
```

![](Storm_Data_Main_Report_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
  
I will rest my case here. Further steps should be taken to be able to consider  
this work for anything else than a simple assessment in the JH Datascience  
course:  
  
* In depth cleaning of the `EVTYPE` variable to ensure the best merge of duplicates.
* Regional repartition to further affinate the specific risk potential.
* Ajusting the index, especially for health, to reflect the true impact on  
a community
