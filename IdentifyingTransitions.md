Identifying Neighborhood Transitions
================
Treva Tam
2/6/2021

    rm(list=ls())
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(data.table)

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

**Research Question:** *What neighborhood patterns of Racial-Ethnic
change have occured in the South from 1990 to 2010?*

In this project I will:

-   [Create a sample](#sampling) of metros based on set criteria
-   [Classify census tracts](#classification) within metros based on
    their racial-ethnic composition
-   Identify the [neighborhood transition
    pathways](#neighborhood-transition-pathways) from 1990 to 2000
    to 2010.

<br>

### **Data**

I use 1990, 2000, and 2010 Census data drawn from Social Explorer.

### **Sampling**

*Which southern metropolitan areas do we include? What is the criteria
of inclusion?*

<br> Pull in “AllSouthernMSA\_Race.csv.” Drawn from the 2010 Census
data, this data set includes all metro areas in the Southern region.
Remove micropolitan areas and Philadelphia (which is considered a
“traditional gateway” of immigration).

    data <- read.csv("AllSouthernMSA_Race.csv", fileEncoding = 'UTF-8-BOM')

    data <- data%>%
      filter(!grepl('Micro', NAME)) #remove micropolitan areas
    data <- data %>%
      filter(!grepl('Philadelphia',NAME)) #remove these "traditional gateways" of immigration

Since I am interested in Southern metros that experienced a large and
rapid increase in Latinx and Asian residents, I want to narrow my sample
to those that currently have a large immigrant population. I sample
Southern MSAs that, in 2010, had at least one immigrant group greater
than or equal to the regional average share (e.g. )

First, I find the percentage of each racial category in the whole
Southern region using population numbers from Social Explorer.

    a <- 68706462/114555744 #60.0% White
    b <- 21578475/114555744 #18.84% Black
    c <- 18227508/114555744 #15.91% Latinx
    d <- 3170814/114555744 #2.77% Asian

Using these percentages, I create a sample dataset that contains the 22
metro areas that fit my criteria.

    # create dummy variable for metros that fit my criteria using "if else" statement
    data <- data %>%
      mutate(sample.dum = ifelse(Latinx/TotPop >= c|
                                   Asian/TotPop >= d, 1, 0))

    # create a separate dataset that contains only these metro areas
    sample <- data %>%
      filter(sample.dum==1) %>%
      select(Geo_CBSA, FIPS, STATE, NAME, TotPop, Latinx, Asian)
    names(data)

    ##  [1] "FIPS"          "NAME"          "QNAME"         "STATE"        
    ##  [5] "Geo_SUMLEV"    "Geo_LOGRECNO"  "REGION"        "DIVISION"     
    ##  [9] "STATEFIPS"     "Geo_CBSA"      "CBSA_Sizecode" "CSA"          
    ## [13] "Geo_AREALAND"  "Geo_AREAWATR"  "HousUnit"      "Geo_INTPTLAT" 
    ## [17] "Geo_INTPTLON"  "Geo_LSADC"     "Geo_PARTFLAG"  "Geo_STATENS"  
    ## [21] "Geo_CSASC"     "Geo_MEMI"      "TotPop"        "White"        
    ## [25] "Black"         "AIAN"          "Asian"         "NHPI"         
    ## [29] "Latinx"        "NatB"          "FB"            "sample.dum"

    list(sample$Geo_CBSA) #list out the CBSA (identifying numbers)

    ## [[1]]
    ##  [1] 22900 47900 15980 18880 23540 27260 29460 33100 34940 36740 45300 12020
    ## [13] 12060 19140 23580 12580 47900 16740 20500 24660 39580 36420 10180 11100
    ## [25] 12420 15180 17780 18580 19100 21340 26420 28660 29700 31180 32580 33260
    ## [37] 36220 41660 41700 46340 47020 47380 13980 16820 40060 47260 47900

The next methodological decision is how to classify census tracts within
each metropolitan region in terms of the specific combination of groups
that are present in them.

<br>

### **Classification**

*How do I classify neighborhoods (tracts) within each of metro? What is
the exact criteria to delineate the conceptual racial-ethnic categories
(i.e. how White is an “all White” neighborhood)?*

<br> Since the data set I created in the previous section is at the
metro-level, I need to create datasets that are at the
neighborhood-level. I will walk through my steps for the 2010 census
tracts, but include the code that I used to pull all three decades
(1990, 2000, and 2010)

I pull in *all* US tracts with race variables (Total population size,
non-Latinx White, non-Latinx Black, and Latinx) in 2010.

From this, I filter my dataset usning the list of CBSA’s that we created
before.

    #getting 2010 census tracts using the identified CBSAs
    data2010 <- read.csv("UStracts_Race2010.csv")
    data2010 <- data2010 %>%
      filter(Geo_CBSA %in% 
               c('22900', '47900', '15980', '18880', '23540', 
                 '27260', '29460', '33100', '34940', '36740',
                 '45300', '12020', '12060', '19140', '23580', 
                 '12580', '16740', '20500', '24660', '39580', 
                 '36420', '10180', '11100', '12420', '15180', 
                 '17780', '18580', '19100', '21340', '26420', 
                 '28660', '29700', '31180', '32580', '33260', 
                 '36220', '41660', '41700', '46340', '47020',
                 '47380', '13980', '16820', '40060', '47260'))%>%
      select("Geo_FIPS","Geo_NAME","Geo_QName","Geo_REGION","Geo_DIVISION",
             "Geo_STATE","Geo_COUNTY", "Geo_TRACT", "Geo_CBSA",
             "TotPop", "White", "Black", "AIAN", "Asian", "NHPI", "Latinx")

<br>

**Classification Criteria**

Following Logan and Zhang (2010), I use a percentage criterion that also
takes into account the overall racial-ethnic composition of the
metropolitan areas in my sample. I use as a reference point the
percentage of each group in the overall population of the 22
metropolises in each year (1990, 2000, and 2010). Allowing the reference
point to shift over time responds to the rapid growth of Latinx and
Asian populations.

I create variables identifying the overall share of each racial-ethnic
group in my sample in 2010.

    w.avg.2010 <- sum(data2010$White)/sum(data2010$TotPop)
    b.avg.2010 <- sum(data2010$Black)/sum(data2010$TotPop)
    l.avg.2010 <- sum(data2010$Latinx)/sum(data2010$TotPop)
    a.avg.2010 <- sum(data2010$Asian)/sum(data2010$TotPop)

Logan and Zhang (2010) use a 25% criterion to label a racial-ethnic
group as “present.” However, I decide to adopt a 50% criterion. I
consider a tract to have a substantial presence of a group if the share
of residents in that tract is at least 50% of their share in the
aggregate population. For example, neighborhoods in 2010, were coded as
having a “substantial presence” of Asians if Asians were at least 2.54%
of the tract since Asians made up 5.07% of the aggregate population.
Using this criteria, I created indicator variables for White, Black,
Latinx, and Asian presence.

    #create dummy variables identifying when a tract has significant presence of a racial-ethnic group
    df2010.50 <- data2010 %>%
      mutate(year = 2010,
             W.type = ifelse(White/TotPop >= .50*(w.avg.2010),1,0),
             B.type = ifelse(Black/TotPop >= .50*(b.avg.2010),1,0),
             L.type = ifelse(Latinx/TotPop >= .50*(l.avg.2010),1,0),
             A.type = ifelse(Asian/TotPop >= .50*(a.avg.2010),1,0))

    df2010.50 <- na.omit(df2010.50)

    # Use identifier dummies to categorize the "type" of neighborhood
    df2010.50 <- df2010.50 %>%
      mutate(W = ifelse(W.type == 1 & B.type == 0 & A.type == 0 & L.type == 0,1,0),
             B = ifelse(W.type == 0 & B.type == 1 & A.type == 0 & L.type == 0,1,0),
             L = ifelse(W.type == 0 & B.type == 0 & A.type == 0 & L.type == 1,1,0),
             A = ifelse(W.type == 0 & B.type == 0 & A.type == 1 & L.type == 0,1,0),
             WB = ifelse(W.type == 1 & B.type == 1 & A.type == 0 & L.type == 0,1,0),
             WA = ifelse(W.type == 1 & B.type == 0 & A.type == 1 & L.type == 0,1,0),
             WL = ifelse(W.type == 1 & B.type == 0 & A.type == 0 & L.type == 1,1,0),
             BA = ifelse(W.type == 0 & B.type == 1 & A.type == 1 & L.type == 1,1,0),
             BL = ifelse(W.type == 0 & B.type == 1 & A.type == 0 & L.type == 1,1,0),
             AL = ifelse(W.type == 0 & B.type == 0 & A.type == 1 & L.type == 1,1,0),
             WBA = ifelse(W.type == 1 & B.type == 1 & A.type == 1 & L.type == 0,1,0),
             WBL = ifelse(W.type == 1 & B.type == 1 & A.type == 0 & L.type == 1,1,0),
             WLA = ifelse(W.type == 1 & B.type == 0 & A.type == 1 & L.type == 1,1,0),
             BLA = ifelse(W.type == 0 & B.type == 1 & A.type == 1 & L.type == 1,1,0),
             WBLA = ifelse(W.type == 1 & B.type == 1 & A.type == 1 & L.type == 1,1,0),
             Ntype = ifelse(W == 1, "W", ifelse(B ==1, "B", ifelse(L ==1, "L", ifelse(A ==1, "A",
                     ifelse(WB ==1, "WB", ifelse(WL ==1, "WL", ifelse(WA ==1, "WA", 
                     ifelse(BL ==1, "BL", ifelse(BA ==1, "BA", 
                     ifelse(AL ==1, "LA",
                     ifelse(WBL ==1, "WBL", ifelse(WBA==1, "WBA", ifelse(WLA ==1, "WLA",
                     ifelse(BLA==1, "BLA","WBLA")))))))))))))))

Let’s check how many of each type of neighborhood were present in 2010.

    df2010.50 %>%
           group_by(Ntype) %>%
           summarise(n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 14 x 2
    ##    Ntype `n()`
    ##    <chr> <int>
    ##  1 A         1
    ##  2 B       740
    ##  3 BA      472
    ##  4 BL      674
    ##  5 L       916
    ##  6 LA      147
    ##  7 W      1548
    ##  8 WA     1559
    ##  9 WB      875
    ## 10 WBA    1259
    ## 11 WBL     648
    ## 12 WBLA   1522
    ## 13 WL     1131
    ## 14 WLA    1036

<br>

Do the same for tracts in 2000 and 1990:

<br>

    #2000 Neighborhood Type Classification####

    data2000 <- read.csv("Sample2000.2.csv")
    data2000 <- data2000 %>%
      select("Geo_FIPS","Geo_NAME","Geo_QName", "Geo_STATE","Geo_COUNTY", "Geo_CT",
             "TotPop", "White", "Black", "AIAN", "Asian", "NHPI", "Latinx")
    data2000 <- na.omit(data2000)

    w.avg.2000 <- sum(data2000$White)/sum(data2000$TotPop)
    b.avg.2000 <- sum(data2000$Black)/sum(data2000$TotPop)
    l.avg.2000 <- sum(data2000$Latinx)/sum(data2000$TotPop)
    a.avg.2000 <- sum(data2000$Asian)/sum(data2000$TotPop)

    df2000.50 <- data2000 %>%
      mutate(year = 2000,
             W.type = ifelse(White/TotPop >= .50*(w.avg.2000),1,0),
             B.type = ifelse(Black/TotPop >= .50*(b.avg.2000),1,0),
             L.type = ifelse(Latinx/TotPop >= .50*(l.avg.2000),1,0),
             A.type = ifelse(Asian/TotPop >= .50*(a.avg.2000),1,0))

    df2000.50 <- na.omit(df2000.50)

    df2000.50 <- df2000.50 %>%
      mutate(W = ifelse(W.type == 1 & B.type == 0 & A.type == 0 & L.type == 0,1,0),
             B = ifelse(W.type == 0 & B.type == 1 & A.type == 0 & L.type == 0,1,0),
             L = ifelse(W.type == 0 & B.type == 0 & A.type == 0 & L.type == 1,1,0),
             A = ifelse(W.type == 0 & B.type == 0 & A.type == 1 & L.type == 0,1,0),
             WB = ifelse(W.type == 1 & B.type == 1 & A.type == 0 & L.type == 0,1,0),
             WA = ifelse(W.type == 1 & B.type == 0 & A.type == 1 & L.type == 0,1,0),
             WL = ifelse(W.type == 1 & B.type == 0 & A.type == 0 & L.type == 1,1,0),
             BA = ifelse(W.type == 0 & B.type == 1 & A.type == 1 & L.type == 1,1,0),
             BL = ifelse(W.type == 0 & B.type == 1 & A.type == 0 & L.type == 1,1,0),
             AL = ifelse(W.type == 0 & B.type == 0 & A.type == 1 & L.type == 1,1,0),
             WBA = ifelse(W.type == 1 & B.type == 1 & A.type == 1 & L.type == 0,1,0),
             WBL = ifelse(W.type == 1 & B.type == 1 & A.type == 0 & L.type == 1,1,0),
             WLA = ifelse(W.type == 1 & B.type == 0 & A.type == 1 & L.type == 1,1,0),
             BLA = ifelse(W.type == 0 & B.type == 1 & A.type == 1 & L.type == 1,1,0),
             WBLA = ifelse(W.type == 1 & B.type == 1 & A.type == 1 & L.type == 1,1,0),
             Ntype = ifelse(W == 1, "W", ifelse(B ==1, "B", ifelse(L ==1, "L", ifelse(A ==1, "A",
                     ifelse(WB ==1, "WB", ifelse(WL ==1, "WL", ifelse(WA ==1, "WA", 
                     ifelse(BL ==1, "BL", ifelse(BA ==1, "BA", 
                     ifelse(AL ==1, "LA",
                     ifelse(WBL ==1, "WBL", ifelse(WBA==1, "WBA", ifelse(WLA ==1, "WLA",
                     ifelse(BLA==1, "BLA","WBLA")))))))))))))))

    #1990 Neighborhood Type Classification####
    data1990 <- read.csv("Sample1990.2.csv")
    data1990 <- data1990 %>%
      select("Geo_FIPS","Geo_NAME","Geo_QName", "Geo_STATE","Geo_COUNTY", "Geo_CT",
             "TotPop", "White", "Black", "AIAN", "Asian", "Latinx")
    data1990 <- na.omit(data1990)

    w.avg.1990 <- sum(data1990$White)/sum(data1990$TotPop)
    b.avg.1990 <- sum(data1990$Black)/sum(data1990$TotPop)
    l.avg.1990 <- sum(data1990$Latinx)/sum(data1990$TotPop)
    a.avg.1990 <- sum(data1990$Asian)/sum(data1990$TotPop)

    df1990.50 <- data1990 %>%
      mutate(year = 1990,
             W.type = ifelse(White/TotPop >= .50*(w.avg.1990),1,0),
             B.type = ifelse(Black/TotPop >= .50*(b.avg.1990),1,0),
             L.type = ifelse(Latinx/TotPop >= .50*(l.avg.1990),1,0),
             A.type = ifelse(Asian/TotPop >= .50*(a.avg.1990),1,0))

    df1990.50 <- na.omit(df1990.50)

    df1990.50 <- df1990.50 %>%
      mutate(W = ifelse(W.type == 1 & B.type == 0 & A.type == 0 & L.type == 0,1,0),
             B = ifelse(W.type == 0 & B.type == 1 & A.type == 0 & L.type == 0,1,0),
             L = ifelse(W.type == 0 & B.type == 0 & A.type == 0 & L.type == 1,1,0),
             A = ifelse(W.type == 0 & B.type == 0 & A.type == 1 & L.type == 0,1,0),
             WB = ifelse(W.type == 1 & B.type == 1 & A.type == 0 & L.type == 0,1,0),
             WA = ifelse(W.type == 1 & B.type == 0 & A.type == 1 & L.type == 0,1,0),
             WL = ifelse(W.type == 1 & B.type == 0 & A.type == 0 & L.type == 1,1,0),
             BA = ifelse(W.type == 0 & B.type == 1 & A.type == 1 & L.type == 1,1,0),
             BL = ifelse(W.type == 0 & B.type == 1 & A.type == 0 & L.type == 1,1,0),
             AL = ifelse(W.type == 0 & B.type == 0 & A.type == 1 & L.type == 1,1,0),
             WBA = ifelse(W.type == 1 & B.type == 1 & A.type == 1 & L.type == 0,1,0),
             WBL = ifelse(W.type == 1 & B.type == 1 & A.type == 0 & L.type == 1,1,0),
             WLA = ifelse(W.type == 1 & B.type == 0 & A.type == 1 & L.type == 1,1,0),
             BLA = ifelse(W.type == 0 & B.type == 1 & A.type == 1 & L.type == 1,1,0),
             WBLA = ifelse(W.type == 1 & B.type == 1 & A.type == 1 & L.type == 1,1,0),
             Ntype = ifelse(W == 1, "W", ifelse(B ==1, "B", ifelse(L ==1, "L", ifelse(A ==1, "A",
             ifelse(WB ==1, "WB", ifelse(WL ==1, "WL", ifelse(WA ==1, "WA", 
             ifelse(BL ==1, "BL", ifelse(BA ==1, "BA", 
             ifelse(AL ==1, "LA",
             ifelse(WBL ==1, "WBL", ifelse(WBA==1, "WBA", ifelse(WLA ==1, "WLA",
             ifelse(BLA==1, "BLA","WBLA")))))))))))))))

You can check to see how many of each type of neighborhood was present
in each year.

    df1990.50 %>%
      group_by(Ntype) %>%
      summarise(n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 13 x 2
    ##    Ntype `n()`
    ##    <chr> <int>
    ##  1 B       695
    ##  2 BA      160
    ##  3 BL      279
    ##  4 L       611
    ##  5 LA       89
    ##  6 W      2374
    ##  7 WA     2210
    ##  8 WB     1088
    ##  9 WBA    1394
    ## 10 WBL     294
    ## 11 WBLA    981
    ## 12 WL      947
    ## 13 WLA    1418

    df2000.50 %>%
      group_by(Ntype) %>%
      summarise(n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 13 x 2
    ##    Ntype `n()`
    ##    <chr> <int>
    ##  1 B       745
    ##  2 BA      352
    ##  3 BL      457
    ##  4 L       755
    ##  5 LA      126
    ##  6 W      1962
    ##  7 WA     1926
    ##  8 WB      934
    ##  9 WBA    1286
    ## 10 WBL     467
    ## 11 WBLA   1307
    ## 12 WL     1025
    ## 13 WLA    1193

<br>

### **Neighborhood Transition Pathways**

Now that I’ve classified each tract in each census year, I want to
combine the neighborhood types into one dataset to chart the
neighborhood pathways from decade to decade.

I merge the datasets and create a “long” dataset.

    df1990.50 <- as.data.table(df1990.50)
    df2000.50 <- as.data.table(df2000.50)
    df2010.50 <- as.data.table(df2010.50)

    dfLong <- rbind(df1990.50,df2000.50,df2010.50, fill=TRUE)

Then omit tracts that have no population or are marked as special land
use. These include tracts numbered 9800-9899 that identify places such
as large parks or employment areas.

    dfLong <- dfLong %>%
      filter(!grepl('980', Geo_NAME))

    #Census tracts with no population in at least one decade
    dfLong <- dfLong %>%
      filter(!Geo_FIPS %in% 
               c(12086003705, 12086006709, 12086009043, 12086009044, 12086009046,
                 12086009047, 12086011008, 12086011009, 12099007835, 12101032106,
                 37063001503, 48085031657, 51059481101, 11001006202, 12086009040,
                 48339691700, 48201455300, 13121003700))

    dfLong <- as.data.table(dfLong)

Rename the column names and create a “wide” dataset for easier viewing
of the pathways.

    cols <- c("Ntype", "TotPop", "White", "Black", "Asian", "Latinx")
    dfWide <- dcast(dfLong, Geo_FIPS+Geo_NAME+Geo_QName~year, value.var=c(cols))

In another project, I will use this dataset to map out neighborhood
transitions in certain metro areas more closely!

The following code allows me see the number of certain transition
pathways from decade to decade:

    library(readxl)
    library(writexl)

    #Viewing the pathways####
    View(dfWide %>%
           group_by(Ntype_1990) %>%
           summarise(n()))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    Path1 <- dfWide %>%
           group_by(Ntype_1990, Ntype_2000) %>%
           summarise(n())

    ## `summarise()` regrouping output by 'Ntype_1990' (override with `.groups` argument)

    Path1

    ## # A tibble: 116 x 3
    ## # Groups:   Ntype_1990 [13]
    ##    Ntype_1990 Ntype_2000 `n()`
    ##    <chr>      <chr>      <int>
    ##  1 B          B            584
    ##  2 B          BA             9
    ##  3 B          BL            50
    ##  4 B          WB             7
    ##  5 B          WBA            1
    ##  6 B          WBLA          42
    ##  7 BA         B              5
    ##  8 BA         BA            76
    ##  9 BA         BL            54
    ## 10 BA         L              2
    ## # ... with 106 more rows

    #write_xlsx(Path1, 'Analysis_Pt2/Pathtemp.xlsx')

    Path2 <- dfWide %>%
      group_by(Ntype_1990, Ntype_2000, Ntype_2010) %>%
      summarise(n())

    ## `summarise()` regrouping output by 'Ntype_1990', 'Ntype_2000' (override with `.groups` argument)

    Path2

    ## # A tibble: 576 x 4
    ## # Groups:   Ntype_1990, Ntype_2000 [116]
    ##    Ntype_1990 Ntype_2000 Ntype_2010 `n()`
    ##    <chr>      <chr>      <chr>      <int>
    ##  1 B          B          B            487
    ##  2 B          B          BL            46
    ##  3 B          B          WA             1
    ##  4 B          B          WB            20
    ##  5 B          B          WBA           12
    ##  6 B          B          WBL            1
    ##  7 B          B          WBLA          17
    ##  8 B          BA         B              3
    ##  9 B          BA         BL             6
    ## 10 B          BL         B              7
    ## # ... with 566 more rows

    #write_xlsx(Path2, 'Analysis_Pt2/Pathtemp.xlsx')

Further projects will create a heatmap of these transitions as well as
run a multinomial logistic regression to understand what predicts
certain neighborhood transitions.
