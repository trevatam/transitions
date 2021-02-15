Descriptive Statistics
================
Treva Tam
11/26/2019

<br> Read in the data we created in “SampleClassify”.

    dfLong <- readRDS("data/dfLong.rds")
    dfWide <- readRDS("data/dfWide.rds")

Create a transition matrix to create tables with.

    #replace neighborhood types with numbers
    dfLong2 <- dfLong %>%
      mutate(Ntype = recode(Ntype,
                            "W" = 1, 
                            "B" = 2, 
                            "L" = 3, 
                            "A" = 4,
                            "WB" = 5, 
                            "WL" = 6, 
                            "WA" = 7, 
                            "BL" = 8, 
                            "BA" = 9, 
                            "LA" = 10,
                            "WBL" = 11, 
                            "WBA" = 12, 
                            "WLA" = 13, 
                            "BLA"= 14,
                            "WBLA" = 15))
    #collapse categories - L,A,LA (all immigrant - 10); WL, WA (white single immigrant - 7); BL, BA (black single immigrant -9); WBL, WBA (semi-global - 12)
    dfLong2 <- dfLong2 %>%
      mutate(Ntype = recode(Ntype,
                            "3" = 10,
                            "4" = 10,
                            "6" = 7,
                            "7" = 7,
                            "8" = 9,
                            "9" = 9,
                            "11" = 12,
                            "12" = 12))
    dfLong2 <- as.data.table(dfLong2)
    cols <- c("Ntype", "TotPop", "White", "Black", "Asian", "Latinx")
    dfWide2 <- dcast(dfLong2, Geo_FIPS+Geo_QName+Geo_STATE~year, value.var=c(cols))

    #%>%
    #filter(!Ntype_1990 %in% c(4), !Ntype_2000 %in% c(4),!Ntype_2010 %in% c(4))
      
    #filter(!Ntype_1990 %in% c(3,4,10), !Ntype_2000 %in% c(3,4,10),!Ntype_2010 %in% c(3,4,10))

    matrix1 <- dfWide2 %>%
      group_by(Ntype_1990)%>%
      count(Ntype_2000)
    #%>%mutate(Percent = n/sum(n))
    matrix1 <- matrix1 %>%
      rename("starttype" = Ntype_1990, "endtype" = Ntype_2000, "freq" = n) %>%
      mutate(year = 1)

    matrix2 <- dfWide2 %>%
      group_by(Ntype_2000)%>%
      count(Ntype_2010)
    #%>%mutate(Percent = n/sum(n))
    matrix2 <- matrix2 %>%
      rename("starttype" = Ntype_2000, "endtype" = Ntype_2010, "freq" = n) %>%
      mutate(year = 2)

    transmatrix <- rbind(matrix1,matrix2)

    library(tidyr)
    transmatrix <- transmatrix %>% group_by(year) %>% complete(starttype, endtype, fill = list(freq = 0))

    #write.csv(transmatrix, "transmatrix.csv", row.names = FALSE)

Create a table for the average composition of each neighborhood type by
census year.

    #Average Composition of each neighborhood type by Census year
    AvgTypeComp <- dfLong %>% 
      group_by(year, Ntype)%>%
      summarise(n=n(), WHmean = mean(White/TotPop), BLmean = mean(Black/TotPop),
                LTNmean = mean(Latinx/TotPop), ASNmean = mean(Asian/TotPop))

    AvgTypeComp <- as.data.table(AvgTypeComp)

    #different layout of the table
    AvgTypeComp2 <- dcast(AvgTypeComp,Ntype~year, 
                            value.var=c("n", "WHmean", "BLmean", "LTNmean", "ASNmean"))
    AvgTypeComp2

    ##     Ntype n_1990 n_2000 n_2010 WHmean_1990 WHmean_2000 WHmean_2010 BLmean_1990
    ##  1:     A     NA     NA      1          NA          NA  0.19351999          NA
    ##  2:     B    428    454    737  0.07193017  0.06233466  0.06305438  0.90959195
    ##  3:    BA    121    294    471  0.21803608  0.18684215  0.15041928  0.40921278
    ##  4:    BL    220    353    669  0.12373979  0.11125676  0.10131159  0.53350723
    ##  5:     L    167    259    908  0.15708053  0.13297345  0.09039316  0.02217072
    ##  6:    LA     29     53    144  0.22815052  0.18376055  0.15954936  0.05144024
    ##  7:     W   1563   1317   1535  0.94073029  0.90546967  0.88323256  0.02241160
    ##  8:    WA   1290   1204   1554  0.89061191  0.82892060  0.78501340  0.03551871
    ##  9:    WB    747    599    873  0.74635768  0.70447734  0.63474011  0.22801417
    ## 10:   WBA    654    546   1256  0.71263731  0.65271581  0.58652971  0.21280926
    ## 11:   WBL    279    376    645  0.58018891  0.52772996  0.46708065  0.22668625
    ## 12:  WBLA    885   1102   1514  0.56667719  0.48163245  0.41949812  0.22864099
    ## 13:    WL    893    919   1126  0.75872842  0.69009037  0.60668445  0.02625374
    ## 14:   WLA   1131    931   1033  0.76876687  0.66132403  0.58050541  0.04286132
    ##     BLmean_2000 BLmean_2010 LTNmean_1990 LTNmean_2000 LTNmean_2010 ASNmean_1990
    ##  1:          NA  0.03961965           NA           NA   0.06497623           NA
    ##  2:  0.89213629  0.87161509   0.01343365   0.02523743   0.03994111  0.002422385
    ##  3:  0.35250545  0.33310904   0.31627903   0.35297921   0.41357775  0.051007949
    ##  4:  0.48344413  0.44156342   0.33496300   0.38657418   0.43346724  0.003395959
    ##  5:  0.02569775  0.01811085   0.81317157   0.82958408   0.88029040  0.002854047
    ##  6:  0.04849263  0.04468903   0.65731217   0.69313277   0.72462752  0.056827921
    ##  7:  0.02707948  0.02940665   0.02319910   0.03772109   0.05324906  0.005118954
    ##  8:  0.03959827  0.04430671   0.03181847   0.04816906   0.06485663  0.036215674
    ##  9:  0.23305087  0.27244890   0.01623744   0.03521520   0.05420892  0.004633901
    ## 10:  0.21455234  0.23290345   0.02964252   0.04972423   0.07185996  0.038695656
    ## 11:  0.21463958  0.21604238   0.18129321   0.22972051   0.28186835  0.004863387
    ## 12:  0.23991201  0.25453182   0.14490552   0.18036313   0.22364910  0.054301252
    ## 13:  0.03342727  0.03811188   0.20267120   0.25026680   0.32557479  0.005641182
    ## 14:  0.04955444  0.05402050   0.14607146   0.20528847   0.27638911  0.038050873
    ##     ASNmean_2000 ASNmean_2010
    ##  1:           NA  0.666314492
    ##  2:  0.004262649  0.005405093
    ##  3:  0.082046911  0.080143656
    ##  4:  0.005507718  0.007501386
    ##  5:  0.003712851  0.004922063
    ##  6:  0.060396738  0.058700646
    ##  7:  0.007987678  0.010398907
    ##  8:  0.061491864  0.079515536
    ##  9:  0.007864974  0.010608780
    ## 10:  0.056165187  0.074896439
    ## 11:  0.007837224  0.011295753
    ## 12:  0.067675275  0.070803852
    ## 13:  0.007614840  0.010069717
    ## 14:  0.059858338  0.064987182

    #write_xlsx(AvgTypeComp2, "AvgTypeComp2.xlsx")

Create transition matrices: from 1990-2000 and 2000-2010.

    #read in data
    transmatrix <- read.csv("data/transmatrix.csv")

    #Matrices for transition 1 (1990-2000) and transition 2 (2000-2010)
    transtime1 <- transmatrix %>% filter(year==1)
    transtime2 <- transmatrix %>% filter(year==2)

    matrix1 <- as.data.frame.matrix(xtabs(freq ~ starttype + endtype, transtime1))
    matrix2 <- as.data.frame.matrix(xtabs(freq ~ starttype + endtype, transtime2))

    #1990-2000 Matrix
    matrix90_00 <- dfWide %>%
      group_by(Ntype_1990)%>%
      count(Ntype_2000)

    library(tidyr)
    matrix90_00 <- pivot_wider(matrix90_00, names_from = Ntype_2000, values_from = n)
    #matrix90_00 <- as.data.frame.matrix(xtabs(n ~ Ntype_1990 + Ntype_2000, matrix90_00))
    matrix90_00

    ## # A tibble: 14 x 15
    ## # Groups:   Ntype_1990 [14]
    ##    Ntype_1990     B    BA    BL    WB   WBA   WBL  WBLA     L    LA    WL     W
    ##    <chr>      <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ##  1 B            359     6    43     5     1     2    12    NA    NA    NA    NA
    ##  2 BA            NA    69    32    NA    NA    NA     5     5    10    NA    NA
    ##  3 BL             2     7   185    NA    NA     8     3    12     2     1    NA
    ##  4 L             NA    NA     1    NA    NA    NA    NA   162     2     2    NA
    ##  5 LA            NA     2    NA    NA    NA    NA    NA    19     8    NA    NA
    ##  6 W             NA    NA    NA    75    38    15     6    NA    NA   119  1085
    ##  7 WA            NA    NA    NA    12   149     4    92    NA    NA    24    77
    ##  8 WB            19    NA     8   454    71    68    16    NA    NA     7    80
    ##  9 WBA           32    32     2    43   264    11   243    NA    NA     1     5
    ## 10 WBL            6     2    41     5    NA   167    14     7    NA    28     4
    ## 11 WBLA          36   172    38     3    17    39   514    NA     8     7    NA
    ## 12 WL            NA    NA    NA     2    NA    43    10    48     3   610    55
    ## 13 WLA           NA     4     3    NA     6    19   187     6    20   120    11
    ## 14 <NA>          NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## # ... with 3 more variables: WA <int>, WLA <int>, `NA` <int>

    #2000-2010 Matrix
    matrix00_10 <- dfWide %>%
      group_by(Ntype_2000)%>%
      count(Ntype_2010)

    matrix00_10 <- pivot_wider(matrix00_10, names_from = Ntype_2010, values_from = n)
    #matrix00_10 <- as.data.frame.matrix(xtabs(n ~ Ntype_2000 + Ntype_2010, matrix00_10))
    matrix00_10

    ## # A tibble: 14 x 15
    ## # Groups:   Ntype_2000 [14]
    ##    Ntype_2000     B    BA    BL    WA    WB   WBA   WBL  WBLA     L    LA    WL
    ##    <chr>      <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ##  1 B            359     1    41     2    17    15     1    18    NA    NA    NA
    ##  2 BA             2   179    55    NA    NA     5     1    29     9    12     1
    ##  3 BL            11    15   265     1     2     2    17    13    23    NA     1
    ##  4 L             NA    NA     5    NA    NA    NA    NA    NA   235     3    15
    ##  5 LA            NA     3     2    NA    NA    NA    NA     2    17    25    NA
    ##  6 W             NA    NA    NA   209    83    51    23    27    NA    NA   120
    ##  7 WA            NA     2    NA   822     1   160     2    61    NA    NA     8
    ##  8 WB            16    NA     6    12   347    80    60    10    NA    NA     4
    ##  9 WBA            7    16     4    36    27   293    13   141     1    NA     1
    ## 10 WBL           NA     4    42     1     7     3   211    41     7    NA    47
    ## 11 WBLA          18   185    40    20     3    59    62   640     5    10    10
    ## 12 WL            NA     1     7    20     1     1    56    66    50     1   559
    ## 13 WLA           NA     7     3    79    NA    11    13   174    12    30    66
    ## 14 <NA>         324    58   199   352   385   576   186   292   549    63   294
    ## # ... with 3 more variables: WLA <int>, W <int>, A <int>

    #Code for LaTeX output
    library(xtable)
    print(xtable(matrix1, type = "latex", tabular.environment="longtable", digits = 0), file = "matrix1.tex")
    print(xtable(matrix2, type = "latex", tabular.environment="longtable", digits = 0), file = "matrix2.tex")

    #create table to look at percentages (uses numbered categories)
    transmatrix2 <- transmatrix %>%
      select(starttype, endtype, Percent, year)
    transmatrix2 <- pivot_wider(transmatrix2, names_from = endtype, values_from = Percent)

    write_xlsx(transmatrix2, "transmatrixPCT.xlsx")
