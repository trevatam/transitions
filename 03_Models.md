Loglinear Analysis
================
Treva Tam
11/26/2019

    # read in data
    dfLong <- readRDS("data/dfLong.rds")
    dfWide <- readRDS("data/dfWide.rds")

Now that we’ve gone through and established our data, let’s set
ourselves up for our analysis. If you need a reminder of what numbers
correlate to which neighborhood type, the code is in “02\_Descriptives”

    transmatrix<- read.csv("data/transmatrix.csv")

    starttype <- as.factor(transmatrix$starttype)
    endtype <- as.factor(transmatrix$endtype)
    year<- as.factor(transmatrix$year)
    freq<- transmatrix$freq

    temp <- cbind(starttype, endtype, year, freq)
    temp <- as.data.frame(temp)

    xtabs(freq ~ starttype + endtype + year, data = transmatrix)

    ## , , year = 1
    ## 
    ##          endtype
    ## starttype    1    2    5    7    9   10   12   13   15
    ##        1  1540    0   96  582    0    0   91   48    9
    ##        2     0  584    7    0   59    0    1    0   42
    ##        5   104   33  625   52   14    0  225    2   27
    ##        7   266    0   55 1913    4   65  371  338  125
    ##        9     0   19    0    1  365   34   10    0    8
    ##        10    0    0    0    6    7  674    0    8    0
    ##        12   14   54  141   84   93    6  913   13  358
    ##        13   27    0    0  281   14   82   44  736  224
    ##        15    1   53    4   18  249   15   86   41  505
    ## 
    ## , , year = 2
    ## 
    ##          endtype
    ## starttype    1    2    5    7    9   10   12   13   15
    ##        1  1193    0   94  461    0    0  107   60   37
    ##        2     0  598   25    1   64    0   20    0   35
    ##        5    81   23  575   16    7    0  195    3   28
    ##        7   240    0   30 1815   10   65  318  296  163
    ##        9     0   15    6    3  652   54   28    3   44
    ##        10    0    0    0   21   21  826    0    6    2
    ##        12   12   24  136  117  101    9 1017   24  301
    ##        13    6    0    0  214   17   77   42  596  234
    ##        15    3   77    7   32  268   22  174   45  670

    #write_xlsx(xtabs, "transmatrixtemp.xlsx")

    startnum <- as.numeric(transmatrix$starttype)
    endnum <- as.numeric(transmatrix$endtype)

I do not show the results for each of the following models. However,
I’ve included the rcode for each model. My paper discusses these models
in more detail.

    library(lmtest)
    library(gnm)

    #saturated model
    Satmod <- gnm(freq ~ starttype * endtype * year, family = poisson)
    summary(Satmod)

    #Quasi-independence model
    Indepmod <- gnm(freq ~ starttype + endtype + year + year:Diag(starttype,endtype),
              family = poisson)
    summary(Indepmod)
    lrtest(Indepmod, Satmod)

    #Na - no association between start type and end type given year of transition
    Na <- gnm(freq ~ starttype + endtype + year + year:starttype + year:endtype + year:Diag(starttype,endtype),
              family = poisson)
    summary(Na)
    lrtest(Na, Satmod)

    #Ro - odds of end type of neighborhood only vary by the start type - association is same across time
    Ro <- update(Na, ~ . + starttype:endnum)
    summary(Ro)
    lrtest(Ro, Satmod)

    Ru <- update(Na, ~ . + starttype:endnum + year:startnum:endnum)
    summary(Ru)
    lrtest(Ru, Satmod)

    Rx <- update(Na, ~ . + Mult(Exp(year), starttype:endnum))
    summary(Rx)
    lrtest(Rx, Satmod)

    Co <- update(Na, ~ . + endtype:startnum)
    summary(Co)
    lrtest(Co, Satmod)

    Cu <- update(Na, ~ . + endtype:startnum + year:startnum:endnum)
    summary(Cu)
    lrtest(Cu, Satmod)

    Cx <- update(Na, ~ . + Mult(year, endtype:startnum))
    summary(Cx)
    lrtest(Cx, Satmod)

    RCIo <- update(Na, ~ . + starttype:endnum + endtype:startnum)
    summary(RCIo)
    lrtest(RCIo, Satmod)

    RCIu <- update(Na, ~ . + starttype:endnum + endtype:startnum + year:startnum:endnum)
    summary(RCIu)
    lrtest(RCIu, Satmod)

    RCIx <- update(Na, ~ . + Mult(year, starttype:endnum + endtype:startnum))
    summary(RCIx)
    lrtest(RCIx, Satmod)

    RCIIo <- update(Na, ~ . + Mult(starttype, endtype))
    summary(RCIIo)
    lrtest(RCIIo, Satmod)

    RCIIx <- update(Na, ~ . + Mult(year, starttype, endtype))
    summary(RCIIx)
    lrtest(RCIIx, Satmod)

    FIo <- update(Na, ~ . + Mult(starttype:endtype))
    summary(FIo) 
    lrtest(FIo, Satmod)

    FIu <- update(Na, ~ . + Mult(starttype:endtype) + year:startnum:endnum)
    summary(FIu) 
    lrtest(FIu, Satmod)

    FIx <- update(Na, ~ . + Mult(year, starttype:endtype))
    summary(FIx)
    lrtest(FIx, Satmod)
