\[Dacon\]Pr-01\|Energy
================

``` r
knitr::opts_chunk$set(echo = TRUE)
```

# 패키지 설치

필요한 각종 패키지를 설치

``` r
install.packages('data.table')
install.packages('magrittr')
install.packages('ggplot2')
install.packages('plotly')
install.packages('tidyverse')
install.packages('factoextra') 
```

``` r
library(data.table)
library(magrittr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(factoextra)
```

# data import

``` r
train_data = fread('data/train.csv')
head(train_data)
```

    ##    num     date_time 전력사용량(kWh) 기온(°C) 풍속(m/s) 습도(%) 강수량(mm)
    ## 1:   1 2020-06-01 00        8179.056      17.6       2.5      92        0.8
    ## 2:   1 2020-06-01 01        8135.640      17.7       2.9      91        0.3
    ## 3:   1 2020-06-01 02        8107.128      17.5       3.2      91        0.0
    ## 4:   1 2020-06-01 03        8048.808      17.1       3.2      91        0.0
    ## 5:   1 2020-06-01 04        8043.624      17.0       3.3      92        0.0
    ## 6:   1 2020-06-01 05        8010.576      16.9       3.4      93        0.0
    ##    일조(hr) 비전기냉방설비운영 태양광보유
    ## 1:        0                  0          0
    ## 2:        0                  0          0
    ## 3:        0                  0          0
    ## 4:        0                  0          0
    ## 5:        0                  0          0
    ## 6:        0                  0          0

## column 변수명 교환

``` r
names(train_data) = c('num','datetime','target','temperature','windspeed','humidity','precipitation','insolation','nelec_cool_flag','solar_flag')
head(train_data)
```

    ##    num      datetime   target temperature windspeed humidity precipitation
    ## 1:   1 2020-06-01 00 8179.056        17.6       2.5       92           0.8
    ## 2:   1 2020-06-01 01 8135.640        17.7       2.9       91           0.3
    ## 3:   1 2020-06-01 02 8107.128        17.5       3.2       91           0.0
    ## 4:   1 2020-06-01 03 8048.808        17.1       3.2       91           0.0
    ## 5:   1 2020-06-01 04 8043.624        17.0       3.3       92           0.0
    ## 6:   1 2020-06-01 05 8010.576        16.9       3.4       93           0.0
    ##    insolation nelec_cool_flag solar_flag
    ## 1:          0               0          0
    ## 2:          0               0          0
    ## 3:          0               0          0
    ## 4:          0               0          0
    ## 5:          0               0          0
    ## 6:          0               0          0

## 날짜 및 시간 파생변수 도출

``` r
train_data[, datetime := as.POSIXct(datetime, tz='', format='%F %H')]
train_data[, `:=`(date = as.Date(datetime, tz=''),
                  hour = as.factor(format(datetime, format='%H')), 
                  #Hours as decimal number (00–23)
                  Weekday  = as.integer(format(datetime, format='%u')))] 
                  #monday =1
train_data <- train_data %>% mutate(Weekend = ifelse(Weekday >= 6, 1,0))
# Weekend 변수 : 토,일요일은 1, 나머지는 0
head(train_data)
```

    ##    num            datetime   target temperature windspeed humidity
    ## 1:   1 2020-06-01 00:00:00 8179.056        17.6       2.5       92
    ## 2:   1 2020-06-01 01:00:00 8135.640        17.7       2.9       91
    ## 3:   1 2020-06-01 02:00:00 8107.128        17.5       3.2       91
    ## 4:   1 2020-06-01 03:00:00 8048.808        17.1       3.2       91
    ## 5:   1 2020-06-01 04:00:00 8043.624        17.0       3.3       92
    ## 6:   1 2020-06-01 05:00:00 8010.576        16.9       3.4       93
    ##    precipitation insolation nelec_cool_flag solar_flag       date hour Weekday
    ## 1:           0.8          0               0          0 2020-06-01   00       1
    ## 2:           0.3          0               0          0 2020-06-01   01       1
    ## 3:           0.0          0               0          0 2020-06-01   02       1
    ## 4:           0.0          0               0          0 2020-06-01   03       1
    ## 5:           0.0          0               0          0 2020-06-01   04       1
    ## 6:           0.0          0               0          0 2020-06-01   05       1
    ##    Weekend
    ## 1:       0
    ## 2:       0
    ## 3:       0
    ## 4:       0
    ## 5:       0
    ## 6:       0

## 결측값 확인 및 대체

# Exploratory Data Analysis (EDA)

## 전력 사용량 분석

### 특정 건물의 요일/시간별 전력사용량

시간, 요일별 전력사용량의 중앙값을 계산하고 heatmap을 그림

``` r
  for ( i in 1:5){
    p = train_data[num==i, .(M = median(target)), by=.(hour, Weekday)] %>% 
    ggplot(aes(hour, Weekday, fill=M)) + 
    geom_tile() +
    scale_fill_distiller(palette='YlGnBu', direction=1) +
    ggtitle(paste0(i, '번째 건물'))
    plot(p)
  }
```

![](Dacon_Practice_01_energy_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](Dacon_Practice_01_energy_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](Dacon_Practice_01_energy_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](Dacon_Practice_01_energy_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](Dacon_Practice_01_energy_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->
열지도에서 색이 짙을수록 전력사용량이 많음

### 특정 건물의 요일/3시간별 전력사용량

``` r
# 3시간 단위로 묶음
train_data[, hour_by3 := formatC((as.integer(format(datetime, format='%H')) %/% 3)*3, width=2, flag='0')]
```

    ## Warning in `[.data.table`(train_data, , `:=`(hour_by3,
    ## formatC((as.integer(format(datetime, : Invalid .internal.selfref detected and
    ## fixed by taking a (shallow) copy of the data.table so that := can add this new
    ## column by reference. At an earlier point, this data.table has been copied by R
    ## (or was created manually using structure() or similar). Avoid names<- and attr<-
    ## which in R currently (and oddly) may copy the whole data.table. Use set* syntax
    ## instead to avoid copying: ?set, ?setnames and ?setattr. If this message doesn't
    ## help, please report your use case to the data.table issue tracker so the root
    ## cause can be fixed or this message improved.

``` r
head(train_data)
```

    ##    num            datetime   target temperature windspeed humidity
    ## 1:   1 2020-06-01 00:00:00 8179.056        17.6       2.5       92
    ## 2:   1 2020-06-01 01:00:00 8135.640        17.7       2.9       91
    ## 3:   1 2020-06-01 02:00:00 8107.128        17.5       3.2       91
    ## 4:   1 2020-06-01 03:00:00 8048.808        17.1       3.2       91
    ## 5:   1 2020-06-01 04:00:00 8043.624        17.0       3.3       92
    ## 6:   1 2020-06-01 05:00:00 8010.576        16.9       3.4       93
    ##    precipitation insolation nelec_cool_flag solar_flag       date hour Weekday
    ## 1:           0.8          0               0          0 2020-06-01   00       1
    ## 2:           0.3          0               0          0 2020-06-01   01       1
    ## 3:           0.0          0               0          0 2020-06-01   02       1
    ## 4:           0.0          0               0          0 2020-06-01   03       1
    ## 5:           0.0          0               0          0 2020-06-01   04       1
    ## 6:           0.0          0               0          0 2020-06-01   05       1
    ##    Weekend hour_by3
    ## 1:       0       00
    ## 2:       0       00
    ## 3:       0       00
    ## 4:       0       03
    ## 5:       0       03
    ## 6:       0       03

``` r
agg_cl = train_data[, .(M = median(target)), by=.(num, hour_by3, Weekday)]
agg_cl
```

    ##       num hour_by3 Weekday        M
    ##    1:   1       00       1 8541.288
    ##    2:   1       03       1 8499.816
    ##    3:   1       06       1 8506.944
    ##    4:   1       09       1 8613.216
    ##    5:   1       12       1 8643.024
    ##   ---                              
    ## 3356:  60       09       7 3666.816
    ## 3357:  60       12       7 3714.336
    ## 3358:  60       15       7 3721.464
    ## 3359:  60       18       7 3515.184
    ## 3360:  60       21       7 3264.840

``` r
agg_cl[, Ratio:= M/max(M), by=.(num)] # 각 건물의 최대값으로 normalization 
agg_cl
```

    ##       num hour_by3 Weekday        M     Ratio
    ##    1:   1       00       1 8541.288 0.9863803
    ##    2:   1       03       1 8499.816 0.9815910
    ##    3:   1       06       1 8506.944 0.9824141
    ##    4:   1       09       1 8613.216 0.9946868
    ##    5:   1       12       1 8643.024 0.9981292
    ##   ---                                        
    ## 3356:  60       09       7 3666.816 0.9710559
    ## 3357:  60       12       7 3714.336 0.9836403
    ## 3358:  60       15       7 3721.464 0.9855280
    ## 3359:  60       18       7 3515.184 0.9309004
    ## 3360:  60       21       7 3264.840 0.8646036

``` r
dim(agg_cl)
```

    ## [1] 3360    5

``` r
dt_cl = dcast(agg_cl, num ~ Weekday+hour_by3, value.var='Ratio')
dim(dt_cl) #60건물, 7요일*3시간단위8개 
```

    ## [1] 60 57

### 계층적 군집화

``` r
  dist_building = dist(dt_cl[, -1])
  hc_builing = hclust(dist(dt_cl[, -1]), method='ward.D2')
  #library(factoextra)
  fviz_dend(hc_builing, k = 5,
            cex = 0.5,
            k_colors = RColorBrewer::brewer.pal(5, 'Set1'),
            color_labels_by_k = TRUE,
            rect = TRUE,
            rect_border = RColorBrewer::brewer.pal(5, 'Set1'),
            rect_fill = TRUE) 
```

    ## Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
    ## "none")` instead.

    ## Warning in if (color == "cluster") color <- "default": length > 1 이라는 조건이
    ## 있고, 첫번째 요소만이 사용될 것입니다

![](Dacon_Practice_01_energy_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
5가지의 군집으로 묶일 수 있는 것을 알 수 있음. 군집별로 요일/시간대별
(최댓값 대비 전력사용량의 중앙값) 비율의 평균을 계산해서 열지도로 표현

``` r
  # 군집 평균(중심)의 시각화
  dt_cl_result = data.table(num=1:60,
                            grp=LETTERS[cutree(hc_builing, k=5)])
  dt_cl_result
```

    ##     num grp
    ##  1:   1   A
    ##  2:   2   B
    ##  3:   3   A
    ##  4:   4   C
    ##  5:   5   A
    ##  6:   6   D
    ##  7:   7   D
    ##  8:   8   D
    ##  9:   9   A
    ## 10:  10   C
    ## 11:  11   C
    ## 12:  12   C
    ## 13:  13   B
    ## 14:  14   B
    ## 15:  15   A
    ## 16:  16   A
    ## 17:  17   D
    ## 18:  18   D
    ## 19:  19   E
    ## 20:  20   E
    ## 21:  21   E
    ## 22:  22   B
    ## 23:  23   B
    ## 24:  24   A
    ## 25:  25   D
    ## 26:  26   B
    ## 27:  27   D
    ## 28:  28   B
    ## 29:  29   B
    ## 30:  30   B
    ## 31:  31   A
    ## 32:  32   A
    ## 33:  33   A
    ## 34:  34   C
    ## 35:  35   D
    ## 36:  36   B
    ## 37:  37   B
    ## 38:  38   B
    ## 39:  39   B
    ## 40:  40   C
    ## 41:  41   C
    ## 42:  42   C
    ## 43:  43   B
    ## 44:  44   B
    ## 45:  45   B
    ## 46:  46   D
    ## 47:  47   D
    ## 48:  48   D
    ## 49:  49   E
    ## 50:  50   E
    ## 51:  51   E
    ## 52:  52   B
    ## 53:  53   D
    ## 54:  54   B
    ## 55:  55   B
    ## 56:  56   D
    ## 57:  57   B
    ## 58:  58   B
    ## 59:  59   B
    ## 60:  60   A
    ##     num grp

``` r
  agg_cl[dt_cl_result, on=.(num), G := i.grp]
  agg_cl[, .(MM = mean(Ratio)), by=.(G, hour_by3, Weekday)] %>%
    ggplot(aes(hour_by3, Weekday, fill=MM)) + geom_tile()+
    scale_fill_distiller(palette='YlGnBu', direction=1) +
    facet_grid(cols=vars(G))
```

![](Dacon_Practice_01_energy_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### 일별 전력사용량 추이

``` r
 # 일별 전력사용량 
  train_data[dt_cl_result, on=.(num), cl_grp := i.grp]
  p_cl = train_data[, .(TOTAL = sum(target)), by=.(num, cl_grp, date)] %>% 
    ggplot(aes(date, TOTAL, group=num, color=cl_grp))+ geom_path() 
  p_cl
```

![](Dacon_Practice_01_energy_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## 비전기냉방설비운영 및 태양광 발전시설 여부 활용

## 외부변수 활용
