dataIn <- data.frame(country =c("IN","IN","PK"), 
Measure =c("GDP","INF_RATE","GDP"),
Q1 = c(12,7.8,10),
Q2 = c(12.4,8,10.1),
Q3 = c(12.3,7.28,10.2),
Q4 = c(12.9,8.23,10.4))


library(tidyr)
dataIn_gather <- gather(dataIn , "Quaters", "values", 3:6)

   country  Measure Quaters values
1       IN      GDP      Q1  12.00
2       IN INF_RATE      Q1   7.80
3       PK      GDP      Q1  10.00
4       IN      GDP      Q2  12.40
5       IN INF_RATE      Q2   8.00
6       PK      GDP      Q2  10.10
7       IN      GDP      Q3  12.30
8       IN INF_RATE      Q3   7.28
9       PK      GDP      Q3  10.20
10      IN      GDP      Q4  12.90
11      IN INF_RATE      Q4   8.23
12      PK      GDP      Q4  10.40

## In case if your data has duplicated rows 
dataIn_gather <- dataIn_gather[!duplicated(dataIn_gather), ]

dataIn_spread <- spread(dataIn_gather, "Measure", "values")
dataIn_spread
  country Quaters  GDP INF_RATE
1      IN      Q1 12.0     7.80
2      IN      Q2 12.4     8.00
3      IN      Q3 12.3     7.28
4      IN      Q4 12.9     8.23
5      PK      Q1 10.0       NA
6      PK      Q2 10.1       NA
7      PK      Q3 10.2       NA
8      PK      Q4 10.4       NA
