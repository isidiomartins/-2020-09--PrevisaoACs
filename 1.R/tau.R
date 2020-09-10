source("1.R/ur.df.modified.R")

tau<- function(x,lags){
  int3<- ur.df.modified(y = na.omit(x), type = "trend", lags = lags)
  int2<- ur.df.modified(y = na.omit(x), type = "drift", lags = lags)
  int1<- ur.df.modified(y = na.omit(x), type = "none", lags = lags)
  
  gamma<- int3$teststat[1]
  m3_gamma_s<- ifelse(gamma <= -4.15, "1%", ifelse(gamma <= -3.5, "5%", "-"))
  bg<- bgtest(int3$result, order = 1)$p.value
  m3_bg_s<- ifelse(bg <= 0.01, "1%", ifelse(bg <= 0.05, "5%", "-"))
  
  gamma<- int2$teststat[1]
  m2_gamma_s<- ifelse(gamma <= -3.58, "1%", ifelse(gamma <= -2.93, "5%", "-"))
  bg<- bgtest(int2$result, order = 1)$p.value
  m2_bg_s<- ifelse(bg <= 0.01, "1%", ifelse(bg <= 0.05, "5%", "-"))
  
  gamma<- int1$teststat[1]
  m1_gamma_s<- ifelse(gamma <= -2.62, "1%", ifelse(gamma <= -1.95, "5%", "-"))
  bg<- bgtest(int1$result, order = 1)$p.value
  m1_bg_s<- ifelse(bg <= 0.01, "1%", ifelse(bg <= 0.05, "5%", "-"))
  
  y<- data.frame(m3_gamma_s,m3_bg_s,
                 m2_gamma_s,m2_bg_s,
                 m1_gamma_s,m1_bg_s,
                 lags,
                 stringsAsFactors = F)
  
  y$resumo<-
    y$m3_gamma_s %in% c("1%", "5%") & !y$m3_bg_s %in% c("1%", "5%")|
    y$m2_gamma_s %in% c("1%", "5%") & !y$m2_bg_s %in% c("1%", "5%")|
    y$m1_gamma_s %in% c("1%", "5%") & !y$m1_bg_s %in% c("1%", "5%")
  
  unlist(y)
}