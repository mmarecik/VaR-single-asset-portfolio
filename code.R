library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(e1071) #kurtoza
library(GAS)

dax <- read.csv("dax.csv") # German Stock Index DAX 30
wig <- read.csv("wig.csv") 
smi <- read.csv("smi.csv")

dax <- dax[,c(1,5)]
wig <- wig[,c(1,5)]
smi <- smi[,c(1,5)]

colnames(dax) <- c("data", "zamkniecie")
colnames(wig) <- c("data", "zamkniecie")
colnames(smi) <- c("data", "zamkniecie")

dax <- dax %>% mutate(data = as.Date(data))
wig <- wig %>% mutate(data = as.Date(data))
wig <- wig %>% mutate(zamkniecie = as.numeric(zamkniecie))
smi <- smi %>% mutate(data = as.Date(data))

dax.st.zwrotu <- log(dax$zamkniecie/lag(dax$zamkniecie))  #German Stock Index
wig.st.zwrotu <- log(wig$zamkniecie/lag(wig$zamkniecie)) #Warsaw Stock Index
smi.st.zwrotu <- log(smi$zamkniecie/lag(smi$zamkniecie)) #Swiss Market Index

dax$st.zwrotu <- dax.st.zwrotu
wig$st.zwrotu <- wig.st.zwrotu
smi$st.zwrotu <- smi.st.zwrotu

dax <- dax[-1,]
wig <- wig[-1,]
smi <- smi[-1,]

#----
ggplot(dax, aes(data)) +
  geom_line(aes(y = zamkniecie), col="#c41d1d") + 
    labs(title="Ceny zamkniecia indeksu gieldowego DAX30 \nw latach 2006-2021") + 
      theme_bw() + ylab("") + xlab("")

ggplot(wig, aes(data)) +
  geom_line(aes(y = zamkniecie), col="#c41d1d") + 
  labs(title="Ceny zamkniecia indeksu gieldowego WIG \nw latach 2006-2021") + 
  theme_bw() + ylab("") + xlab("")

ggplot(smi, aes(data)) +
  geom_line(aes(y = zamkniecie), col="#c41d1d") + 
  labs(title="Ceny zamkniecia indeksu gieldowego SMI \nw latach 2006-2021") + 
  theme_bw() + ylab("") + xlab("")


# Dokonaæ krótkiej opisowej analizy danych (opis wybranych zmiennych, statystyki opisowe itp).

ggplot(dax, aes(x=data, y=st.zwrotu)) +
  geom_line() + labs(title="Logarytmiczne stopy zwrotu niemieckiego indeksu gieldowego DAX30 \nw latach 2006-2021") + 
  theme_bw() + ylab("") + xlab("")

ggplot(wig, aes(x=data, y=st.zwrotu)) +
  geom_line() + labs(title="Logarytmiczne stopy zwrotu warszawskiego indeksu gieldowego WIG \nw latach 2006-2021") + 
  theme_bw() + ylab("") + xlab("")

ggplot(smi, aes(x=data, y=st.zwrotu)) +
  geom_line() + labs(title="Logarytmiczne stopy zwrotu szwajcarskiego indeksu gieldowego SMI \nw latach 2006-2021") + 
  theme_bw() + ylab("") + xlab("")
#----

#----
# Wartoœci wybranych statystyk opisowych  logarytmicznych stóp zwrotu z indeksów gie³dowych notowanych na GPW w Warszawie

indeks = c("DAX", "WIG", "SMI")

srednia = c(mean(dax$st.zwrotu),
            mean(wig$st.zwrotu),
            mean(smi$st.zwrotu))

odch.stand = c(sd(dax$st.zwrotu),
               sd(wig$st.zwrotu),
               sd(smi$st.zwrotu))

max = c(max(dax$st.zwrotu),
        max(wig$st.zwrotu),
        max(smi$st.zwrotu))

min = c(min(dax$st.zwrotu),
        min(wig$st.zwrotu),
        min(smi$st.zwrotu))

kurtoza = c(kurtosis(dax$st.zwrotu),
            kurtosis(wig$st.zwrotu),
            kurtosis(smi$st.zwrotu))

skosnosc = c(skewness(dax$st.zwrotu),
             skewness(wig$st.zwrotu),
             skewness(smi$st.zwrotu))

stat.opisowe <- data.frame( indeks, srednia, odch.stand, max, min, kurtoza, skosnosc)
stat.opisowe
#----

#----
#rozklad

g1 <- ggplot(dax, aes(st.zwrotu)) +
  geom_density(size = 0.7) + theme_bw() + ylab("") + xlab("") +
  labs(title="Wykres gestosci empirycznego rozkladu stop zwrotu \nindeksu DAX30, WIG oraz SMIoraz teoretycznego rozkladu normalnego") + 
   stat_function(fun = dnorm,
                 args = list(mean = mean(dax$st.zwrotu),
                             sd = sd(dax$st.zwrotu)),
                 col = "#d92588",
                 size = 0.7)

g2 <- ggplot(wig, aes(st.zwrotu)) +
  geom_density(size = 0.7) + theme_bw() + ylab("") + xlab("") +
  labs(title="") + 
  stat_function(fun = dnorm,
                args = list(mean = mean(wig$st.zwrotu),
                            sd = sd(wig$st.zwrotu)),
                col = "#31ad9b",
                size = 0.7)

g3 <- ggplot(smi, aes(st.zwrotu)) +
  geom_density(size = 0.7) + theme_bw() + ylab("") + xlab("") +
  labs(title="") + 
  stat_function(fun = dnorm,
                args = list(mean = mean(smi$st.zwrotu),
                            sd = sd(smi$st.zwrotu)),
                col = "#2c6ad4",
                size = 0.7)

library(ggpubr)
grid.arrange(g1,g2,g3, nrow=3)

 

library(stabledist)
par <- libstableR::stable_fit_mle(dax$st.zwrotu)
goftest::ad.test(dax$st.zwrotu, pstable, par[1], par[2], par[3], par[4])
goftest::ad.test(dax$st.zwrotu, pt, df=20)
install.packages("statmod")
library(statmod)
goftest::ad.test(dax$st.zwrotu, pinvgauss, mean(dax$st.zwrotu))
goftest::ad.test(dax$st.zwrotu, plogis)

par <- libstableR::stable_fit_mle(wig$st.zwrotu)
goftest::ad.test(wig$st.zwrotu, pstable, par[1], par[2], par[3], par[4])
goftest::ad.test(wig$st.zwrotu, pt, df=20)
goftest::ad.test(wig$st.zwrotu, pinvgauss, mean(wig$st.zwrotu))
goftest::ad.test(wig$st.zwrotu, plogis)

par <- libstableR::stable_fit_mle(smi$st.zwrotu)
goftest::ad.test(smi$st.zwrotu, pstable, par[1], par[2], par[3], par[4])
goftest::ad.test(smi$st.zwrotu, pt, df=20)
goftest::ad.test(smi$st.zwrotu, pinvgauss, mean(smi$st.zwrotu))
goftest::ad.test(smi$st.zwrotu, plogis)

#----


#Symulacja historyczna----

T <- 250

sym.hist <- function(st.zwrotu, okno = 250, p = 0.05){
  VaR_msh <- c()
  ES_msh <- c()
  k <- as.integer(okno*p)
  i <- okno + 1
  while(i < length(st.zwrotu) + 2){
    dane <- st.zwrotu[(i-okno):(i-1)]
    VaR_msh[i - okno] <- quantile(dane, p)
    ES_msh[i - okno] <- mean(sort(dane)[1:k])
    i=i+1
  }
  result <- data.frame(VaR = VaR_msh, ES = ES_msh)
  return(result)
}


#DAX
dax.sym.hist.95 <- -sym.hist(dax$st.zwrotu, okno = 250, p = 0.05)
dax.sym.hist.95$data <- dax$data[T:nrow(dax)]
dax.sym.hist.95$st.zwrotu <- dax$st.zwrotu[T:nrow(dax)]

g1 <- ggplot(dax.sym.hist.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="DAX - 95% VaR oraz ES - metoda symualcji historycznych",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


dax.sym.hist.99 <- -sym.hist(dax$st.zwrotu, okno = 250, p = 0.01)
dax.sym.hist.99$data <- dax$data[T:nrow(dax)]
dax.sym.hist.99$st.zwrotu <- dax$st.zwrotu[T:nrow(dax)]

g2 <- ggplot(dax.sym.hist.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="DAX - 99% VaR oraz ES - metoda symualcji historycznych",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()



#WIG
wig.sym.hist.95 <- -sym.hist(wig$st.zwrotu, okno = 250, p = 0.05)
wig.sym.hist.95$data <- wig$data[T:nrow(wig)]
wig.sym.hist.95$st.zwrotu <- wig$st.zwrotu[T:nrow(wig)]

g3 <- ggplot(wig.sym.hist.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="WIG - 95% VaR oraz ES - metoda symualcji historycznych",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


wig.sym.hist.99 <- -sym.hist(wig$st.zwrotu, okno = 250, p = 0.01)
wig.sym.hist.99$data <- wig$data[T:nrow(wig)]
wig.sym.hist.99$st.zwrotu <- wig$st.zwrotu[T:nrow(wig)]

g4 <- ggplot(wig.sym.hist.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="WIG - 99% VaR oraz ES - metoda symualcji historycznych",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


#SMI
smi.sym.hist.95 <- -sym.hist(smi$st.zwrotu, okno = 250, p = 0.05)
smi.sym.hist.95$data <- smi$data[T:nrow(smi)]
smi.sym.hist.95$st.zwrotu <- smi$st.zwrotu[T:nrow(smi)]

g5 <- ggplot(smi.sym.hist.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="SMI - 95% VaR oraz ES - metoda symualcji historycznych",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


smi.sym.hist.99 <- -sym.hist(smi$st.zwrotu, okno = 250, p = 0.01)
smi.sym.hist.99$data <- smi$data[T:nrow(smi)]
smi.sym.hist.99$st.zwrotu <- smi$st.zwrotu[T:nrow(smi)]

g6 <- ggplot(smi.sym.hist.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="SMI - 99% VaR oraz ES - metoda symualcji historycznych",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()



#Symulacja historyczna z wagami----

#https://github.com/harrelfe/Hmisc/issues/97
wtd.quantile<- function (x, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1),
                         type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
                         na.rm = TRUE)  {
  # Function taken from HMISC, but issue solved which is documented here: https://github.com/harrelfe/Hmisc/issues/97#issuecomment-429634634
  normwt = FALSE
  if (!length(weights))      return(quantile(x, probs = probs, na.rm = na.rm))
  type <- match.arg(type)
  if (any(probs < 0 | probs > 1))      stop("Probabilities must be between 0 and 1 inclusive")
  nams <- paste(format(round(probs * 100, if (length(probs) >
                                              1) 2 - log10(diff(range(probs))) else 2)), "%", sep = "")
  
  if(na.rm & any(is.na(weights))){   ###### new
    i<- is.na(weights)
    x <- x[!i]
    weights <- weights[!i]
  }
  i <- weights <= 0         # nwe: kill negative and zero weights and associated data
  if (any(i)) {
    x <- x[!i]
    weights <- weights[!i]
  }
  if (type == "quantile") {
    if(sum(weights) < 1000000 ) {weights<- weights*1000000/sum(weights)}  ##### new
    w <- wtd.table(x, weights, na.rm = na.rm, normwt = normwt,
                   type = "list")
    x <- w$x
    wts <- w$sum.of.weights
    n <- sum(wts)
    order <- 1 + (n - 1) * probs
    low <- pmax(floor(order), 1)
    high <- pmin(low + 1, n)
    order <- order%%1
    allq <- approx(cumsum(wts), x, xout = c(low, high), method = "constant",
                   f = 1, rule = 2)$y
    k <- length(probs)
    quantiles <- (1 - order) * allq[1:k] + order * allq[-(1:k)]
    names(quantiles) <- nams
    return(quantiles)
  }
  w <- wtd.Ecdf(x, weights, na.rm = na.rm, type = type, normwt = normwt)
  structure(approx(w$ecdf, w$x, xout = probs, rule = 2)$y,
            names = nams)
}


wtd.table<- function (x, weights = NULL, type = c("list", "table"), normwt = FALSE,
                      na.rm = TRUE) {
  # Function taken from HMISC, but issue solved which is documented here: https://github.com/harrelfe/Hmisc/issues/97#issuecomment-429634634
  p_load(timeDate)
  type <- match.arg(type)
  if (!length(weights))
    weights <- rep(1, length(x))
  isdate <- ( class(x)[1]=="Date"  | class(x)[1]=="POSIXct") ### MODIFIED
  ax <- attributes(x)
  ax$names <- NULL
  if (is.character(x))
    x <- as.factor(x)
  lev <- levels(x)
  x <- unclass(x)
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s, drop = FALSE]
    weights <- weights[s]
  }
  n <- length(x)
  if (normwt)
    weights <- weights * length(x)/sum(weights)
  i <- order(x)
  x <- x[i]
  weights <- weights[i]
  if (anyDuplicated(x)) {
    weights <- tapply(weights, x, sum)
    if (length(lev)) {
      levused <- lev[sort(unique(x))]
      if ((length(weights) > length(levused)) && any(is.na(weights)))
        weights <- weights[!is.na(weights)]
      if (length(weights) != length(levused))
        stop("program logic error")
      names(weights) <- levused
    }
    if (!length(names(weights)))
      stop("program logic error")
    if (type == "table")
      return(weights)
    #x <-  all.is.numeric(names(weights), "vector")#  modified: commented out. function checked whether all are numeric and if yes returned the weights
    if (isdate)      attributes(x) <- c(attributes(x), ax)
    x_out<- as.numeric(names(weights))
    names(weights) <- NULL
    return(list(x = x_out, sum.of.weights = weights))
  }
  xx <- x
  if (isdate)
    attributes(xx) <- c(attributes(xx), ax)
  if (type == "list")
    list(x = if (length(lev)) lev[x] else xx, sum.of.weights = weights)
  else {
    names(weights) <- if (length(lev))
      lev[x]
    else xx
    weights
  }
}

library(pacman)

w.sym.hist <- function(st.zwrotu, q=0.995, okno = 250, p = 0.01){
  wagi <- c()
  for(j in c(1:okno)){
    wagi[j] <- q^(okno-j)*(1-q)/(1-q^okno)
  }
  k <- as.integer(okno*p)
  
  VaR_wmsh <- c()
  ES_wmsh <- c()
  i <- okno + 1
  while(i < length(st.zwrotu) + 2){
    dane <- st.zwrotu[(i-okno):(i-1)]
    temp_df <- data.frame(st.zw=dane, w=wagi)
    temp_df <- temp_df[order(temp_df$st.zw),]
    temp_df$cum <- cumsum(temp_df$w)
    VaR_wmsh[i-okno] <- wtd.quantile(dane, prob=p, weight=wagi)
    ES_wmsh[i-okno] <- sum(temp_df[1:k,1]*temp_df[1:k,2])/sum(temp_df[1:k,2])
    i=i+1
  }
  result <- data.frame(VaR = VaR_wmsh, ES = ES_wmsh)
  return(result)
}

#wykresy----
#DAX
dax.waz.sym.hist.95 <- -w.sym.hist(dax$st.zwrotu, q=0.995, okno = 250, p = 0.05)
dax.waz.sym.hist.95$data <- dax$data[T:nrow(dax)]
dax.waz.sym.hist.95$st.zwrotu <- dax$st.zwrotu[T:nrow(dax)]

g7 <- ggplot(dax.waz.sym.hist.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="DAX - 95% VaR oraz ES - metoda symualcji historycznych z wagami",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


dax.waz.sym.hist.99 <- -w.sym.hist(dax$st.zwrotu, q=0.995, okno = 250, p = 0.01)
dax.waz.sym.hist.99$data <- dax$data[T:nrow(dax)]
dax.waz.sym.hist.99$st.zwrotu <- dax$st.zwrotu[T:nrow(dax)]

g8 <- ggplot(dax.waz.sym.hist.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="DAX - 99% VaR oraz ES - metoda symualcji historycznych z wagami",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


#WIG
wig.waz.sym.hist.95 <- -w.sym.hist(wig$st.zwrotu, q=0.995, okno = 250, p = 0.05)
wig.waz.sym.hist.95$data <- wig$data[T:nrow(wig)]
wig.waz.sym.hist.95$st.zwrotu <- wig$st.zwrotu[T:nrow(wig)]

g9 <- ggplot(wig.waz.sym.hist.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="WIG - 95% VaR oraz ES - metoda symualcji historycznych z wagami",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


wig.waz.sym.hist.99 <- -w.sym.hist(wig$st.zwrotu, q=0.995, okno = 250, p = 0.01)
wig.waz.sym.hist.99$data <- wig$data[T:nrow(wig)]
wig.waz.sym.hist.99$st.zwrotu <- wig$st.zwrotu[T:nrow(wig)]

g10 <- ggplot(wig.waz.sym.hist.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="WIG - 99% VaR oraz ES - metoda symualcji historycznych z wagami",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


#SMI
smi.waz.sym.hist.95 <- -w.sym.hist(smi$st.zwrotu, q=0.995, okno = 250, p = 0.05)
smi.waz.sym.hist.95$data <- smi$data[T:nrow(smi)]
smi.waz.sym.hist.95$st.zwrotu <- smi$st.zwrotu[T:nrow(smi)]

g11 <- ggplot(smi.waz.sym.hist.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="SMI - 95% VaR oraz ES - metoda symualcji historycznych z wagami",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


smi.waz.sym.hist.99 <- -w.sym.hist(smi$st.zwrotu, q=0.995, okno = 250, p = 0.01)
smi.waz.sym.hist.99$data <- smi$data[T:nrow(smi)]
smi.waz.sym.hist.99$st.zwrotu <- smi$st.zwrotu[T:nrow(smi)]

g12 <- ggplot(smi.waz.sym.hist.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" ), size=.9) + 
  geom_line(aes(y = ES, colour = "ES" ), size=.9) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="SMI - 99% VaR oraz ES - metoda symualcji historycznych z wagami",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


grid.arrange(g1,g7,g2,g8, nrow=2)
grid.arrange(g3,g9,g4,g10, nrow=2)
grid.arrange(g5,g11,g6,g12, nrow=2)


# ggplot(dax.waz.sym.hist.95, aes(data)) + 
#   geom_line(aes(y=-VaR), col="#c41d1d", size=.9) + 
#     geom_line(aes(y=-ES), col="#206d87", size=.9) + 
#       geom_line(aes(y=dax$st.zwrotu[T:nrow(dax)]), col="#949494", alpha=0.7, size=.4) +
#   xlab("") + ylab("") + 
#   labs(title="DAX - 95% VaR oraz ES - metoda symualcji historycznych z wagami") + scale_x_date(date_breaks = "2 years")  + theme_bw()

#----


#EWMA----

zmiennosc.1 <- function(v, m=25){
  n <- length(v)
  s <- c()
  s[1:m] <- NaN
  
  for(i in c((m+1):n)){
    s[i] <- var(v[(i-m):(i-1)])
  }
  return(s)
}

EWMA <- function(v, lambda, m=25){
  n <- length(v)
  s <- c()
  s[1:(m+10)] <- zmiennosc.1(v[1:(m+10)], m)
  for(i in c((m+11):n)){
    s[i] <- lambda*s[i-1] + (1-lambda)*v[i-1]^2
  }
  return(s)
}

VaR.EWMA <- function(st.zwrotu, okno=250, m=25, p=0.05, lambda=0.94){
  
  ES_EWMA <- c()
  VaR_EWMA <- c()
  k <- as.integer(okno*p)
  i <- okno + m + 1
  
  while(i < length(st.zwrotu) + 2){
    zmiennosc <- EWMA(st.zwrotu[(i-(okno+m)):(i-1)], lambda, m)
    zmiennosc <- na.omit(zmiennosc)
    st.zw <- st.zwrotu[(i-okno):(i-1)] * 
      sqrt(zmiennosc[length(zmiennosc)]) / sqrt(zmiennosc)
    VaR_EWMA[i-(okno+m)] <- quantile(st.zw, p)
    ES_EWMA[i-(okno+m)] <- mean(sort(st.zw)[1:k])
    i=i+1
  }
  VaR_EWMA <- c(rep(NaN, m), VaR_EWMA)
  ES_EWMA <- c(rep(NaN, m), ES_EWMA)
  result <- data.frame(VaR = VaR_EWMA, ES = ES_EWMA)
  return(result)
}


#DAX
dax.ewma.95 <- -VaR.EWMA(dax$st.zwrotu, okno = 250, m=25, p = 0.05, lambda=0.9)
dax.ewma.95$data <- dax$data[T:nrow(dax)]
dax.ewma.95$st.zwrotu <- dax$st.zwrotu[T:nrow(dax)]

g1 <- ggplot(dax.ewma.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="DAX - 95% VaR oraz ES - metoda EWMA",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

dax.ewma.99 <- -VaR.EWMA(dax$st.zwrotu, okno = 250, m=25, p = 0.01, lambda=0.9)
dax.ewma.99$data <- dax$data[T:nrow(dax)]
dax.ewma.99$st.zwrotu <- dax$st.zwrotu[T:nrow(dax)]

g2 <- ggplot(dax.ewma.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="DAX - 99% VaR oraz ES - metoda EWMA",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()



#WIG
wig.ewma.95 <- -VaR.EWMA(wig$st.zwrotu, okno = 250, m=25, p = 0.05, lambda=0.9)
wig.ewma.95$data <- wig$data[T:nrow(wig)]
wig.ewma.95$st.zwrotu <- wig$st.zwrotu[T:nrow(wig)]

g3 <- ggplot(wig.ewma.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="WIG - 95% VaR oraz ES - metoda EWMA",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

wig.ewma.99 <- -VaR.EWMA(wig$st.zwrotu, okno = 250, m=25, p = 0.01, lambda=0.9)
wig.ewma.99$data <- wig$data[T:nrow(wig)]
wig.ewma.99$st.zwrotu <- wig$st.zwrotu[T:nrow(wig)]

g4 <- ggplot(wig.ewma.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="WIG - 99% VaR oraz ES - metoda EWMA",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


#SMI
smi.ewma.95 <- -VaR.EWMA(smi$st.zwrotu, okno = 250, m=25, p = 0.05, lambda=0.9)
smi.ewma.95$data <- smi$data[T:nrow(smi)]
smi.ewma.95$st.zwrotu <- smi$st.zwrotu[T:nrow(smi)]

g5 <- ggplot(smi.ewma.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="SMI - 95% VaR oraz ES - metoda EWMA",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

smi.ewma.99 <- -VaR.EWMA(smi$st.zwrotu, okno = 250, m=25, p = 0.01, lambda=0.9)
smi.ewma.99$data <- smi$data[T:nrow(smi)]
smi.ewma.99$st.zwrotu <- smi$st.zwrotu[T:nrow(smi)]

g6 <- ggplot(smi.ewma.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="SMI - 99% VaR oraz ES - metoda EWMA",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()



#GARCH----

GARCH <- function(v, gamm, alfa, beta, m=25){
  n <- length(v)
  V_L <- var(v)
  s <- c()
  s[1:(m+10)] <- zmiennosc.1(v[1:(m+10)], m)
  
  for(i in c((m+11):n)){
    s[i] <- gamm*V_L + alfa*v[i-1]^2 + beta*s[i-1]
  }
  return(s)
}

VaR.GARCH <- function(st.zwrotu, okno=250, m=25, p=0.05, gamm=0.01, alfa=0.21, beta=0.78){
  
  ES_GARCH <- c()
  VaR_GARCH <- c()
  k <- as.integer(okno*p)
  i <- okno + m + 1
  
  while(i < length(st.zwrotu) + 2){
    zmiennosc <- GARCH(st.zwrotu[(i-(okno+m)):(i-1)], gamm, alfa, beta, m)
    zmiennosc <- na.omit(zmiennosc)
    st.zw <- st.zwrotu[(i-okno):(i-1)] * 
      sqrt(zmiennosc[length(zmiennosc)]) / sqrt(zmiennosc)
    VaR_GARCH[i-(okno+m)] <- quantile(st.zw, p)
    ES_GARCH[i-(okno+m)] <- mean(sort(st.zw)[1:k])
    i=i+1
  }
  VaR_GARCH <- c(rep(NaN, m), VaR_GARCH)
  ES_GARCH <- c(rep(NaN, m), ES_GARCH)
  result <- data.frame(VaR = VaR_GARCH, ES = ES_GARCH)
  return(result)
}

#DAX
dax.garch.95 <- -VaR.GARCH(dax$st.zwrotu, okno = 250, m=25, p = 0.05, gamm=0.01, alfa=0.21, beta=0.78)
dax.garch.95$data <- dax$data[T:nrow(dax)]
dax.garch.95$st.zwrotu <- dax$st.zwrotu[T:nrow(dax)]

g7 <- ggplot(dax.garch.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="DAX - 95% VaR oraz ES - metoda GARCH",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

dax.garch.99 <- -VaR.GARCH(dax$st.zwrotu, okno = 250, m=25, p = 0.01, gamm=0.01, alfa=0.21, beta=0.78)
dax.garch.99$data <- dax$data[T:nrow(dax)]
dax.garch.99$st.zwrotu <- dax$st.zwrotu[T:nrow(dax)]

g8 <- ggplot(dax.garch.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="DAX - 99% VaR oraz ES - metoda GARCH",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()



#WIG
wig.garch.95 <- -VaR.GARCH(wig$st.zwrotu, okno = 250, m=25, p = 0.05, gamm=0.01, alfa=0.21, beta=0.78)
wig.garch.95$data <- wig$data[T:nrow(wig)]
wig.garch.95$st.zwrotu <- wig$st.zwrotu[T:nrow(wig)]

g9 <- ggplot(wig.garch.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="WIG - 95% VaR oraz ES - metoda GARCH",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

wig.garch.99 <- -VaR.GARCH(wig$st.zwrotu, okno = 250, m=25, p = 0.01, gamm=0.01, alfa=0.21, beta=0.78)
wig.garch.99$data <- wig$data[T:nrow(wig)]
wig.garch.99$st.zwrotu <- wig$st.zwrotu[T:nrow(wig)]

g10 <- ggplot(wig.garch.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="WIG - 99% VaR oraz ES - metoda GARCH",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


#SMI
smi.garch.95 <- -VaR.GARCH(smi$st.zwrotu, okno = 250, m=25, p = 0.05, gamm=0.01, alfa=0.21, beta=0.78)
smi.garch.95$data <- smi$data[T:nrow(smi)]
smi.garch.95$st.zwrotu <- smi$st.zwrotu[T:nrow(smi)]

g11 <- ggplot(smi.garch.95, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="SMI - 95% VaR oraz ES - metoda GARCH",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

smi.garch.99 <- -VaR.GARCH(smi$st.zwrotu, okno = 250, m=25, p = 0.01, gamm=0.01, alfa=0.21, beta=0.78)
smi.garch.99$data <- smi$data[T:nrow(smi)]
smi.garch.99$st.zwrotu <- smi$st.zwrotu[T:nrow(smi)]

g12 <- ggplot(smi.garch.99, aes(data)) + 
  geom_line(aes(y = VaR, colour = "VaR" )) + 
  geom_line(aes(y = ES, colour = "ES" )) + 
  geom_line(aes(y = st.zwrotu), col="#949494", alpha=0.7, size=.4) +
  xlab("") + ylab("") + 
  labs(title="SMI - 99% VaR oraz ES - metoda GARCH",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years") + theme_bw()


grid.arrange(g1,g7,g2,g8, nrow=2)
grid.arrange(g3,g9,g4,g10, nrow=2)
grid.arrange(g5,g11,g6,g12, nrow=2)


#testowanie

wyniki <- data.frame(indeks="", metoda="", alfa=0, proc.wyjatkow=0, test.Kupca=0, test.Christoffersona=0)
wyniki <- wyniki[-1,]


##############
ind="DAX"

metoda="sym. historyczna"
alfa=0.95
b <- BacktestVaR(dax.sym.hist.95$st.zwrotu, dax.sym.hist.95$VaR, alfa)
proc <- sum(dax.sym.hist.95$st.zwrotu > dax.sym.hist.95$VaR)/length(dax.sym.hist.95$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(dax.sym.hist.99$st.zwrotu, dax.sym.hist.99$VaR, alfa)
proc <- sum(dax.sym.hist.99$st.zwrotu > dax.sym.hist.99$VaR)/length(dax.sym.hist.99$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

metoda="sym. historyczna z wagami"
alfa=0.95
b <- BacktestVaR(dax.waz.sym.hist.95$st.zwrotu, dax.waz.sym.hist.95$VaR, alfa)
proc <- sum(dax.waz.sym.hist.95$st.zwrotu > dax.waz.sym.hist.95$VaR)/length(dax.waz.sym.hist.95$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(dax.waz.sym.hist.99$st.zwrotu, dax.waz.sym.hist.99$VaR, alfa)
proc <- sum(dax.waz.sym.hist.99$st.zwrotu > dax.waz.sym.hist.99$VaR)/length(dax.waz.sym.hist.99$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

metoda="EWMA"
alfa=0.95
b <- BacktestVaR(dax.ewma.95$st.zwrotu[26:nrow(dax.ewma.95)], dax.ewma.95$VaR[26:nrow(dax.ewma.95)], alfa)
proc <- sum(dax.ewma.95$st.zwrotu[26:nrow(dax.ewma.95)] > dax.ewma.95$VaR[26:nrow(dax.ewma.95)])/length(dax.ewma.95$VaR[26:nrow(dax.ewma.95)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(dax.ewma.99$st.zwrotu[26:nrow(dax.ewma.95)], dax.ewma.99$VaR[26:nrow(dax.ewma.95)], alfa)
proc <- sum(dax.ewma.99$st.zwrotu[26:nrow(dax.ewma.95)] > dax.ewma.99$VaR[26:nrow(dax.ewma.95)])/length(dax.ewma.99$VaR[26:nrow(dax.ewma.95)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

metoda="GARCH"
alfa=0.95
b <- BacktestVaR(dax.garch.95$st.zwrotu[26:nrow(dax.garch.95)], dax.garch.95$VaR[26:nrow(dax.garch.95)], alfa)
proc <- sum(dax.garch.95$st.zwrotu[26:nrow(dax.garch.95)] > dax.garch.95$VaR[26:nrow(dax.garch.95)])/length(dax.garch.95$VaR[26:nrow(dax.garch.95)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(dax.garch.99$st.zwrotu[26:nrow(dax.garch.99)], dax.garch.99$VaR[26:nrow(dax.garch.99)], alfa)
proc <- sum(dax.garch.99$st.zwrotu[26:nrow(dax.garch.99)] > dax.ewma.99$VaR[26:nrow(dax.garch.99)])/length(dax.garch.99$VaR[26:nrow(dax.garch.99)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))


#################
ind="WIG"

metoda="sym. historyczna"
alfa=0.95
b <- BacktestVaR(wig.sym.hist.95$st.zwrotu, wig.sym.hist.95$VaR, alfa)
proc <- sum(wig.sym.hist.95$st.zwrotu > wig.sym.hist.95$VaR)/length(wig.sym.hist.95$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(wig.sym.hist.99$st.zwrotu, wig.sym.hist.99$VaR, alfa)
proc <- sum(wig.sym.hist.99$st.zwrotu > wig.sym.hist.99$VaR)/length(wig.sym.hist.99$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

metoda="sym. historyczna z wagami"
alfa=0.95
b <- BacktestVaR(wig.waz.sym.hist.95$st.zwrotu, wig.waz.sym.hist.95$VaR, alfa)
proc <- sum(wig.waz.sym.hist.95$st.zwrotu > wig.waz.sym.hist.95$VaR)/length(wig.waz.sym.hist.95$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(wig.waz.sym.hist.99$st.zwrotu, wig.waz.sym.hist.99$VaR, alfa)
proc <- sum(wig.waz.sym.hist.99$st.zwrotu > wig.waz.sym.hist.99$VaR)/length(wig.waz.sym.hist.99$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

metoda="EWMA"
alfa=0.95
b <- BacktestVaR(wig.ewma.95$st.zwrotu[26:nrow(wig.ewma.95)], wig.ewma.95$VaR[26:nrow(wig.ewma.95)], alfa)
proc <- sum(wig.ewma.95$st.zwrotu[26:nrow(wig.ewma.95)] > wig.ewma.95$VaR[26:nrow(wig.ewma.95)])/length(wig.ewma.95$VaR[26:nrow(wig.ewma.95)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(wig.ewma.99$st.zwrotu[26:nrow(wig.ewma.95)], wig.ewma.99$VaR[26:nrow(wig.ewma.95)], alfa)
proc <- sum(wig.ewma.99$st.zwrotu[26:nrow(wig.ewma.95)] > wig.ewma.99$VaR[26:nrow(wig.ewma.95)])/length(wig.ewma.99$VaR[26:nrow(wig.ewma.95)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

metoda="GARCH"
alfa=0.95
b <- BacktestVaR(wig.garch.95$st.zwrotu[26:nrow(wig.garch.95)], wig.garch.95$VaR[26:nrow(wig.garch.95)], alfa)
proc <- sum(wig.garch.95$st.zwrotu[26:nrow(wig.garch.95)] > wig.garch.95$VaR[26:nrow(wig.garch.95)])/length(wig.garch.95$VaR[26:nrow(wig.garch.95)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(wig.garch.99$st.zwrotu[26:nrow(wig.garch.99)], wig.garch.99$VaR[26:nrow(wig.garch.99)], alfa)
proc <- sum(wig.garch.99$st.zwrotu[26:nrow(wig.garch.99)] > wig.ewma.99$VaR[26:nrow(wig.garch.99)])/length(wig.garch.99$VaR[26:nrow(wig.garch.99)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))



############
ind="SMI"

metoda="sym. historyczna"
alfa=0.95
b <- BacktestVaR(smi.sym.hist.95$st.zwrotu, smi.sym.hist.95$VaR, alfa)
proc <- sum(smi.sym.hist.95$st.zwrotu > smi.sym.hist.95$VaR)/length(smi.sym.hist.95$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(smi.sym.hist.99$st.zwrotu, smi.sym.hist.99$VaR, alfa)
proc <- sum(smi.sym.hist.99$st.zwrotu > smi.sym.hist.99$VaR)/length(smi.sym.hist.99$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

metoda="sym. historyczna z wagami"
alfa=0.95
b <- BacktestVaR(smi.waz.sym.hist.95$st.zwrotu, smi.waz.sym.hist.95$VaR, alfa)
proc <- sum(smi.waz.sym.hist.95$st.zwrotu > smi.waz.sym.hist.95$VaR)/length(smi.waz.sym.hist.95$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(smi.waz.sym.hist.99$st.zwrotu, smi.waz.sym.hist.99$VaR, alfa)
proc <- sum(smi.waz.sym.hist.99$st.zwrotu > smi.waz.sym.hist.99$VaR)/length(smi.waz.sym.hist.99$VaR)*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

metoda="EWMA"
alfa=0.95
b <- BacktestVaR(smi.ewma.95$st.zwrotu[26:nrow(smi.ewma.95)], smi.ewma.95$VaR[26:nrow(smi.ewma.95)], alfa)
proc <- sum(smi.ewma.95$st.zwrotu[26:nrow(smi.ewma.95)] > smi.ewma.95$VaR[26:nrow(smi.ewma.95)])/length(smi.ewma.95$VaR[26:nrow(smi.ewma.95)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(smi.ewma.99$st.zwrotu[26:nrow(smi.ewma.95)], smi.ewma.99$VaR[26:nrow(smi.ewma.95)], alfa)
proc <- sum(smi.ewma.99$st.zwrotu[26:nrow(smi.ewma.95)] > smi.ewma.99$VaR[26:nrow(smi.ewma.95)])/length(smi.ewma.99$VaR[26:nrow(smi.ewma.95)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

metoda="GARCH"
alfa=0.95
b <- BacktestVaR(smi.garch.95$st.zwrotu[26:nrow(smi.garch.95)], smi.garch.95$VaR[26:nrow(smi.garch.95)], alfa)
proc <- sum(smi.garch.95$st.zwrotu[26:nrow(smi.garch.95)] > smi.garch.95$VaR[26:nrow(smi.garch.95)])/length(smi.garch.95$VaR[26:nrow(smi.garch.95)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))
alfa=0.99
b <- BacktestVaR(smi.garch.99$st.zwrotu[26:nrow(smi.garch.99)], smi.garch.99$VaR[26:nrow(smi.garch.99)], alfa)
proc <- sum(smi.garch.99$st.zwrotu[26:nrow(smi.garch.99)] > smi.ewma.99$VaR[26:nrow(smi.garch.99)])/length(smi.garch.99$VaR[26:nrow(smi.garch.99)])*100
wyniki[nrow(wyniki)+1,] <- c(ind, metoda, alfa, round(proc,3), round(b$LRuc["Pvalue"],3), round(b$LRcc["Pvalue"],3))

wyniki


