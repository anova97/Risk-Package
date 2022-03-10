#' Return Rates of EUR
#'
#' A dataset containing the daily return rates of EUR
#'  calculated based on daily data from the begining of 2012
#'  to the end of 2018
#'
#' \itemize{
#'   \item date, date of return rate in format YYYYMMDD
#'   \item rate, exchange rate of EUR in PLN
#'   \item dailyReturnRate, daily return rate
#' }
#'
#' @docType data
#' @keywords datasets
#' @name returnRateEUR
#' @usage data(returnRateEUR)
#' @format A data frame with 1763 rows and 3 variables
#'

Risk<-function(data) UseMethod("Risk")

## Risk function

#' Calculating for given rates of return the value at risk and the expected loss using the historical method.
#'
#' @param data A dataframe object with column dailyReturnRate
#' @param time A number of time window.
#' @return A data frame with calculated VaR and ES
#' @examples
#' #data("returnRateEUR")
#' #Risk(returnRateEUR, 200)
#' @keywords Risk
#' @export
#'
Risk<-function(data, time){
  # Args: data - macierz danych z kolumną dailyReturnRate zawierającą stopy zwrotu
  # time - długosc okna czasowego
  # Result: macierz wejściowa z dodatkową kolumną z obliczoną wartością VaR i ES
  # Sprawdzanie poprawności argumentów
  stopifnot(is.numeric(data$dailyReturnRate))
  stopifnot(is.numeric(time))
  stopifnot(time<nrow(data))
  stopifnot(time>1)
  # Wyznaczanie wartości i zapisanie wyniku do listy
  varData<-NULL
  ESData<-NULL

  for(i in 1:(nrow(data)-time-1))
  {
    okno<-data$dailyReturnRate[i:(i+time-1)]
    var<-quantile(okno,0.01)
    varData<-c(varData,var)
    ES<-mean(okno[which(okno<var)])
    ESData<-c(ESData,ES)
  }
  wynik<-data[1:length(varData),]
  wynik$VAR<-varData
  wynik$ES<-ESData

  # Definiowanie klasy
  class(wynik)<-append(class(wynik),'Risk')
  return(wynik)
}


#' Print calculated ccalues of VaR or ES
#'
#' @param data A dataframe object with column dailyReturnRate
#' @param time A number of time window.
#' @param typ A character - VaR or ES
#' @return calculated values of VaR or ES
#' @examples
#' #data("returnRateEUR")
#' #printRisk(returnRateEUR, 500, typ="VaR")
#' @keywords Risk
#' @export
#'
printRisk <- function(data, time, typ)
{ # Wypisuje dwie miary ryzyka
  # w zaleźności od podadego typu VaR, lub ES
  wynik<-Risk(data, time)
  #wyświetlanie wszystkich wartości
  options(max.print=999999)
  if(typ=="VaR")
  {
    cat("VaR:\n")
    print(wynik$VAR)
  }
  if(typ=="ES")
  {
    cat("\nES:\n")
    print(wynik$ES)
  }
}


#' Create a graph of calculated values of VaR and ES
#'
#' @param data A dataframe object with column dailyReturnRate
#' @param time A number of time window.
#' @return graph of calculated values of VaR or ES
#' @examples
#' #data("returnRateEUR")
#' #plotRisk(returnRateEUR, 500)
#' @keywords Risk
#' @importFrom "stats" "quantile"
#' @import ggplot2
#' @import lubridate
#' @export
#'
plotRisk<-function(data, time){
  # Rysowanie wykresu dla VaR i ES
  # Args: data - elemnt klasy Risk
  # time - dlugosc okna czasowego
  # Result: Wykres VaR i ES
  # przygotowanie nowego dataframe do wykresu
  rodzaj=0
  wartosc=0
  if (requireNamespace(c("ggplot2", "lubridate"), quietly=TRUE))
  {
    wynik<-Risk(data, time)
    n<-nrow(wynik)
    VarES<-c(rep("VAR",n), rep("ES",n))
    wynik[,1]<-lubridate::ymd(wynik[,1])
    daneVarES<-data.frame("wartosc"=c(wynik$VAR, wynik$ES), "rodzaj"= VarES,  "data_poczatku_okna"=rep(wynik[,1],2))

    wykres<-ggplot2::ggplot(daneVarES, aes(y=wartosc, x=daneVarES[,3], col=rodzaj))+geom_line()+
      ggtitle("Wykres VaR i ES dla metody historycznej")+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(x = "Data poczatku okna", y = "wartosc")+theme_linedraw()
    return(wykres)
  }
}



