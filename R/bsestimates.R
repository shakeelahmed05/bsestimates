#' Bootstrap properties of estimators
#'
#' @param data  Required Data Frame with at least 2 variables 
#' @param n Sample size
#' @param sim No of simulated samples
#'
#' @return  The bias, mean squared error, relative efficiency and Percentage contribution of bias
#' @export
#'
#' @examples bsestimates(data.frame(c(2,3,1,2,3,4,5,5,6,6), c(1,1,2,3,4,4,5,3,3,5)),5, 100)
bsestimates<-function(data, n, sim){
  M=mean(data[,2])
  M_0x<-c()
  M_0y<-c()
  M_r<-c()
  M_reg<-c()
  M_x<-mean(data[,1])
  Beta<-cov(data[,1], data[,2])/var(data[,2])
  R=c(1:sim)
  for (i in R){
    x<-sample(data[,1],n)
    y<-sample(data[,2],n)
    M_0x[i]<-sum(x)/n
    M_0y[i]<-sum(y)/n
    M_r[i]<-M_x*M_0y[i]/M_0x[i]
    M_reg[i]<-M_0y[i]+Beta*(M_x-M_0x[i])
  }
  
  summary_1<-list(simple=data.frame(bias=mean(M_0y-M), mse=mean((M_0y-M)^2),re=mean((M_0y-M)^2)/mean((M_0y-M)^2), PCOB=100*((mean(M_0y-M))^2)/mean((M_0y-M)^2)),
                  ratio=data.frame(bias=mean(M_r-M)
                                   , mse=mean((M_r-M)^2), re=mean((M_0y-M)^2)/mean((M_r-M)^2), PCOB=100*((mean(M_r-M))^2)/mean((M_r-M)^2)),
                  regression=data.frame(bias=mean(M_reg-M)
                                        , mse=mean((M_reg-M)^2), re=mean((M_0y-M)^2)/mean((M_reg-M)^2), PCOB=100*((mean(M_reg-M))^2)/mean((M_reg-M)^2))   )
  return(summary_1)
}
