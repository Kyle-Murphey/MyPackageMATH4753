#' Squares the entries of a vector
#'
#' This function returns the square of a vector.
#' @param x a vector for squaring
#'
#' @return returns squared vector
#' @export
#'
#' @examples mysquarefun(someVector)
mysquarefun = function(x)
{
  x^2
}

#################################################################################################

#' binomial sampling function
#'
#' This uses a sample to make a binomial simulation.
#' @param iter iterations
#' @param n number of trials
#' @param p probability of success
#'
#' @return returns a graph of the simulation with proportions and a table of proportions.
#' @export
#'
#' @examples mybin(iter=1000,n=30,p=0.7)
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}

###############################################################################################

#' Generates a pareto graph
#'
#' This function creates a pareto graph from the given vector x.
#' @param x vector of data to use
#' @param mn title of barplot
#' @param ... whatever else you want to add to the title
#'
#' @return a pareto graph
#' @export
#'
#' @examples pareto(x1,mn="graph")
pareto<-function(x,mn="Pareto barplot",...){  # x is a vector
  x.tab=table(x)
  xx.tab=sort(x.tab, decreasing=TRUE,index.return=FALSE) # sort the table
  cumsum(as.vector(xx.tab))->cs #cumulative sums of table
  length(x.tab)->lenx
  bp<-barplot(xx.tab,ylim=c(0,max(cs)),las=2) #make the barplot
  lb<-seq(0,cs[lenx],l=11) #points to draw tick marks
  axis(side=4,at=lb,labels=paste(seq(0,100,length=11),"%",sep=""),las=1,line=-1,col="Blue",col.axis="Red")
  for(i in 1:(lenx-1)){
    segments(bp[i],cs[i],bp[i+1],cs[i+1],col=i,lwd=2)
  }
  title(main=mn,...)

}

#################################################################################################

#' My chi-squared
#'
#' This function returns a chi-squared graph with a theoretical and simulated line.
#' @param n1 sample size
#' @param sigma1 standard deviation
#' @param mean1 mean
#' @param iter iterations
#' @param ymax max y value
#'
#' @return Chi-squared graph of the values with a theoretical line and simulated line
#' @export
#'
#' @examples mychisim(30,4,5,10000,0.15)
mychisim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1){    # adjust ymax to make graph fit
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1

  ssq1=apply(data1.mat,2,var) # ssq1 is s squared

  w=(n1-1)*ssq1/sigma1^2      #chi-sq stat

  hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(dchisq(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  title=expression(chi^2==frac((n[1]-1)*s^2,sigma^2)) #mathematical annotation -see ?plotmath
  legend(x=25,y=0.07,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #
  return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq")) # some output to use if needed
}

###################################################################################################

#' central limit tendency uniform function
#'
#' This function returns a CLT graph with theoretical and simulated density curves.
#' @param n size of sample
#' @param iter iterations
#' @param a lower limit
#' @param b upper limit
#'
#' @return a histogram with a theoretical and simulated line to show density curve
#' @export
#'
#' @examples mycltu(10,100)
mycltu=function(n,iter,a=0,b=10){
  ## r-random sample from the uniform
  y=runif(n*iter,a,b)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax
  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  ## add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  ## Add the density from which the samples were taken
  curve(dunif(x,a,b),add=TRUE,lwd=4)

}
