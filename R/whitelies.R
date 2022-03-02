#' Disguise the sample mean and sample deviation
#'
#' Disguises the sample mean and standard deviation via a choice of methods.
#'
#'
#' @param usersample A vector of all individual sample values.
#'
#' @param method Approach for disguising mean and standard deviation. (default = 1)
#'
#' @return Outputs disguised mean and disguised standard deviation.
#'
#' @export
#'
#'
#' @details
#'
#' *Method 1*
#'
#' Randomly split the sample into two (approx. equal size) samples A, and B.
#' For sample A calculate and report mean. For sample B calculate and
#' standard deviation.
#'
#'
#' *Method 2* (default)
#'
#' Take a sample of size N with replacement; calculate and report mean.
#' Repeat to calculate and report standard deviation.
#'
#'
#' *Method 3*
#'
#' Generate a random number (RN1) between N/2 and N. Sample with
#' replacement a sample size of RN1; calculate and report mean.
#' Generate a random number (RN2) between N/2 and N. Sample with
#' replacement a sample size of RN2; calculate and report standard deviation.
#'
#'
#' *Method 4*
#'
#' As Method 3, but sampling without replacement.
#'
#' @examples
#'
#' usersample<-c(1,1,2,3,4,4,5)
#'
#' disguise(usersample,method=1)
#' disguise(usersample,method=2)
#' disguise(usersample,method=3)
#' disguise(usersample,method=4)
#'
#'
#' @references
#' Derrick, B., Green, L., Kember, K., Ritchie, F. & White P, 2022, Safety in numbers: Minimum thresholding, Maximum bounds, and Little White Lies.
#' Scottish Economic Society Annual Conference, University of Glasgow, 25th-27th April 2022


disguise<-function(usersample,method=2){
  n<-length(usersample)
  #### method 1 #####
  if (method == 1){
    ind<-sample(1:n,size=ceiling(n/2), replace = FALSE)
    split<-sort(ind)
    SampleA<-usersample[split]
    SampleB<-usersample[-split]
    SampleAmean<-mean(SampleA)
    SampleBsd<-sd(SampleB)
  }

  if (method == 2){
    #### method 2 #####
    SampleA<-sample(usersample, size =n, replace = TRUE)
    SampleAmean<-mean(SampleA)
    SampleB<-sample(usersample, size =n, replace = TRUE)
    SampleBsd<-sd(SampleB)
  }

  if (method == 3){
    ### method 3 ####
    rnd1<-round(runif(1,min=n/2,max=n),0)
    SampleA<-sample(usersample, size =rnd1, replace = TRUE)
    SampleAmean<-mean(SampleA)
    rnd2<-round(runif(1,min=n/2,max=n),0)
    SampleB<-sample(usersample, size =rnd2, replace = TRUE)
    SampleBsd<-sd(SampleB)
  }

  if (method == 4){
    ### method 4 ####
    rnd1<-round(runif(1,min=n/2,max=n),0)
    SampleA<-sample(usersample, size =rnd1, replace = FALSE)
    SampleAmean<-mean(SampleA)
    rnd2<-round(runif(1,min=n/2,max=n),0)
    SampleB<-sample(usersample, size =rnd2, replace = FALSE)
    SampleBsd<-sd(SampleB)
  }

  print(paste0("mean = ",round(SampleAmean,1)))
  print(paste0("sd = ",round(SampleBsd,1)))
}


