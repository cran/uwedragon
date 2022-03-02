#' Find individual sample values from the sample mean and standard deviation
#'
#' For integer based scales, finds possible solutions for each value within a sample.
#' This is revealed upon providing sample size, minimum possible value, maximum possible value,
#' mean, standard deviation (and optionally median).
#'
#'
#' @param n Sample size.
#'
#' @param min_poss Minimum possible value. If sample minimum is disclosed, this can be inserted here, otherwise use the theoretical minimum. If there is no theoretical maximum 'Inf' can be inserted.
#'
#' @param max_poss Maximum possible value. If sample maximum is disclosed, this can be inserted here, otherwise use the theoretical maximum. If there is no theoretical minimum '-Inf' can be inserted.
#'
#' @param usermean Sample mean.
#'
#' @param usersd Sample standard deviation, i.e. n-1 denominator.
#'
#' @param meandp (optional, default=NULL) Number of decimal places mean is reported to, only required if including trailing zeroes.
#'
#' @param sddp (optional, default=NULL) Number of decimal places standard deviation is reported to, only required if including trailing zeroes.
#'
#' @param usermed (optional, default=NULL) Sample median.
#'
#' @return Outputs possible combinations of original integer sample values.
#'
#' @export
#'
#'
#' @details
#'
#'
#' For use with data measured on a scale with 1 unit increments.
#' Samuelson's inequality [1] used to further restrict the minimum and maximum.
#' All possible combinations within this inequality are calculated [2] for
#' factorial(n+k-1)/(factorial(k)*factorial(n-1))<65,000,000.
#'
#' No restriction on number of decimal places input. Reporting less than
#' two decimal places will reduce the chances of unique solution to all
#' sample values being uncovered [3]
#'
#' Additional options to specify number of digits following the decimal place that are reported,
#' required for trailing zeroes.
#'
#' @examples
#'
#' # EXAMPLE 1
#' # Seven observations are taken from a five-point Likert scale (coded 1 to 5).
#' # The reported mean is 2.857 and the reported standard deviation is 1.574.
#'
#' solutions(7,1,5,2.857,1.574)
#'
#' # For this mean and standard deviation there are two possible distributions:
#' # 1  1  2  3  4  4  5
#' # 1  2  2  2  3  5  5
#'
#' # Optionally adding median value of 3.
#'
#' solutions(7,1,5,2.857,1.574, usermed=3)
#'
#' # uniquely reveals the raw sample values:
#' # 1  1  2  3  4  4  5
#'
#'
#' # EXAMPLE 2
#' # The mean is '4.00'.
#' # The standard deviation is '2.00'.
#' # Narrower set of solutions found specifying 2dp including trailing zeroes.
#'
#' solutions(3,-Inf,Inf,4.00,2.00,2,2)
#'
#' # uniquely reveals the raw sample values:
#' # 2  4  6
#'
#' @references
#'
#' [1] Samuelson, P.A, 1968, How deviant can you be? Journal of the American Statistical Association, Vol 63, 1522-1525.
#'
#' [2] Allenby, R.B. and Slomson, A., 2010. How to count: An introduction to combinatorics. Chapman and Hall/CRC.
#'
#' [3] Derrick, B., Green, L., Kember, K., Ritchie, F. & White P, 2022, Safety in numbers: Minimum thresholding, Maximum bounds, and Little White Lies.
#' Scottish Economic Society Annual Conference, University of Glasgow, 25th-27th April 2022
#'

solutions<- function(n, min_poss, max_poss, usermean, usersd, meandp=NULL, sddp=NULL, usermed=NULL) {

#ensure valid data entry
  if (min_poss > max_poss)
    stop("Check data input. Minimum cannot be greater than Maximum")

  if ((is.null(n)) | (is.null(min_poss)) | (is.null(max_poss)) | (is.null(usermean)) | (is.null(usersd)))
    stop("N, Minimum, Maximum, Mean and Standard Deviation all required")

#check Samuelson's inequality and adjust min / max if this reduces the range
  min_poss_s<- floor(usermean-((sqrt(n-1))*usersd))
  max_poss_s<- ceiling(((sqrt(n-1))*usersd) + usermean)
  if(min_poss_s> min_poss){
    min_poss_f<- min_poss_s
  }
    else{
      min_poss_f<- min_poss
      }
  if(max_poss_s< max_poss){
    max_poss_f<-max_poss_s
  }
    else{
      max_poss_f<-max_poss
      }
  k<-length(min_poss_f:max_poss_f)

#stop user if combinations too large for R to store in memory
  if (factorial(n+k-1) == Inf)
    stop("Sample size or potential range of values too large to compute solutions")
  if (factorial(n+k-1)/(factorial(k)*factorial(n-1))>65000000)
    stop("Sample size or potential range of values too large to compute solutions")

#apply number of decimal places as determined by user input (where meandp and sddp not specified)
  decimalplaces <- function(x) {
    if (abs(x - round(x)) > .Machine$double.eps^0.5) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }

#generate all combinations
  samples<-gtools::combinations(k, n, min_poss_f:max_poss_f, repeats.allowed=TRUE)
  samples<-data.frame(samples)

#calculate summary statistics for all combinations and compare to users stated summary statistics
  if (is.null(usermed)){
    all<-transform(samples, means=apply(samples, 1, mean))
    all2<-transform(all, sd=apply(samples, 1, sd))
      if (is.null(meandp) & is.null(sddp)) {
        all2$combine<-paste0(round(all2$means,decimalplaces(usermean)),round(all2$sd,decimalplaces(usersd)))
        user<-paste0(round(usermean,decimalplaces(usermean)),round(usersd,decimalplaces(usersd)))
        }
      else{
        if (is.null(meandp) | is.null(sddp))
          stop("If either specified, number of decimal places must be specified for both mean and standard deviation")
        if (meandp > round(meandp) | meandp < round(meandp) | sddp > round(sddp) | sddp < round(sddp)  )
          stop("Data entry incorrect. Number of decimal places must be integer")
      all2$combine<-paste0(round(all2$means,meandp),round(all2$sd,sddp))
      user<-paste0(round(usermean,meandp),round(usersd,sddp))
      }
    if(nrow(all[which(all2$combine == user ),c(1:n)])==0){
      warning("No solutions found: data input is incorrect or mean and standard deviation disguised")
    }
    return(all[which(all2$combine == user ),c(1:n)])
  }
  #if including median
  else {
    all<-transform(samples, means=apply(samples, 1, mean))
    all2<-transform(all, sd=apply(samples, 1, sd))
    all2<-transform(all2, med=apply(samples, 1, median))
    if (is.null(meandp) & is.null(sddp)) {
      all2$combine<-paste0(round(all2$means,decimalplaces(usermean)),round(all2$sd,decimalplaces(usersd)),round(all2$med,decimalplaces(usermed)))
      user<-paste0(round(usermean,decimalplaces(usermean)),round(usersd,decimalplaces(usersd)),round(usermed,decimalplaces(usermed)))
      }
    else{
      if (is.null(meandp) | is.null(sddp))
        stop("If either specified, number of decimal places must be specified for both mean and standard deviation")
      if (meandp > round(meandp) | meandp < round(meandp) | sddp > round(sddp) | sddp < round(sddp)  )
        stop("Data entry incorrect. Number of decimal places must be integer")
      all2$combine<-paste0(round(all2$means,meandp),round(all2$sd,sddp),round(all2$med,decimalplaces(usermed)))
      user<-paste0(round(usermean,meandp),round(usersd,sddp), round(usermed,decimalplaces(usermed)))
    }
    if(nrow(all[which(all2$combine == user ),c(1:n)])==0){
      warning("No solutions found: data input is incorrect or mean and standard deviation disguised")
    }
    return(all[which(all2$combine == user ),c(1:n)])
  }
}

