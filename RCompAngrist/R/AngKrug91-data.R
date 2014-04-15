#' @name AngKrug91
#' @title Data used in Angrist and Krueger 1991
#' @description TODO
#' @docType data
#' @usage AngKrug91
#' @format A data frame containing 329509 observations and 5 variables. 
#' @details TODO
#' @references Krueger, A. (1999) "Experimental Estimates Of Education Production Functions," 
#' \emph{The Quarterly Journal of Economics}, Vol. 114(2), pages 497-532, May.
#' @references Angrist, A. and Krueger, A. (1991) Does Compulsory Schooling Attendance Affect 
#' Schooling and Earnings?, \emph{Quarterly Journal of Economics}, 106, 976-1014.
#' @source TODO

NULL




# AK91 <- read.table("~/Dropbox/Documents/stats/R/RcompAngrist/Datasets/Angrist, Krueger 1991/asciiqob.txt")
# head(AK91)
# dim(AK91)
# 
# 
# colnames(AK91) <- c("lwklywge", "educ", "yob", "qob", "pob")
# AK91$yqob <- AK91$yob + AK91$qob/4
# AK91_b <- AK91
# AK91_b$yob <- as.factor(AK91$yob)
# AK91_b$qob <- as.factor(AK91$qob)
# 
# save(AK91_b, file="/home/matifou/Téléchargements/AngKrug91.rda")
