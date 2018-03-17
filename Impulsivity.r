# Tatenda Ndambakuwa
#Stat 321
# Project 2

# To read in an excel sheet we first install related packages

install.packages("readxl")
library("readxl")

read_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
} 

# if we were reading from C:\TempData we would say sites <- read_allsheets("~/TempData/Impulsivity.xlsx")

#read in all the sites from the data

sites <- read_allsheets("~/Downloads/Impulsivity.xlsx")

s1 <- sites$`Site 1`
s2 <- sites$`Site 2`
s3 <- sites$`Site 3`
s4 <- sites$`Site 4`
s5 <- sites$`Site 5`

sitesstack <- rbind(s1, s2, s3, s4, s5)

#1 - A histogram of Impulsivity at each visit time

hist(sitesstack$Imp1,
     xlim = c(60,80),
     main = "Trial 1",
     xlab = "Imp 1",
     col = "blue") 


###############
# Shape: Uniform Distribution
###############
# summary(sitesstack$Imp1)
#    Min.  1st Qu.  Median  Mean    3rd Qu.   Max. 
#   65.00   70.00   72.00   71.94   73.50   79.00 
###############
# Unusual Features: No outliers
# Spread is from 65-79
# Center: 72
##############


hist(sitesstack$Imp2,
     xlim = c(55,80),
     main = "Trial 2",
     xlab = "Imp 2",
     col = "red")
###############
#summary(sitesstack$Imp2)
#Min.  1st Qu.  Median    Mean   3rd Qu.  Max. 
#59.00   66.00   68.00   68.07   70.00   77.00
#
# Spread: 59 to 77
# Unusual Features: No outliers
# Shape: Uniform Distribution
# Center=68.00
###############

hist(sitesstack$Imp3,
     xlim = c(55,75),
     main = "Trial 3",
     xlab = "Imp 3",
     col = "yellow")
#summary(sitesstack$Imp3)
#Min.   1st Qu.  Median  Mean   3rd Qu.   Max. 
#56.00   63.00   65.00   65.08   67.00   73.00 
# Shape: Bimodal
# Center: 65
# Spread: 56 to 73
# Unusual Features: Two peaks


hist(sitesstack$Imp4,
     xlim = c(50,76),
     main = "Trial 4",
     xlab = "Imp 4",
     col = "brown")

#summary(sitesstack$Imp4)
#Min.   1st Qu. Median   Mean    3rd Qu.  Max. 
#58.00   63.00   65.00   64.81   67.00   72.00 
# Unusual Features: Slightly skewed
# Shape: Skewed Right
# Center: About 65
# Spread: 58 to 72


 
hist(sitesstack$Imp5,
     xlim = c(50,75),
     main = "Trial 5",
     xlab = "Imp 5",
     col = "purple")

#summary(sitesstack$Imp5)
#Min.    1st Qu.  Median    Mean   3rd Qu.    Max. 
#53.00   58.00    60.00    60.03   62.00     70.00 
# Shape: Slightly Skewed Right
# Center=60
# Spread is from 53 to 70
# Unusual Features: Slightly skewed right

# End of 1
###########################################################################
#2 - Boxplots comparing across visits

#In order for it to be across all visits must do a wide to long
im1 <- sitesstack[ , c(1,2,3,4,5,6)]
im2 <- sitesstack[ , c(1,2,3,4,5,7)]
im3 <- sitesstack[ , c(1,2,3,4,5,8)]
im4 <- sitesstack[ , c(1,2,3,4,5,9)]
im5 <- sitesstack[ , c(1,2,3,4,5,10)]

#Changing column name

names( im1 )[6] <- "Imp"
names( im2 )[6] <- "Imp"
names( im3 )[6] <- "Imp"
names( im4 )[6] <- "Imp"
names( im5 )[6] <- "Imp"

#Combining all of them into one wide to long data set

impstack <- rbind( im1, im2, im3, im4, im5)

# 2 - Boxplots comparing impulsivity across visits
boxplot(sitesstack$Imp1, sitesstack$Imp2, sitesstack$Imp3, sitesstack$Imp4,
        sitesstack$Imp5, 
        names = c("IMP1", "IMP2", "IMP3", "IMP4", "IMP5"),
        ylab = "Impulsivity",
        main = "Impulsivity 1-5",
        col = c("blue","grey","orange","black", "green"))  

#IMP5
# Unusual Features: One outlier at 71
# Shape: Uniform Distribution
# Center: 60
# Spread: 53 to 70

#IMP4
# Center:65
# Spread: 58 to 72
# Unusual Features: No Unusual features
# Shape: Uniform Distribution

#IMP3
# Shape: Skewed Right
# Center: About 65
# Spread: 60 to 74
# Unusual Features: One Outlier at 56

#IMP2
# Shape: Skewed Right
# Center: 68
# Spread: 59 to 77
# Unusual Features: Two outliers at 59 & 77

#IMP1
# Shape: Slightly Skewed Right
# Center: About 70 to 74
# Spread: 65 to 78
# Unusual Features: One outlier at about 79

###########################################################################
# 3 - boxplot comparing Impulsivity across TRT where TRT refers to treatments
boxplot(impstack$IMP ~ impstack$TRT,
        xlab = "Treatments",
        ylab = "Frequency",
        main ="Impusivity vs Treatment Group",
        col = c("pink","yellow", "grey")) 

#C-TRT
# Shape: Uniform Distribution
# Center: About 66
# Spread: 55 to 79
# Unusual Features: No Unusual features

#L-TRT
# Shape: Slightly Skewed Left
# Center: About 66
# Spread: 55 to 79
# Unusual Features: Slightly Skewed Left

#N-TRT
# Shape: Skewed Left
# Center: About 66
# Spread: 55 to 76
# Unusual Features: Slightly Skewed left
###########################################################################
# 4 - boxplot comparing impulsivity across gender
boxplot(impstack$Imp ~ impstack$Gender,
        xlab = "Gender",
        ylab = "Impulse Frequency",
        main ="Impusivity vs Frequency across Gender",
        col = c("pink","blue"))
#M for Males
# Center: Around 66
# Unusual Features: No Unusual Features
# Spread: 53 to 79
# Shape: Uniform Distribution

#F for Females
# Unusual Features: 2 outliers at 51 and 79
# Shape: Uniform Distribution
# Spread: 53 to 77
# Center: Around 66

###########################################################################

