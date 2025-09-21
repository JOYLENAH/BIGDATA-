
#BABIRYE JOY LENAH
#B28490

install.packages("ggplot2")
library(ggplot2)
 install.packages("readxl")
 library(readxl)
 install.packages("dplyr")
 library(dplyr)
install.packages("writexl")
install.packages("ggplot2")
library(ggplot2)
df <- read.csv("C:/Users/DIL/Downloads/ida_credits_to_uganda_09-20-2025.csv")


library(writexl)
#NUMBER1
#running a time series that explains the trend
install.packages("scales")
library(scales)

head(df$`End.of.Period`)
df$End.of.Period <- as.Date(df$End.of.Period, format = "%m/%d/%Y")


df$Year <- format(df$`End of Period`, "%Y")

# Clean column names first
names(df) <- make.names(names(df))

# Convert End.of.Period to Date (using lubridate for safety)
install.packages("lubridate")
library(lubridate)
df$End.of.Period <- mdy(df$End.of.Period)
df$Year <- year(df$End.of.Period)
colnames(df)
nrow(df)
summary(df$`Disbursed Amount (US$)`)
#removing the null before aggreagating
Disbursement_trend <- aggregate(
  `Disbursed.Amount..US..` ~ Year,
  data = df[!is.na(df$`Disbursed.Amount..US`), ],
  FUN = sum
)


disbursement_trend <- aggregate(Disbursed.Amount..US.. ~ Year, data = df, FUN = sum)
names(df)

head(disbursement_trend)

ggplot2(disbursement_trend, aes(x=Group.1, y=x)) +
  geom_line(color="blue") +
  geom_point() +
  labs(title="Trend of World Bank Disbursement to Uganda",
       x="Year", y="Disbursed.Amount..US..")

# Loading the package
library(ggplot2)

# Plot your disbursement trend
ggplot(disbursement_trend, aes(x = Group.1, y = x)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(
    title = "Trend of World Bank Disbursement to Uganda",
    x = "Year",
    y = "Disbursed Amount (US$)"
  )

#number2
#Overall Credit Status of Uganda 

# the distribution of credit status is:
#Fully Repaid → 96 projects
#Repaying → 65 projects
#Disbursing → 10 projects
#Disbursing & Repaying → 8 projects
#Others (Cancelled, Terminated, Approved, Effective) → <10 
#most credits in the dataset are either already fully repaid or are in the process of being repaid. A smaller number are still being disbursed. This signals a mature portfolio where many historical credits have run their course (repaid) while a significant share remains active (repaying or disbursing).
names(df) <- make.names(names(df))
table(df$Credit.Status)
names(df) <- make.names(names(df))
barplot(table(df$Credit.Status),
        col="skyblue",
        main="Credit Status of Uganda's World Bank Loans",
        ylab="Number of Projects")


#number3
#Original Principal Amount borrowed
#The total original principal amount in 2025 is ≈ US$ 11.9 billion.

#Over time, the borrowing pattern shows progressive increases in principal amounts, indicating Uganda’s growing financing needs.
#The dataset suggests larger and more frequent borrowing in recent decades, aligning with infrastructure, education, and development programs.


names(df)

head(df$End.of.Period)
table(df$Year)

head(principal_trend)

#correct
str(df)
names(df)
principal_trend <- aggregate(df$Original.Principal.Amount..US..,
                             by = list(Year = df$Year), FUN = sum)
nrow(df)
sum(is.na(df$Original.Principal.Amount..US...))
ggplot(principal_trend, aes(x=Year, y=x)) +
  geom_bar(stat="identity", fill="darkgreen") +
  labs(title="Original Principal Amount Borrowed from World Bank",
       x="Year", y="Principal Amount (US$)")

#Mean (average) Original Principal: ≈ $62,198,592.

#Median: $29,000,000.

#25th percentile: ≈ $12,435,000; 75th percentile: ≈ $85,000,000.

#Minimum: $0 (some records have 0), Maximum: $518,000,000 (largest single project).

#Pattern: the distribution is right-skewed (a majority of projects are smaller to medium size, while a few very large loans drive the mean upward). That is, many smaller projects (tens of millions) and several large projects (hundreds of millions).

#Top projects: the largest original principals (from the dataset) are recent large programs, for example:
  
#GKMA Urban Development Program — $518,000,000 (Board Approval 2022),

#UG — Support to Municipal Infra Dev — $335,000,000 (2018),

#Electricity Access Scale-up Project — $331,500,000 (2022),

#and several others ~ $240–$330M (including COVID recovery financing around 2020).

