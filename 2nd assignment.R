## Harvard Data Science Project
## Jerry Xia
## Second Assignment
## 07/20/2019

library(foreign)
library(ggm)

# Read in Demographic data
DEMO <- read.xport("C:/Users/apple/Downloads/DEMO_I.XPT")


# Read in Dietary Data
DR1TOT <- read.xport("C:/Users/apple/Downloads/DR1TOT_I.XPT")

DR2TOT <- read.xport("C:/Users/apple/Downloads/DR2TOT_I.XPT")

DR2FF <- read.xport("C:/Users/apple/Downloads/DR2IFF_I.XPT")

DR1FF <- read.xport("C:/Users/apple/Downloads/DR1IFF_I.XPT")

# Merge each dietary data with demographic data
d1 <- merge(DEMO,DR1TOT,by="SEQN")
d2 <- merge(DEMO,DR2TOT,by="SEQN")
d3 <- merge(DEMO,DR2FF,by="SEQN")
d4 <- merge(DEMO,DR1FF,by="SEQN")

# Correlation, Covariance, and Partial Correlation matrix of DEMO and DR1TOT
DandD1 <- cor(d1)
DandD2 <- cov(d1)


# Correlation, Covariance, and Partial Correlation matrix of DEMO and DR2TOT
DandD3 <- cor(d2)
DandD4 <- cov(d2)

# Correlation, Covariance, and Partial Correlation matrix of DEMO and DR2FF
DandD5 <- cor(d3)
DandD6 <- cov(d3)

# Correlation, Covariance, and Partial Correlation matrix of DEMO and DR1FF
DandD7 <- cor(d4)
DandD8 <- cov(d4)


# Read in Questionnaire data
ACQ <- read.xport("C:/Users/apple/Downloads/ACQ_I.XPT")

ALQ <- read.xport("C:/Users/apple/Downloads/ALQ_I.XPT")

AUQ <- read.xport("C:/Users/apple/Downloads/AUQ_I.XPT")

BPQ <- read.xport("C:/Users/apple/Downloads/BPQ_I.XPT")

CDQ <- read.xport("C:/Users/apple/Downloads/CDQ_I.XPT")

CBQ <- read.xport("C:/Users/apple/Downloads/CBQ_I.XPT")

HSQ <- read.xport("C:/Users/apple/Downloads/HSQ_I.XPT")

DEQ <- read.xport("C:/Users/apple/Downloads/DEQ_I.XPT")

DIQ <- read.xport("C:/Users/apple/Downloads/DIQ_I.XPT")

DBQ <- read.xport("C:/Users/apple/Downloads/DBQ_I.XPT")

DLQ <- read.xport("C:/Users/apple/Downloads/DLQ_I.XPT")

DUQ <- read.xport("C:/Users/apple/Downloads/DUQ_I.XPT")

ECQ <- read.xport("C:/Users/apple/Downloads/ECQ_I.XPT")

HIQ <- read.xport("C:/Users/apple/Downloads/HIQ_I.XPT")

HEQ <- read.xport("C:/Users/apple/Downloads/HEQ_I.XPT")

HUQ <- read.xport("C:/Users/apple/Downloads/HUQ_I.XPT")

HOQ <- read.xport("C:/Users/apple/Downloads/HOQ_I.XPT")

IMQ <- read.xport("C:/Users/apple/Downloads/IMQ_I.XPT")

INQ <- read.xport("C:/Users/apple/Downloads/INQ_I.XPT")

KIQ <- read.xport("C:/Users/apple/Downloads/KIQ_U_I.XPT")

MCQ <- read.xport("C:/Users/apple/Downloads/MCQ_I.XPT")

DPQ <- read.xport("C:/Users/apple/Downloads/DPQ_I.XPT")

OCQ <- read.xport("C:/Users/apple/Downloads/OCQ_I.XPT")

OHQ <- read.xport("C:/Users/apple/Downloads/OHQ_I.XPT")

PAQ <- read.xport("C:/Users/apple/Downloads/PAQ_I.XPT")

PFQ <- read.xport("C:/Users/apple/Downloads/PFQ_I.XPT")

RXQ <- read.xport("C:/Users/apple/Downloads/RXQ_RX_I.XPT")

RXQ_DRUG <- read.xport("C:/Users/apple/Downloads/RXQ_DRUG.XPT")

RXQASA <- read.xport("C:/Users/apple/Downloads/RXQASA_I.XPT")

RHQ <- read.xport("C:/Users/apple/Downloads/RHQ_I.XPT")

SLQ <- read.xport("C:/Users/apple/Downloads/SLQ_I.XPT")
SLQ <- SLQ[,-2:-3]

SXQ <- read.xport("C:/Users/apple/Downloads/SXQ_I.XPT")

SMQ <- read.xport("C:/Users/apple/Downloads/SMQ_I.XPT")
SMQ <- SMQ[,-13:-14]

SMQFAM <- read.xport("C:/Users/apple/Downloads/SMQFAM_I.XPT")

SMQRTU <- read.xport("C:/Users/apple/Downloads/SMQRTU_I.XPT")

SMQSHS <- read.xport("C:/Users/apple/Downloads/SMQSHS_I.XPT")

VTQ <- read.xport("C:/Users/apple/Downloads/VTQ_I.XPT")

WHQ <- read.xport("C:/Users/apple/Downloads/WHQ_I.XPT")

WHQMEC <- read.xport("C:/Users/apple/Downloads/WHQMEC_I.XPT")



# Put all data in a list
Questionnaire1 <- list(ACQ,ALQ,AUQ,BPQ,CBQ,CDQ,CBQ,
                       HSQ,DEQ,DIQ,DBQ,DLQ,DEQ,ECQ,
                       HIQ,HEQ,HUQ,HOQ,IMQ,INQ,KIQ,
                       MCQ,DPQ,OCQ,OHQ,PAQ,PFQ,
                       RXQASA,RHQ,SLQ,SXQ,
                       SMQ,SMQFAM,SMQRTU,SMQSHS,VTQ,
                       WHQ,WHQMEC)



# Merge all Questionnaire data
df_total <- Reduce(function(x,y) merge(x,y,all=T),Questionnaire1)

# Merge Demo and Questionnaire data into one matrix
DANDQ <- merge(DEMO,df_total,by="SEQN")

  
# Correlation and Covariance matrix of Demographic and Questionnaire data
cormatrix <- cor(DANDQ)
covmatrix <- cov(DANDQ)

# Merge DR1TOT and Questionarie
Driq1 <- merge(DR1TOT,df_total,by="SEQN")

Cordietary1 <- cor(Driq1)
Covdietary1 <- cov(Driq1)

# Merge DR2TOT and Questionarie
Driq2 <- merge(DR2TOT,df_total,by="SEQN")

Cordietary2 <- cor(Driq2)
Covdietary2 <- cov(Driq2)

# Merge DR1FF and Questionarie
Drfq1 <- merge(DR1FF,df_total,by="SEQN")

Cordietary3 <- cor(Drfq1)
Covdietary3 <- cov(Drfq1)

# Merge DR2FF and Questionarie
Drfq2 <- merge(DR2FF,df_total,by="SEQN")

Cordietary4 <- cor(Drfq2)
Covdietary4 <- cov(Drfq2)

