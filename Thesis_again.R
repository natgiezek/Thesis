rm(list=ls())
getwd()

#Verify if all libraries needed
library(tidyverse)
library(estimatr)
library(plm)
library(modelsummary)
library(AER)
library(stargazer)
library(haven)
library(magrittr)
library(foreign)
library(data.table)

library(readxl)

### LOAD THE DATA ###
AV_Data = read_excel("Desktop/MSc Thesis/AV_Data.xlsx", sheet = "Sheet4")
ORES    = read_excel("Downloads/ORES.xlsx", sheet = 2)

data = left_join(AV_Data,ORES, by = c("Country", "Code"))

Oil <- read_excel("Downloads/Oil_fuel.xlsx", sheet = 2)
Fuel <- read_excel("Downloads/Oil_fuel.xlsx", sheet = 3)
GDP <- read_excel("Downloads/GDP_USD.xlsx", sheet = "GDP")
GDP_growth <- read_excel("Downloads/GDPandInflation.xlsx", sheet = "Growth")
INR <- read_excel("Downloads/Internet.xlsx", sheet = "Net")
INF <- read_excel("Downloads/GDPandInflation.xlsx", sheet = "Sheet2")
### in OFDI, Frm. Sudan has been added to Sudan
OFDI <- read_excel("Downloads/FDIData_10Jan2022.xlsx", sheet = "OFDI")
data = left_join(data,Oil, by = c("Country", "Code"))
data = left_join(data, Fuel, by = c("Country", "Code"))
data = left_join(data, INR, by = c("Country", "Code"))
data = left_join(data, GDP_growth, by = c("Country", "Code"))
data = left_join(data, GDP, by = c("Country", "Code"))
data = left_join(data, INF, by = c("Country", "Code"))
data = left_join(data, OFDI, by = c("Country", "Code"))

# CREATE a dummy variable for the rare earth occurences
mydata$REE_dummy <- ifelse(mydata$REE > 0, 1, 0)
# Asiedu (2013)
#FDI = b0 + b1*lag(FDI) + b2*X + b3*Inflation + bi + bt + epsilon
OLS <- lm(OFDI ~ GDP_mn + Infrastructure + Inflation + WGI + ores_metals + REE, data = mydata)
summary(OLS) #not bad
#lagged OFDI does not work well
ols_fe <- lm(OFDI ~ GDP_mn + Infrastructure + Inflation + WGI + ores_metals + REE + factor(Country) - 1, data = mydata)
summary(ols_fe) # not good
panel_fe<- plm(OFDI ~ GDP_mn + Infrastructure + Inflation + WGI + ores_metals + REE, data= mydata,index=c("Country", "Year"), model="within")
summary(panel_fe) # no REE
fixed.time <- plm(OFDI ~ GDP_mn + Infrastructure + Inflation + WGI + ores_metals + REE + factor(Year), data=mydata, index=c("Country","Year"), model="within")
summary(fixed.time) #strange
twoways <- plm(OFDI ~ GDP_mn + Infrastructure + Inflation + WGI + ores_metals + REE, data = mydata,index=c("Country", "Year"), model = "within", effect = "twoways")
summary(twoways) # no REE
# Testing time-fixed effects. The null is that no time-fixed effects needed
pFtest(fixed.time, panel_fe)
plmtest(fixed, c("time"), type=("bp"))
# one significant, one non-significant
#Log transformations: How to handle negative data values?
fixef(panel_fe) 
pFtest(panel_fe, OLS) # Testing for fixed effects, null: OLS better than fixed;If the p-value is < 0.05 then the fixed effects model is better, fixed effects not better?
panel_random <- plm(OFDI ~ GDP_mn + Infrastructure + Inflation + WGI + ores_metals + REE, data=mydata, index=c("Country", "Year"), model="random")
summary(panel_random) #not bad
phtest(panel_fe,panel_random) # fe not better?
# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
panel.pool <- plm(OFDI ~ GDP_mn + Infrastructure + Inflation + WGI + ores_metals + REE, data = mydata, model = "pooling", index = c("Country","Year"))
plmtest(panel.pool, type=c("bp")) # simply OLS is fine?
my_data2020 <- filter(mydata, Year == "2020")
#OLS_2020 <- lm(OFDI ~ GDP_mn + Infrastructure + Inflation + WGI + ores_metals + REE, data = my_data2020)
#summary(OLS_2020) not useful
#Log transformation of variables for 2020; Buckley et al. (2007) use log of GDP, GDP per capita, GDP increase, ore and metal exports, annual patent registration, political risk rating, exchange rate, inflation rate, export, import, distance, openness
my_data2020 <- my_data2020 %>% rowwise() %>% mutate(LY = log(OFDI + 296))
my_data2020 <- my_data2020 %>% rowwise() %>% mutate(Linfr = log(Infrastructure + 1))
my_data2020 <- my_data2020 %>% rowwise() %>% mutate(Linf = log(Inflation + 10))
my_data2020 <- my_data2020 %>% rowwise() %>% mutate(Linfr = log(Infrastructure + 1))
my_data2020 <- my_data2020 %>% rowwise() %>% mutate(Lwgi = log(WGI + 3))
my_data2020 <- my_data2020 %>% rowwise() %>% mutate(Loilrent = log(oilrent + 1))
my_data2020 <- my_data2020 %>% rowwise() %>% mutate(Loilex = log(oilex + 1))
my_data2020 <- my_data2020 %>% rowwise() %>% mutate(Lores = log(ores_metals + 1))
OLS_2020 <- lm(LY ~ log(GDP_mn) + Infrastructure + Inflation + WGI + ores_metals + REE, data = my_data2020)
summary(OLS_2020)
OLS_naive <- lm(LY ~ log(GDP_mn) + REE, data = my_data2020) # not significant, p-value > 0.5
summary(OLS_naive)
another_OLS_naive <-lm(LY ~ REE, data = my_data2020) # not significant, p-value > 0.37
summary(another_OLS_naive)
resource_OLS <- lm(LY ~ log(GDP_mn) + ores_metals, data = my_data2020)
summary(resource_OLS) # also not significant, also does not work with "Resources"
my_data2015 <- filter(mydata, Year == "2015")
OLS_2020_logs <- lm(LY ~ log(GDP_mn) + Linfr + Linf + Lwgi + log(Resources) + REE_dummy, data = my_data2020)
summary(OLS_2020_logs)
#log transformation of OFDI variable for 2015
my_data2015 <- my_data2015 %>% rowwise() %>% mutate(LY = log(OFDI + 42))
OLS_2015 <- lm(LY ~ log(GDP_mn) + Infrastructure + Inflation + WGI + ores_metals + REE, data = my_data2015)
summary(OLS_2015)
OLS_2015_1 <- lm(LY ~ log(GDP_mn) + Infrastructure + Inflation + WGI + Resources + REE, data = my_data2015)
summary(OLS_2015_1)
OLS_2015_2 <- lm(LY ~ log(GDP_mn) + Infrastructure + Inflation + WGI + oilrent + oilex + REE, data = my_data2015)
summary(OLS_2015_2)
mydata <- mydata %>% rowwise() %>% mutate(LY = log(OFDI + 815))
OFDI_diff <- diff(mydata$OFDI)
OFDI_diff <- c(NA, OFDI_diff)
mydata$OFDI_diff <- OFDI_diff
GDP_mn_diff <- diff(mydata$GDP_mn)
GDP_mn_diff <- c(NA, GDP_mn_diff)
mydata$GDP_mn_diff <- GDP_mn_diff
Infr_diff <- diff(mydata$Infrastructure)
Infr_diff <- c(NA, Infr_diff)
mydata$Infr_diff <- Infr_diff
Inf_diff <- diff(mydata$Inflation)
Inf_diff <- c(NA, Inf_diff)
mydata$Infr_diff <- Inf_diff
ores_diff <- diff(mydata$ores_metals)
ores_diff <- c(NA, ores_diff)
mydata$ores_diff <- ores_diff
OLS_diff <- lm(OFDI_diff ~ GDP_mn_diff + Infrastructure + Inflation + WGI + ores_metals + REE_dummy, data = mydata)
summary(OLS_diff)
summary(mydata)
mydata <- mydata %>% rowwise() %>% mutate(Linfr = log(Infrastructure + 1))
mydata <- mydata %>% rowwise() %>% mutate(Linf = log(Inflation + 10))
mydata <- mydata %>% rowwise() %>% mutate(Lwgi = log(WGI + 3))
mydata <- mydata %>% rowwise() %>% mutate(Loilrent = log(oilrent + 1))
mydata <- mydata %>% rowwise() %>% mutate(Loilex = log(oilex + 1))
mydata <- mydata %>% rowwise() %>% mutate(Lores = log(ores_metals + 1))
OLS_log <- lm(LY ~ log(GDP_mn) + Linfr + Linf + Lwgi + log(Resources) + REE_dummy, data = mydata)
summary(OLS_log) # also insignificant
fwrite(mydata,'mydata.csv')

AV1 <- select(AV_Data, c(OFDI_flow,GDP,GDP_growth, WGI,Infrastructure, Inflation, ORES,REE))
AV1.y <- AV1$OFDI_flow
AV1$OFDI_flow <- NULL
AV1_standardize <- as.data.frame(scale(AV1))
AV1.y_standardize <- as.data.frame(scale(AV1.y))
AV1.pca1 <- prcomp(AV1_standardize, center=TRUE, scale.=TRUE)
summary(AV1.pca1)
pcs1 <- as.data.frame(AV1.pca1$x)
ols.data1 <- cbind(AV1.y_standardize, pcs1)
ols.dat1 <- select(ols.data1, -c(V1)) 
lmodel1 <- lm(formula = unlist(AV1.y_standardize) ~ ., data = ols.dat1) 
summary(lmodel1)
cor(ols.dat1$PC1, AV1)
cor(ols.dat1$PC2, AV1)
cor(ols.dat1$PC3, AV1)
cor(ols.dat1$PC4, AV1)
cor(ols.dat1$PC5, AV1)
cor(ols.dat1$PC6, AV1)
cor(ols.dat1$PC7, AV1)

AV2 <- select(AV_Data, c(OFDI_flow,GDP,GDP_growth, WGI,Infrastructure, Inflation, Resources,REE))
AV2.y <- AV2$OFDI_flow
AV2$OFDI_flow <- NULL
AV2_standardize <- as.data.frame(scale(AV2))
AV2.y_standardize <- as.data.frame(scale(AV2.y))
AV2.pca2 <- prcomp(AV2_standardize, center=TRUE, scale.=TRUE)
summary(AV2.pca2)
pcs2 <- as.data.frame(AV2.pca2$x)
ols.data2 <- cbind(AV2.y_standardize, pcs2)
ols.dat2 <- select(ols.data2, -c(V1)) 
lmodel2 <- lm(formula = unlist(AV2.y_standardize) ~ ., data = ols.dat2) 
summary(lmodel2)
cor(ols.dat2$PC1, AV2)
cor(ols.dat2$PC2, AV2)
cor(ols.dat2$PC3, AV2)
cor(ols.dat2$PC4, AV2)
cor(ols.dat2$PC5, AV2)
cor(ols.dat2$PC6, AV2)
cor(ols.dat2$PC7, AV2)

AV3 <- select(AV_Data, c(OFDI_flow,GDP,GDP_growth, WGI,Infrastructure, Inflation, oilrent,REE))
AV3.y <- AV3$OFDI_flow
AV3$OFDI_flow <- NULL
AV3_standardize <- as.data.frame(scale(AV3))
AV3.y_standardize <- as.data.frame(scale(AV3.y))
AV3.pca3 <- prcomp(AV3_standardize, center=TRUE, scale.=TRUE)
summary(AV3.pca3)
pcs3 <- as.data.frame(AV3.pca3$x)
ols.data3 <- cbind(AV3.y_standardize, pcs3)
ols.dat3 <- select(ols.data3, -c(V1)) 
lmodel3 <- lm(formula = unlist(AV3.y_standardize) ~ ., data = ols.dat3) 
summary(lmodel3)
cor(ols.dat3$PC1, AV3)
cor(ols.dat3$PC2, AV3)
cor(ols.dat3$PC3, AV3)
cor(ols.dat3$PC4, AV3)
cor(ols.dat3$PC5, AV3)
cor(ols.dat3$PC6, AV3)
cor(ols.dat3$PC7, AV3)
# Principal Component Analysis
AV4 <- select(AV_Data, c(OFDI_flow,GDP,GDP_growth, WGI,Infrastructure, Inflation, fuels,REE))
AV4.y <- AV4$OFDI_flow
AV4$OFDI_flow <- NULL
AV4_standardize <- as.data.frame(scale(AV4))
AV4.y_standardize <- as.data.frame(scale(AV4.y))
AV4.pca4 <- prcomp(AV4_standardize, center=TRUE, scale.=TRUE)
summary(AV4.pca4)
pcs4 <- as.data.frame(AV4.pca4$x)
ols.data4 <- cbind(AV4.y_standardize, pcs4)
ols.dat4 <- select(ols.data4, -c(V1)) 
lmodel4 <- lm(formula = unlist(AV4.y_standardize) ~ ., data = ols.dat4) 
summary(lmodel4)
cor(ols.dat4$PC1, AV4)
cor(ols.dat4$PC2, AV4)
cor(ols.dat4$PC3, AV4)
cor(ols.dat4$PC4, AV4)
cor(ols.dat4$PC5, AV4)
cor(ols.dat4$PC6, AV4)
cor(ols.dat4$PC7, AV4)

AV2 <- select(AV_Data, c(OFDI_flow,GDP,GDP_growth, WGI,Infrastructure, Inflation, Resources,REE))
AV2.y <- AV2$OFDI_flow
AV2$OFDI_flow <- NULL
AV2_standardize <- as.data.frame(scale(AV2))
AV2.y_standardize <- as.data.frame(scale(AV2.y))
AV2.pca2 <- prcomp(AV2_standardize, center=TRUE, scale.=TRUE)
summary(AV2.pca2)
pcs2 <- as.data.frame(AV2.pca2$x)
ols.data2 <- cbind(AV2.y_standardize, pcs2)
ols.dat2 <- select(ols.data2, -c(V1)) 
lmodel2 <- lm(formula = unlist(AV2.y_standardize) ~ ., data = ols.dat2) 
summary(lmodel2)
cor(ols.dat2$PC1, AV2)
cor(ols.dat2$PC2, AV2)
cor(ols.dat2$PC3, AV2)
cor(ols.dat2$PC4, AV2)
cor(ols.dat2$PC5, AV2)
cor(ols.dat2$PC6, AV2)
cor(ols.dat2$PC7, AV2)

AV3 <- select(AV_Data, c(OFDI_flow,GDP,GDP_growth, WGI,Infrastructure, Inflation, oilrent,REE))
AV3.y <- AV3$OFDI_flow
AV3$OFDI_flow <- NULL
AV3_standardize <- as.data.frame(scale(AV3))
AV3.y_standardize <- as.data.frame(scale(AV3.y))
AV3.pca3 <- prcomp(AV3_standardize, center=TRUE, scale.=TRUE)
summary(AV3.pca3)
pcs3 <- as.data.frame(AV3.pca3$x)
ols.data3 <- cbind(AV3.y_standardize, pcs3)
ols.dat3 <- select(ols.data3, -c(V1)) 
lmodel3 <- lm(formula = unlist(AV3.y_standardize) ~ ., data = ols.dat3) 
summary(lmodel3)
cor(ols.dat3$PC1, AV3)
cor(ols.dat3$PC2, AV3)
cor(ols.dat3$PC3, AV3)
cor(ols.dat3$PC4, AV3)
cor(ols.dat3$PC5, AV3)
cor(ols.dat3$PC6, AV3)
cor(ols.dat3$PC7, AV3)

### some graphs
library(cowplot)
a <-ggplot(AV_Data, aes(x=GDP, y=OFDI_flow)) + geom_point() + geom_smooth(method=lm, se=FALSE)
b <-ggplot(AV_Data, aes(x=GDP_growth, y=OFDI_flow)) + geom_point() + geom_smooth(method=lm, se=FALSE)
c <-ggplot(AV_Data, aes(x= WGI, y=OFDI_flow)) + geom_point() + geom_smooth(method=lm, se=FALSE)
d <- ggplot(AV_Data, aes(x=Infrastructure, y=OFDI_flow)) + geom_point() + geom_smooth(method=lm, se=FALSE)
e <- ggplot(AV_Data, aes(x=Inflation, y=OFDI_flow)) + geom_point() + geom_smooth(method=lm, se=FALSE)
f <-ggplot(AV_Data, aes(x=ORES, y=OFDI_flow)) + geom_point() + geom_smooth(method=lm, se=FALSE)
g <-ggplot(AV_Data, aes(x= fuels, y=OFDI_flow)) + geom_point() + geom_smooth(method=lm, se=FALSE)
h <-ggplot(AV_Data, aes(x= oilrent, y=OFDI_flow)) + geom_point() + geom_smooth(method=lm, se=FALSE)
i <-ggplot(AV_Data, aes(x= REE, y=OFDI_flow)) + geom_point() + geom_smooth(method=lm, se=FALSE)
plot_grid(a,b,c,d,e,f,g,h,i, labels = "AUTO")

aa <-ggplot(AV_Data, aes(x=log(GDP), y=log(OFDI_flow+13))) + geom_point() + geom_smooth(method=lm, se=FALSE)
bb <-ggplot(AV_Data, aes(x=log(GDP_growth+5), y=log(OFDI_flow+13))) + geom_point() + geom_smooth(method=lm, se=FALSE)
cc <-ggplot(AV_Data, aes(x= log(WGI+2), y=log(OFDI_flow+13))) + geom_point() + geom_smooth(method=lm, se=FALSE)
dd <- ggplot(AV_Data, aes(x=log(Infrastructure), y=log(OFDI_flow+13))) + geom_point() + geom_smooth(method=lm, se=FALSE)
ee <- ggplot(AV_Data, aes(x=log(Inflation+1), y=log(OFDI_flow+13))) + geom_point() + geom_smooth(method=lm, se=FALSE)
ff <-ggplot(AV_Data, aes(x=log(ORES+1), y=log(OFDI_flow+13))) + geom_point() + geom_smooth(method=lm, se=FALSE)
gg <-ggplot(AV_Data, aes(x= log(fuels+1), y=log(OFDI_flow+13))) + geom_point() + geom_smooth(method=lm, se=FALSE)
hh <-ggplot(AV_Data, aes(x= log(oilrent+1), y=log(OFDI_flow+13))) + geom_point() + geom_smooth(method=lm, se=FALSE)
ii <-ggplot(AV_Data, aes(x= log(REE+1), y=log(OFDI_flow+13))) + geom_point() + geom_smooth(method=lm, se=FALSE)
plot_grid(aa,bb,cc,dd,ee,ff,gg,hh,ii)

stargazer(OLS_log,OLS_log1,OLS_log3, OLS_log4, OLS_log5, OLS_log6, type='text', digits = 2, title = 'The log-log models',out = 'Lm_2.doc')

library(knitr)
library(kableExtra)

# Assuming df is your dataset
cor_matrix <- cor(AV_Data[3:14], use = "complete.obs")  # use "complete.obs" to handle missing values

# Create a basic table
cor_table <- kable(cor_matrix, caption = "Table 5: Correlation Matrix", format = "html", booktabs = TRUE)

# Add professional styling with kableExtra
cor_table <- cor_table %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE)  # Make the first column bold

summary_stats <- summary(AV_Data[3:14])

# Create a basic table
summary_table <- kable(summary_stats, caption = "Table 4: Summary Statistics", format = "html", booktabs = TRUE)

# Add professional styling with kableExtra
summary_table <- summary_table %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE)  # Make the first column bold







       