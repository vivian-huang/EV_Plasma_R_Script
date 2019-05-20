ev=read.csv("EV.csv")
par(mfrow=c(2,3))
boxplot(ev$IL17E~ev$SUBJECT,col=c("Blue","Red"),
        ylab="Concentration (pg/mL)",main="IL17E",boxwex=0.5)
boxplot(ev$IL12p70~ev$SUBJECT,col=c("Blue","Red"),
        ylab="Concentration (pg/mL)",main="IL12p70",boxwex=0.5)
boxplot(ev$IL2~ev$SUBJECT,col=c("Blue","Red"),
        ylab="Concentration (pg/mL)",main="IL2",boxwex=0.5)
boxplot(ev$GMCSF~ev$SUBJECT,col=c("Blue","Red"),
        ylab="Concentration (pg/mL)",main="GMCSF",boxwex=0.5)


par(mfrow=c(2,3))
plasma=read.csv("Plasma.csv")
boxplot(plasma$MCP1~plasma$SUBJECT,col=c("Blue","Red"),
        ylab="Concentration (pg/mL)",main="MCP1", boxwex=0.5)
boxplot(plasma$IP10~plasma$SUBJECT,col=c("Blue","Red"),
        ylab="Concentration (pg/mL)",main="IP10", boxwex=0.5)
boxplot(plasma$GRObeta~plasma$SUBJECT,col=c("Blue","Red"),
        ylab="Concentration (pg/mL)",main="GRObeta", boxwex=0.5)
boxplot(plasma$RANTES~plasma$SUBJECT,col=c("Blue","Red"),
        ylab="Concentration (pg/mL)",main="RANTES", boxwex=0.5)
#t.test(plasma$RANTES~plasma$SUBJECT)
boxplot(plasma$EGF~plasma$SUBJECT,col=c("Blue","Red"),
        ylab="Concentration (pg/mL)",main="EGF", boxwex=0.5)

#t.test(plasma$EGF~plasma$SUBJECT)

#library(ggpubr)
#narrower width, fill color, barplots w/ SD
