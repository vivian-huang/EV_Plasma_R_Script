library(ggpubr)
library(forcats)
#PLASMA
plasma=read.csv("Plasma.csv")

#MCP1
#compare_means(MCP1~SUBJECT, plasma,
            #  method = "kruskal.test")
#q3=quantile(plasma$MCP1,3/4)
#out=q3+1.5*IQR(plasma$MCP1)

mcp1=ggplot(plasma,aes(fct_rev(SUBJECT), MCP1)) + 
  geom_boxplot(aes(fill = SUBJECT),width=0.2) +
  scale_fill_manual(values = c("BLUE", "RED")) +
  annotate("text", x=1.5, y=1500, label= "p=0.013") + ylab("MCP1 (pg/mL)") + 
  xlab("SUBJECT")
 # stat_compare_means(method = "kruskal.test", label.x=1.3) + ylab("MCP1 (pg/mL)") #+
  #geom_text(aes(label=ifelse((MCP1>out),ID2,"")), hjust=2)
mcp1

#IP10
#compare_means(IP10~SUBJECT, plasma,
 #             method = "t.test")
#q3=quantile(plasma$IP10,3/4)
#out=q3+3*IQR(plasma$IP10) #should only register for 1.5 but 3 gets 8

ip10=ggplot(plasma,aes(fct_rev(SUBJECT), IP10)) + 
  geom_boxplot(aes(fill = SUBJECT), width=0.2) +
  scale_fill_manual(values = c("BLUE", "RED")) +
  annotate("text", x=1.5, y=200, label= "p=0.014") + ylab("IP10 (pg/mL)") + 
  xlab("SUBJECT")
  #stat_compare_means(method = "t.test", label.x=1.3)  #+ 
  #geom_text(aes(label=ifelse((IP10>out),ID2,"")), hjust=2)


#GRObeta
#compare_means(GRObeta~SUBJECT, plasma,
#              method = "t.test")
#q3=quantile(plasma$GRObeta,3/4)
#out=q3+1*IQR(plasma$GRObeta) #should be 1.5 but adjusting

grobeta=ggplot(plasma,aes(fct_rev(SUBJECT), GRObeta)) + 
  geom_boxplot(aes(fill = SUBJECT), width=0.2) +
  scale_fill_manual(values = c("BLUE", "RED")) + 
  annotate("text", x=1.5, y=2300, label= "p=0.032") + ylab("GRObeta (pg/mL)") +
  xlab("SUBJECT")
  #stat_compare_means(method = "t.test", label.x=1.3) + ylab("GRObeta (pg/mL)") #+ 
  #geom_text(aes(label=ifelse((GRObeta>out),ID2,"")), hjust=2)

#RANTES
#compare_means(RANTES~SUBJECT, plasma,
#              method = "t.test")

rantes=ggplot(plasma,aes(fct_rev(SUBJECT), RANTES)) + 
  geom_boxplot(aes(fill = SUBJECT), width=0.2) +
  scale_fill_manual(values = c("BLUE", "RED")) + 
  annotate("text", x=1.5, y=150000, label= "p=0.012") + ylab("RANTES (pg/mL)") + 
  xlab("SUBJECT")
#  stat_compare_means(method = "t.test", label.x=1.3) + ylab("RANTES (pg/mL)")

#EGF
#compare_means(EGF~SUBJECT, plasma,
#              method = "t.test")
#q3=quantile(plasma$EGF,3/4)
#out=q3+1.5*IQR(plasma$EGF)

egf=ggplot(plasma,aes(fct_rev(SUBJECT), EGF)) + 
  geom_boxplot(aes(fill = SUBJECT), width=0.2) +
  scale_fill_manual(values = c("BLUE", "RED"))+ 
  annotate("text", x=1.5, y=350, label= "p=0.03") + ylab("EGF (pg/mL)") + 
  xlab("SUBJECT")
  #stat_compare_means(method = "t.test", label.x=1.3) + ylab("EGF (pg/mL)") #+ 
  #geom_text(aes(label=ifelse((EGF>out),ID2,"")), hjust=2)

pfigure = ggarrange(mcp1, ip10, grobeta, rantes, egf, 
                   labels = c("A", "B", "C", "D","E"),
                    ncol = 2, nrow = 3, 
                   common.legend = TRUE, legend = "bottom")
annotate_figure(pfigure,top = text_grob("Plasma Cytokines", color = "black",
                                       face = "bold", size = 14))

#--------------------------------------------------------------------------
#EV
ev=read.csv("EV.csv")

#IL17E small p value
#compare_means(IL17E~SUBJECT, ev,
#              method = "t.test")

il17e=ggplot(ev,aes(fct_rev(SUBJECT), IL17E)) + 
  geom_boxplot(aes(fill = SUBJECT), width=0.2) +
  scale_fill_manual(values = c("BLUE", "RED")) + 
  annotate("text", x=1.5, y=450, label= "p=0.002") + ylab("IL17E (pg/mL)") + 
  xlab("SUBJECT")
  #stat_compare_means(method = "t.test", label.x=1.3) + ylab("IL17E (pg/mL)") + 
  #geom_text(aes(label=ifelse((IL17E>4*IQR(IL17E)),ID2,"")), hjust=2)

#IL12p70
#compare_means(IL12p70~SUBJECT, ev,
#              method = "t.test")

il12p70=ggplot(ev,aes(fct_rev(SUBJECT), IL12p70)) + 
  geom_boxplot(aes(fill = SUBJECT), width=0.2) +
  scale_fill_manual(values = c("BLUE", "RED")) +
  annotate("text", x=1.5, y=40, label= "p=0.023") + ylab("IL12p70 (pg/mL)") + 
  xlab("SUBJECT")
  #stat_compare_means(method = "t.test", label.x=1.3) + ylab("IL12p70 (pg/mL)") +
  #geom_text(aes(label=ifelse((IL12p70>4*IQR(IL12p70)),ID2,"")), hjust=2)

#IL2
#compare_means(IL2~SUBJECT, ev,
#              method = "t.test")

il2=ggplot(ev,aes(fct_rev(SUBJECT), IL2)) + 
  geom_boxplot(aes(fill = SUBJECT), width=0.2) +
  scale_fill_manual(values = c("BLUE", "RED"))+ 
  annotate("text", x=1.5, y=25, label= "p=0.016") + ylab("IL2 (pg/mL)") + 
  xlab("SUBJECT")
  #stat_compare_means(method = "t.test", label.x=1.3) + ylab("IL2 (pg/mL)") +
  #geom_text(aes(label=ifelse((IL2>4*IQR(IL2)),ID2,"")), hjust=2)

#GMCSF

#compare_means(GMCSF~SUBJECT, ev,
#              method = "t.test")
#q3=quantile(ev$GMCSF,3/4)
#out=q31+2*IQR(ev$GMCSF)

gmcsf=ggplot(ev,aes(fct_rev(SUBJECT), GMCSF)) + 
  geom_boxplot(aes(fill = SUBJECT), width=0.2) +
  scale_fill_manual(values = c("BLUE", "RED"))+
  annotate("text", x=1.5, y=75, label= "p=0.031") + ylab("EGF (pg/mL)") + 
  xlab("SUBJECT")
  #stat_compare_means(method = "t.test", label.x=1.3) + ylab("GMCSF (pg/mL)") +
  #geom_text(aes(label=ifelse((GMCSF>out),ID2,"")), hjust=2)

efigure = ggarrange(il17e, il12p70, il2, gmcsf, 
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
annotate_figure(efigure,top = text_grob("Extracellular Vesicle Cytokines", 
                                        color = "black",
                                        face = "bold", size = 14))

