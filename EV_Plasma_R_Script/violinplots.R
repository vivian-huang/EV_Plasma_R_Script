plasma=read.csv("Plasma.csv")

str(plasma)
head(plasma)

library(ggplot2)
theme_set( theme_classic() + theme(legend.position = "top"))

#MCP1
violin=ggplot(plasma,aes(x=fct_rev(SUBJECT),y=MCP1))
#add mean points +/- SD
violin + geom_violin(trim = FALSE) +
  stat_summary( fun.data = "mean_sdl", fun.args = list(mult = 1), 
                geom = "pointrange", color = "black")

# Combine with box plot to add median and quartiles 
# Change fill color by groups, remove legend
mcp1=violin + geom_violin(aes(fill = SUBJECT), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("MCP1 (pg/mL)") +
  annotate("text", x=1.5, y=1500, label= "p=0.013") + xlab("SUBJECT")

#IP10
violin=ggplot(plasma,aes(x=fct_rev(SUBJECT),y=IP10))
violin + geom_violin(trim = FALSE) +
  stat_summary( fun.data = "mean_sdl", fun.args = list(mult = 1), 
                geom = "pointrange", color = "black")

ip10=violin + geom_violin(aes(fill = SUBJECT), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("IP10 (pg/mL)") + 
  annotate("text", x=1.5, y=200, label= "p=0.014") + xlab("SUBJECT")

#GRObeta
violin=ggplot(plasma,aes(x=fct_rev(SUBJECT),y=GRObeta))
violin + geom_violin(trim = FALSE) +
  stat_summary( fun.data = "mean_sdl", fun.args = list(mult = 1), 
                geom = "pointrange", color = "black")

grobeta=violin + geom_violin(aes(fill = SUBJECT), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("GRObeta (pg/mL)") + 
  annotate("text", x=1.5, y=2300, label= "p=0.032") + xlab("SUBJECT")

#RANTES
violin=ggplot(plasma,aes(x=fct_rev(SUBJECT),y=RANTES))
violin + geom_violin(trim = FALSE) +
  stat_summary( fun.data = "mean_sdl", fun.args = list(mult = 1), 
                geom = "pointrange", color = "black")

rantes=violin + geom_violin(aes(fill = SUBJECT), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("RANTES (pg/mL)") + 
  annotate("text", x=1.5, y=150000, label= "p=0.012") + xlab("SUBJECT") + 
  coord_cartesian(ylim=c(0,160000))

#EGF
violin=ggplot(plasma,aes(x=fct_rev(SUBJECT),y=EGF))
violin + geom_violin(trim = FALSE) +
  stat_summary( fun.data = "mean_sdl", fun.args = list(mult = 1), 
                geom = "pointrange", color = "black")

egf=violin + geom_violin(aes(fill = SUBJECT), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("EGF (pg/mL)") + 
  annotate("text", x=1.5, y=350, label= "p=0.03") + xlab("SUBJECT")

pfigure = ggarrange(mcp1, ip10, grobeta, rantes, egf, 
                    labels = c("A", "B", "C", "D","E"),
                    ncol = 2, nrow = 3, 
                    common.legend = TRUE, legend = "bottom")
annotate_figure(pfigure,top = text_grob("Plasma Cytokines", color = "black",
                                        face = "bold", size = 14))
#------------------------------------------------------------------------
#Extracellular Vesicles
ev=read.csv("EV.csv")

str(ev)
head(ev)

#IL17E
violin=ggplot(ev,aes(x=fct_rev(SUBJECT),y=IL17E))
violin + geom_violin(trim = TRUE) +
  stat_summary( fun.data = "mean_sdl", fun.args = list(mult = 1), 
                geom = "pointrange", color = "black")

il17e=violin + geom_violin(aes(fill = SUBJECT), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("IL17E (pg/mL)") + 
  annotate("text", x=1.5, y=600, label= "p=0.002") + xlab("SUBJECT") + 
  coord_cartesian(ylim=c(0,650))

#IL12p70
violin=ggplot(ev,aes(x=fct_rev(SUBJECT),y=IL12p70))
violin + geom_violin(trim = TRUE) +
  stat_summary( fun.data = "mean_sdl", fun.args = list(mult = 1), 
                geom = "pointrange", color = "black")

il12p70=violin + geom_violin(aes(fill = SUBJECT), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("IL12p70 (pg/mL)") + 
  annotate("text", x=1.5, y=40, label= "p=0.023") + xlab("SUBJECT")

#IL2
violin=ggplot(ev,aes(x=fct_rev(SUBJECT),y=IL2))
violin + geom_violin(trim = TRUE) +
  stat_summary( fun.data = "mean_sdl", fun.args = list(mult = 1), 
                geom = "pointrange", color = "black")

il2=violin + geom_violin(aes(fill = SUBJECT), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("IL2 (pg/mL)") + 
  annotate("text", x=1.5, y=25, label= "p=0.016") + xlab("SUBJECT")

#GMCSF
violin=ggplot(ev,aes(x=fct_rev(SUBJECT),y=GMCSF))
violin + geom_violin(trim = TRUE) +
  stat_summary( fun.data = "mean_sdl", fun.args = list(mult = 1), 
                geom = "pointrange", color = "black")

gmcsf=violin + geom_violin(aes(fill = SUBJECT), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("GMCSF (pg/mL)") + 
  annotate("text", x=1.5, y=75, label= "p=0.031") + xlab("SUBJECT")

efigure = ggarrange(il17e, il12p70, il2, gmcsf, 
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
annotate_figure(efigure,top = text_grob("Extracellular Vesicle Cytokines", 
                                        color = "black",
                                        face = "bold", size = 14))
