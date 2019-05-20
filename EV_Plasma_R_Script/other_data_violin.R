data=read.csv("other-data.csv")
str(data)
#30-130 nm conc.
#total conc.
library(ggplot2)
library(forcats)
library(ggpubr)
theme_set( theme_classic() + theme(legend.position = "top"))

#size
violin=ggplot(data,aes(x=fct_rev(Subject),y=Size))
size=violin + geom_violin(aes(fill = Subject), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("Size (nm)") + xlab("Subject")
size

#total concentration
violin=ggplot(data,aes(x=fct_rev(Subject),y=Total))
total=violin + geom_violin(aes(fill = Subject), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("Total Concentration") + xlab("Subject")
total

violin=ggplot(data,aes(x=fct_rev(Subject),y=Diluted))
diluted=violin + geom_violin(aes(fill = Subject), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("BLUE", "RED")) + 
  theme(legend.position = "none") + ylab("30-130 nm Concentration") + 
  xlab("Subject")
diluted

figure = ggarrange(total, diluted, size, ncol = 2, nrow = 2, 
                    labels = c("A", "B", "C"),
                    common.legend = TRUE, legend = "bottom")

annotate_figure(figure,top = text_grob("Extracellular Vesicles", color = "black",
                                        face = "bold", size = 14))
              