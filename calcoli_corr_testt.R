require(GGally)
require(skimr)
require("corpcor")
require(ggplot2)
require(ggcorrplot)
require(cowplot)
require(ppcor)

#GRUPPO 1
G1<-read.csv("G1_como.csv", sep=';', header=TRUE); head(G1)
#correlazioni tot tutte positive
cor_g1<-cor(G1)
g1tot<-ggcorrplot(corr = cor_g1, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                  colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                  legend.title = "intensità \u03C1", lab_size = 3.2)
#cor con p value
p_valueG1<-cor_pmat(G1)
g1tot_p<-ggcorrplot(corr = cor_g1, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                  colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                  legend.title = "intensità \u03C1", lab_size = 3, p.mat = p_valueG1)
plot_grid(g1tot, g1tot_p, labels=c("R", "R con p-value"), label_size = 20)

#velocità
v1_m<-(G1$M_giu-G1$M_dic)/2; mean(v1_m) #miglioramento medio motorio ogni 3 mesi: 2,39899
#miglioramento medio cm: 3.787879
v1_cm<-(G1$CM_giu-G1$CM_dic)/2; mean(v1_cm) 
#S: #9.848485
v1_s<-(G1$S_giu-G1$S_dic)/2; mean(v1_s) 
#CG #9.090909
v1_cg<-(G1$CG_giu-G1$CG_dic)/2;  mean(v1_cg)
#dataset velocità
v1<-data.frame(v1_m, v1_cm, v1_s, v1_cg); v1
#attenzione elemento 2 (ED ss), 3 (SS+2), 10 (niente)
#corr velocita'
cor_v1<-cor(v1)
v1tot<-ggcorrplot(corr = cor_v1, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                         colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                         legend.title = "intensità \u03C1", lab_size = 3.5)
#plot_grid(g1tot, v1tot, labels=c("g1tot", "v1tot"), label_size=12)
#cor con p value
p_valuev1<-cor_pmat(v1)
v1tot_p<-ggcorrplot(corr = cor_v1, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                    colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                    legend.title = "intensità \u03C1", lab_size = 3, p.mat = p_valuev1)
plot_grid(v1tot, v1tot_p, labels=c("R", "R con p-value"), label_size = 12)


#G2
G2<-read.csv("G2_como.csv", sep=';', header=TRUE); head(G2) #corr negative!!!
cor_g2<-cor(G2)
g2tot<-ggcorrplot(corr = cor_g2, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                  colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                  legend.title = "intensità \u03C1", lab_size = 3.2)
#cor con p value
p_valueG2<-cor_pmat(G2)
g2tot_p<-ggcorrplot(corr = cor_g2, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                    colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                    legend.title = "intensità \u03C1", lab_size = 3, p.mat = p_valueG2)
plot_grid(g2tot, g2tot_p, labels=c("R", "R con p-value"), label_size = 20)

#velocità
v2_m<-(G2$M_giu-G2$M_dic)/2; mean(v2_m)
v2_cm<-(G2$CM_giu-G2$CM_dic)/2; mean(v2_cm)
v2_s<-(G2$S_giu-G2$S_dic)/2; mean(v2_s)
v2_cg<-(G2$CG_giu-G2$CG_dic)/2; mean(v2_cg)
v2<-data.frame(v2_m, v2_cm, v2_s, v2_cg); v2 
#attenzione elemento 4 (HE ss+2), 6(CF ss+2), 7 (VO ss), 10 (YY nnt)
#cor velocita
cor_v2<-cor(v2)
v2tot<-ggcorrplot(corr = cor_v2, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                  colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                  legend.title = "intensità \u03C1", lab_size = 3.5)
#plot_grid(g2tot, v2tot, labels=c("g2tot", "v2tot"), label_size=12)
#cor con p value
p_valuev2<-cor_pmat(v2)
v2tot_p<-ggcorrplot(corr = cor_v2, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                    colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                    legend.title = "intensità \u03C1", lab_size = 3, p.mat = p_valuev2)
plot_grid(v2tot, v2tot_p, labels=c("R", "R con p-value"), label_size = 12)


#G3
G3<-read.csv("G3_como.csv", sep=';', header=TRUE); head(G3) #corr negative deboli
cor_g3<-cor(G3)
g3tot<-ggcorrplot(corr = cor_g3, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                  colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                  legend.title = "intensità \u03C1", lab_size = 3.2)
#cor con p value
p_valueG3<-cor_pmat(G3)
g3tot_p<-ggcorrplot(corr = cor_g3, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                    colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                    legend.title = "intensità \u03C1", lab_size = 3, p.mat = p_valueG3)
plot_grid(g3tot, g3tot_p, labels=c("R", "R con p-value"), label_size = 20)


#velocità
#M 5.815972
v3_m<-(G3$M_giu-G3$M_dic)/2; mean(v3_m)
v3_cm<-(G3$CM_giu-G3$CM_dic)/2; mean(v3_cm)
v3_s<-(G3$S_giu-G3$S_dic)/2; mean(v3_s)
v3_cg<-(G3$CG_giu-G3$CG_dic)/2; mean(v3_cg)
v3<-data.frame(v3_m, v3_cm, v3_s, v3_cg); v3
#attenzione elemento 2 (LA ss), 3 (BA ss+2), 4 (GT nnt), 7 (JI nnt), 8 (KS nnt), 14 (RB ss), 16 (EV nnt)
#boh strano....
cor_v3<-cor(v3)
v3tot<-ggcorrplot(corr = cor_v3, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                  colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                  legend.title = "intensità \u03C1", lab_size = 3.5)
#plot_grid(g3tot, v3tot, labels=c("g3tot", "v3tot"), label_size=12)
#cor con p value
p_valuev3<-cor_pmat(v3)
v3tot_p<-ggcorrplot(corr = cor_v3, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                    colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                    legend.title = "intensità \u03C1", lab_size = 3, p.mat = p_valuev3)
plot_grid(v3tot, v3tot_p, labels=c("R", "R con p-value"), label_size = 12)



#TEST T GENERICI
test12<-t.test(G1, G2); test12 #unico non significativo
test13<-t.test(G1, G3); test13 
test23<-t.test(G2, G3); test23
#test t velocità
test_v_12<-t.test(v1, v2); test_v_12 #p-value = 0.01025
test_v_13<-t.test(v1, v3); test_v_13 #p-value = 4.016e-05
test_v_23<-t.test(v2, v3); test_v_23 #p-value = 0.01708

###G1 E G2 SOLAMENTE
g1g2<-read.csv("como_g1g2.csv", sep=';', header=TRUE)
cor_g1g2<-cor(g1g2)
g1g2tot<-ggcorrplot(corr = cor_g1g2, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                  colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                  legend.title = "intensità \u03C1", lab_size = 3)
#cor con p value
p_valueg1g2<-cor_pmat(g1g2)
g1g2tot_p<-ggcorrplot(corr = cor_g1g2, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                    colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                    legend.title = "intensità \u03C1", lab_size = 3, p.mat = p_valueg1g2)
plot_grid(g1g2tot, g1g2tot_p, labels=c("R", "R con p-value"), label_size = 20)

#velocita
v12_m<-(g1g2$M_giu-g1g2$M_dic)/2; mean(v12_m)
v12_cm<-(g1g2$CM_giu-g1g2$CM_dic)/2; mean(v12_cm) 
v12_s<-(g1g2$S_giu-g1g2$S_dic)/2; mean(v12_s)
v12_cg<-(g1g2$CG_giu-g1g2$CG_dic)/2; mean(v12_cg)
v12<-data.frame(v12_m, v12_cm, v12_s, v12_cg); v12; max(v12); min(v12)
#corr velocita'
cor_v12<-cor(v12)
v12tot<-ggcorrplot(corr = cor_v12, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                  colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                  legend.title = "intensità \u03C1", lab_size = 3.5)
#plot_grid(g3tot, v3tot, labels=c("g3tot", "v3tot"), label_size=12)
#cor con p value
p_valuev12<-cor_pmat(v12)
v12tot_p<-ggcorrplot(corr = cor_v12, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                    colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                    legend.title = "intensità \u03C1", lab_size = 3, p.mat = p_valuev12)
plot_grid(v12tot, v12tot_p, labels=c("R", "R con p-value"), label_size = 18)

#correlazione tra i mesi
dicembre<-data.frame(g1g2$M_dic, g1g2$CM_dic, g1g2$S_dic, g1g2$CG_dic)
dicembre<-rowMeans(dicembre); dicembre
marzo<-data.frame(g1g2$M_mar, g1g2$CM_mar, g1g2$S_mar, g1g2$CG_mar)
marzo<-rowMeans(marzo)
giugno<-data.frame(g1g2$M_giu, g1g2$CM_giu, g1g2$S_giu, g1g2$CG_giu)
giugno<-rowMeans(giugno)
mesi<-data.frame(dicembre, marzo, giugno); mesi
cor_mesi<-cor(mesi)
require(ppcor)
pcor(mesi)$estimate

#TEST T
#servizi sociali vs no ss G1 e G2
ss_12<-read.csv("como_ss_g1g2.csv", sep =';', header=TRUE)
noss_12<-read.csv("como_noss_g1g2.csv", sep =';', header=TRUE)
test_ss_12<-t.test(ss_12, noss_12); test_ss_12 #p-value=0,4737 non significativo

#velocita
v12_m_ss<-(ss_12$M_giu-ss_12$M_dic)/2 
v12_cm_ss<-(ss_12$CM_giu-ss_12$CM_dic)/2 
v12_s_ss<-(ss_12$S_giu-ss_12$S_dic)/2 
v12_cg_ss<-(ss_12$CG_giu-ss_12$CG_dic)/2 
v12_ss<-data.frame(v12_m_ss, v12_cm_ss, v12_s_ss, v12_s_ss); v12_ss

v12_m_noss<-(noss_12$M_giu-noss_12$M_dic)/2 
v12_cm_noss<-(noss_12$CM_giu-noss_12$CM_dic)/2 
v12_s_noss<-(noss_12$S_giu-noss_12$S_dic)/2 
v12_cg_noss<-(noss_12$CG_giu-noss_12$CG_dic)/2 
v12_noss<-data.frame(v12_m_noss, v12_cm_noss, v12_s_noss, v12_s_noss); v12_noss

test_v_12_ss<-t.test(v12_noss, v12_ss); test_v_12_ss

# un anno vs 2 anni G1 e G2
o_12<-read.csv("como_1anno_g1g2.csv", sep =';', header=TRUE)
i_12<-read.csv("como_2anni_g1g2.csv", sep =';', header=TRUE)
test_io_12<-t.test(i_12, o_12); test_io_12
#significativo: p-value = 0,002535 con media 2anni + alta

v12_m_o<-(o_12$M_giu-o_12$M_dic)/2 
v12_cm_o<-(o_12$CM_giu-o_12$CM_dic)/2 
v12_s_o<-(o_12$S_giu-o_12$S_dic)/2 
v12_cg_o<-(o_12$CG_giu-o_12$CG_dic)/2 
v12_o<-data.frame(v12_m_o, v12_cm_o, v12_s_o, v12_s_o); v12_o

v12_m_i<-(i_12$M_giu-i_12$M_dic)/2 
v12_cm_i<-(i_12$CM_giu-i_12$CM_dic)/2 
v12_s_i<-(i_12$S_giu-i_12$S_dic)/2 
v12_cg_i<-(i_12$CG_giu-i_12$CG_dic)/2 
v12_i<-data.frame(v12_m_i, v12_cm_i, v12_s_i, v12_s_i); v12_i

test_v_12_io<-t.test(v12_o, v12_i); test_v_12_io


#normalita

