require(skimr)
require(GGally)
require(ggplot2)
require(ggcorrplot)
require(Hmisc)
require(psych)
require(cowplot)
#BAMBINI
#BAMB22
bamb22<-read.csv("bamb22_bucc.csv", sep=';', header=TRUE)
colnames(bamb22)<- c("D1", "D2", "D3", "D4"); head(bamb22)
bamb22$"D1"<-NULL
S<- cor(bamb22); S<-round(S, 4); S; max(S[S!=1])
mean(S[S!=1])
pvalue_b22<-cor_pmat(bamb22); pvalue_b22
corbamb22<-ggcorrplot(corr = S, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
           colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, legend.title = "intensità \u03C1", lab_size = 3.5)
#grafico con p value
corbamb22_p<-ggcorrplot(corr = S, hc.order = FALSE, type = "lower", ggtheme = ggplot2::theme_dark(),
           colors=c('steelblue','white','red3'), p.mat=pvalue_b22,
           legend.title = "intensità \u03C1", digits=4,show.diag = TRUE)

plot_grid(corbamb22, corbamb22_p, labels=c("R", "R con p-value"), label_size=12)
#alpha
alpha(bamb22, check.keys = TRUE)

#SD VARIABILI
my_skim<- skim_with(base=sfl(),
                    numeric=sfl(hist=NULL))
my_skim(bamb22)
#RMSSTD
n_b22<-nrow(bamb22); n_b22
cov.bamb22<-(n_b22-1)/n_b22*var(bamb22); cov.bamb22
Vt_bamb22<-sum(diag(cov.bamb22)); Vt_bamb22
rms_bamb22<-sqrt(Vt_bamb22/ncol(bamb22)); rms_bamb22

#BAMB23
bamb23<-read.csv("bamb23_bucc.csv", sep=';', header=FALSE)
colnames(bamb23)<- c("D1", "D2", "D3", "D4"); head(bamb23)
bamb23$D1<-NULL
pvalue_b23<-cor_pmat(bamb23); pvalue_b23

S2<- cor(bamb23); S2<-round(S2, 4); S2; max(S2[S2!=1])
corbamb23<-ggcorrplot(corr = S2,type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
           colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
           legend.title = "intensità \u03C1", lab_size = 3.5)
#grafico con p-value (x sono non significativi)
corbamb23_p<-ggcorrplot(corr = S2, type = "lower",
           lab = FALSE, show.diag=TRUE, legend.title = "intensità \u03C1", 
           ggtheme = ggplot2::theme_dark(),
           colors=c('steelblue','white','red3'), p.mat=pvalue_b23, digits = 4)
plot_grid(corbamb23, corbamb23_p, labels=c("R", "R con p-value"), label_size=12)
#alpha
alpha(bamb23, check.keys = TRUE)
my_skim<- skim_with(base=sfl(),
                    numeric=sfl(hist=NULL))
my_skim(bamb23)
#rmsstd
n_b23<-nrow(bamb23); n_b23
cov.bamb23<-(n_b23-1)/n_b23*var(bamb23); cov.bamb23
Vt_bamb23<-sum(diag(cov.bamb23)); Vt_bamb23
rms_bamb23<-sqrt(Vt_bamb23/ncol(bamb23)); rms_bamb23

#INTERMEDI
#INTER22
inter22<-read.csv("inter22_bucc.csv", sep=';', header=FALSE)
colnames(inter22)<- c("D1", "D2", "D3", "D4","D5","D6"); head(inter22)
inter22$"D1"<-NULL
S3<-cor(inter22) #; S3<-round(S3, 4); S3; max(S3[S3!=1])

pvalue_i22<-cor_pmat(inter22); pvalue_i22
corinter22<-ggcorrplot(corr = S3,type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                      colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                      legend.title = "intensità \u03C1", lab_size = 3.5)
#grafico con p-value (x sono non significativi)
corinter22_p<-ggcorrplot(corr = S3, type = "lower",
                        lab = FALSE, show.diag=TRUE, legend.title = "intensità \u03C1", 
                        ggtheme = ggplot2::theme_dark(),
                        colors=c('steelblue','white','red3'), p.mat=pvalue_i22)
plot_grid(corinter22, corinter22_p, labels=c("R", "R con p-value"), label_size=12)
#alpha
alpha(inter22, check.keys = TRUE)
my_skim<- skim_with(base=sfl(),
                    numeric=sfl(hist=NULL))
my_skim(inter22)
n_i22<-nrow(inter22)
cov.inter22<-(n_i22-1)/n_i22*var(inter22); cov.inter22
Vt_inter22<-sum(diag(cov.inter22)); Vt_inter22
rms_inter22<-sqrt(Vt_inter22/ncol(inter22)); rms_inter22


inter23<-read.csv("inter23_bucc.csv", sep=';', header=FALSE)
colnames(inter23)<- c("D1", "D2", "D3", "D4","D5","D6"); head(inter23)
inter23$D1<-NULL
require(skimr)
my_skim<- skim_with(base=sfl(),
                    numeric=sfl(hist=NULL))
my_skim(inter23)

n_i23<-nrow(inter23)
cov.inter23<-(n_i23-1)/n_i23*var(inter23); cov.inter23
Vt_inter23<-sum(diag(cov.inter23)); Vt_inter23
rms_inter23<-sqrt(Vt_inter23/ncol(inter23)); rms_inter23
S4<-cor(inter23); S4<-round(S4, 4); S4; max(S4[S4!=1])
pvalue_i23<-cor_pmat(inter23); pvalue_i23
corinter23<-ggcorrplot(corr = S4,type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                       colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                       legend.title = "intensità \u03C1", lab_size = 3.5)
#grafico con p-value (x sono non significativi)
corinter23_p<-ggcorrplot(corr = S4, type = "lower",
                         lab = FALSE, show.diag=TRUE, legend.title = "intensità \u03C1", 
                         ggtheme = ggplot2::theme_dark(),
                         colors=c('steelblue','white','red3'), p.mat=pvalue_i23)
plot_grid(corinter23, corinter23_p, labels=c("R", "R con p-value"), label_size=12)
#alpha
alpha(inter23, check.keys = TRUE)
#test t
test_i<-t.test(inter22, inter23); test_i

#inter23cont
i23cont<-read.csv("inter23cont.csv", sep=';', header=TRUE)
i23cont$X1<-NULL
require(psych)
alpha(i23cont)

i23sing<-read.csv("inter23sing.csv", sep=';', header=TRUE)
i23sing$X1<-NULL
alpha(i23sing)
# > t.test(i22cont, i23cont) p value 0,01794 significativo
i22cont<-read.csv("i22cont.csv", sep=';', header=TRUE)
i22sing<-read.csv("i22sing.csv", sep=';', header=FALSE)
# > t.test(i22sing, i23sing) p value 0,1814 gruppi ok

#RAGAZZI
rag22<-read.csv("rag22_bucc.csv", sep=';', header=FALSE)
colnames(rag22)<- c("D1", "D2", "D3", "D4","D5","D6", "D7", "D8", "D9", "D10"); head(rag22)
rag22$D1<-NULL
my_skim<- skim_with(base=sfl(),
                    numeric=sfl(hist=NULL))
my_skim(rag22)

n_r22<-nrow(rag22)
cov.rag22<-(n_r22-1)/n_r22*var(rag22); cov.rag22
Vt_rag22<-sum(diag(cov.rag22)); Vt_rag22
rms_rag22<-sqrt(Vt_rag22/ncol(rag22)); rms_rag22

S5<-cor(rag22); S5<-round(S5, 4); S5; max(S5[S5!=1])
pvalue_r22<-cor_pmat(rag22); pvalue_r22
corrag22<-ggcorrplot(corr = S5,type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                       colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                       legend.title = "intensità \u03C1", lab_size = 3.5)
#grafico con p-value (x sono non significativi)
corrag22_p<-ggcorrplot(corr = S5, type = "lower",
                         lab = FALSE, show.diag=TRUE, legend.title = "intensità \u03C1", 
                         ggtheme = ggplot2::theme_dark(),
                         colors=c('steelblue','white','red3'), p.mat=pvalue_r22)
plot_grid(corrag22, corrag22_p, labels=c("R", "R con p-value"), label_size=20)
#alpha
alpha(rag22)
#connessione tra domande
chiquad<-chisq.test(rag22); chiquad #X-squared = 3220.2, df = 711, p-value < 2.2e-16
#AREE
emogiallo<-cbind(rag22$"D2", rag22$"D3")
emotivo<-rowMeans(emogiallo)

relverde<-cbind(rag22$"D6", rag22$"D7", rag22$"D10")
relazionale<-rowMeans(relverde)

cogrosso<-cbind(rag22$"D4", rag22$"D5", rag22$"D8", rag22$"D9")
cognitivo<-rowMeans(cogrosso)

df1<-data.frame(emotivo, relazionale, cognitivo); head(df1)
#correlazione tra aree
primo<-cor(df1)
primotot<-ggcorrplot(corr = primo, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                   colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                   legend.title = "intensità \u03C1", lab_size = 6); primotot
p_value_primo<-cor_pmat(df1)
primotot_p<-ggcorrplot(corr = primo, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                     colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                     legend.title = "intensità \u03C1", lab_size = 4, p.mat = p_value_primo)
plot_grid(primotot, primotot_p, labels=c("R", "R con p-value"), label_size = 18)

#connessione tra aree
chi1<-chisq.test(df1); chi1

#2023
rag23<-read.csv("rag23_bucc.csv", sep=';', header=FALSE)
colnames(rag23)<- c("D1", "D2", "D3", "D4","D5","D6", "D7", "D8", "D9", "D10"); head(rag23)
rag23$D1<-NULL
n_r23<-nrow(rag23)
cov.rag23<-(n_r23-1)/n_r23*var(rag23); cov.rag23
Vt_rag23<-sum(diag(cov.rag23)); Vt_rag23
rms_rag23<-sqrt(Vt_rag22/ncol(rag23)); rms_rag23
my_skim<- skim_with(base=sfl(),
                    numeric=sfl(hist=NULL))
my_skim(rag23)

S6<-cor(rag23); S6<-round(S6, 4); S6; max(S6[S6!=1])
pvalue_r23<-cor_pmat(rag23); pvalue_r23
corrag23<-ggcorrplot(corr = S6,type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                       colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                       legend.title = "intensità \u03C1", lab_size = 3.5)
#grafico con p-value (x sono non significativi)
corrag23_p<-ggcorrplot(corr = S6, type = "lower",
                         lab = FALSE, show.diag=TRUE, legend.title = "intensità \u03C1", 
                         ggtheme = ggplot2::theme_dark(),
                         colors=c('steelblue','white','red3'), p.mat=pvalue_i23)
plot_grid(corrag23, corrag23_p, labels=c("R", "R con p-value"), label_size=20)

#connessione tra aree
emogiallo2<-cbind(rag23$"D2", rag23$"D3")
emotivo<-rowMeans(emogiallo2)

relverde2<-cbind(rag23$"D6", rag23$"D7", rag23$"D10")
relazionale<-rowMeans(relverde2)

cogrosso2<-cbind(rag23$"D4", rag23$"D5", rag23$"D8", rag23$"D9")
cognitivo<-rowMeans(cogrosso2)

df_2<-data.frame(emotivo, relazionale, cognitivo); head(df_2)
#correlazione tra aree
secondo<-cor(df_2)
secondotot<-ggcorrplot(corr = secondo, type = "lower", lab = TRUE, ggtheme = ggplot2::theme_dark(),
                     colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                     legend.title = "intensità \u03C1", lab_size = 6); secondotot
p_value_secondo<-cor_pmat(df_2)
secondotot_p<-ggcorrplot(corr = secondo, type = "lower", lab = FALSE, ggtheme = ggplot2::theme_dark(),
                       colors=c('steelblue','white','red3'), show.diag=TRUE, digits=4, 
                       legend.title = "intensità \u03C1", lab_size = 4, p.mat = p_value_secondo)
plot_grid(secondotot, secondotot_p, labels=c("R", "R con p-value"), label_size = 18)

#connessione tra aree
chi2<-chisq.test(df_2); chi2

testdf<-t.test(df1, df_2)
test_corr<-t.test(cor(df1), cor(df_2))

#MEDIE
m_b22<-mean(colMeans(bamb22)); m_b22 #92.06349
m_b23<-mean(colMeans(bamb23)); m_b23 #89.79592
m_i22<-mean(colMeans(inter22)); m_i22 #82.22826
m_i23<-mean(colMeans(inter23)); m_i23 #77.88288
m_r22<-mean(colMeans(rag22)); m_r22 #76.04167
m_r23<-mean(colMeans(rag23)); m_r23 #76.98802
