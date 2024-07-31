###AVISO
#Para esse código rodar, é necessário antes rodar os códigos imputando a média e a mediana com n = 100 3 n = 1000

#Para n = 100

Comparando.data.100 <- data.frame("Media" = c(mAR02.tab5.100, mAR02.tab10.100, mAR02.tab20.100,
                                         mAR02.tab40.100, mAR02_2.tab5.100, mAR02_2.tab10.100,
                                         mAR02_2.tab20.100, mAR02_2.tab40.100, mARMA1.tab5.100,
                                         mARMA1.tab10.100, mARMA1.tab20.100, mARMA1.tab40.100),
                             "Mediana" = c(mAR02.tab5.med.100,
                                           mAR02.tab10.med.100,
                                           mAR02.tab20.med.100,
                                           mAR02.tab40.med.100,
                                           mAR02.tab5_2.med.100,
                                           mAR02.tab10_2.med.100,
                                           mAR02.tab20_2.med.100,
                                           mAR02.tab40_2.med.100,
                                           mARMA1.tab5.med.100,
                                           mARMA1.tab10.med.100,
                                           mARMA1.tab20.med.100,
                                           mARMA1.tab40.med.100))

attach(Comparando.data.100)

wilcox.100 <- wilcox.test(Media,Mediana, paired = TRUE, alternative ="less")
wilcox.100

#Para n = 1000

Comparando.data.1000 <- data.frame("media" = c(mAR02.tab5,
                                               mAR02.tab10,
                                               mAR02.tab20,
                                               mAR02.tab40,
                                               mAR02.tab5_2,
                                               mAR02.tab10_2,
                                               mAR02.tab20_2,
                                               mAR02.tab40_2,
                                               mARMA1.tab5,
                                               mARMA1.tab10,
                                               mARMA1.tab20,
                                               mARMA1.tab40),
                                  "mediana" = c(mAR02.tab5.med,
                                                mAR02.tab10.med,
                                                mAR02.tab20.med,
                                                mAR02.tab40.med,
                                                mAR02.tab5_2.med,
                                                mAR02.tab10_2.med,
                                                mAR02.tab20_2.med,
                                                mAR02.tab40_2.med,
                                                mARMA1.tab5.med,
                                                mARMA1.tab10.med,
                                                mARMA1.tab20.med,
                                                mARMA1.tab40.med))

attach(Comparando.data.1000)

wilcox.1000 <- wilcox.test(media,mediana, paired = TRUE, alternative ="less")
wilcox.1000
