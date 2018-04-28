data=read.table("http://www.stat.ufl.edu/~athienit/STA6166/assignment3_1.txt",header=FALSE,col.names=c('id_number', 'psa_level', 'cancer_volume', 'weight', 'age', 'benign_prostatic_hyperplasia', 'seminal_vesicle_invasion', 'capsular_penetration', 'gleason_score'))

data_test=data
data_test$cancer_volume=(data_test$cancer_volume-mean(data_test$cancer_volume))/sd(data_test$cancer_volume)
data_test$weight=(data_test$weight-mean(data_test$weight))/sd(data_test$weight)
data_test$age=(data_test$age-mean(data_test$age))/sd(data_test$age)
data_test$benign_prostatic_hyperplasia=(data_test$benign_prostatic_hyperplasia-mean(data_test$benign_prostatic_hyperplasia))/sd(data_test$benign_prostatic_hyperplasia)
data_test$capsular_penetration=(data_test$capsular_penetration-mean(data_test$capsular_penetration))/sd(data_test$capsular_penetration)
data_test$gleason_score=(data_test$gleason_score-mean(data_test$gleason_score))/sd(data_test$gleason_score)
data_rescale = data_test

reg2=lm(data_rescale_x$psa_level~data_rescale_x$cancer_volume+data_rescale_x$weight+data_rescale_x$age+data_rescale_x$benign_prostatic_hyperplasia+data_rescale_x$benign_prostatic_hyperplasia+data_rescale_x$seminal_vesicle_invasion+data_rescale_x$capsular_penetration+data_rescale_x$gleason_score+data_rescale_x$seminal_vesicle_invasion*(data_rescale_x$cancer_volume+data_rescale_x$weight+data_rescale_x$age+data_rescale_x$benign_prostatic_hyperplasia+data_rescale_x$capsular_penetration+data_rescale_x$gleason_score))
allcruise <- regsubsets(log(psa_level) ~ cancer_volume + weight + age + benign_prostatic_hyperplasia + benign_prostatic_hyperplasia + seminal_vesicle_invasion + capsular_penetration+gleason_score+seminal_vesicle_invasion*(cancer_volume+weight+age+benign_prostatic_hyperplasia+capsular_penetration+gleason_score), nbest=4,data=data_rescale)
aprout <- summary(allcruise)

stripchart(solvants~group, data=df, method="stack", vertical=TRUE,pch=1, cex=1.5, xlab="Group", ylab="sorption rate", main="Dotplots by Sorption Rate")