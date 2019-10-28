secher<-read.table("secher.txt",header=T)

model_bpd = lm(bwt~bpd, data = secher)
sum.model_bpd<-summary(model_bpd)
R2_bpd = sum.model_bpd$r.squared
f_bpd = sum.model_bpd$fstatistic
p.value_bpd<-pf(f_bpd[1],f_bpd[2],f_bpd[3],lower.tail=F)



model_ad = lm(bwt~ad, data = secher)
sum.model_ad<-summary(model_ad)
R2_ad = sum.model_ad$r.squared
f_ad = sum.model_ad$fstatistic
p.value_ad<-pf(f_ad[1],f_ad[2],f_ad[3],lower.tail=F)

intercept_bpd = model_bpd$coefficients[1]
slope_bpd = model_bpd$coefficient[2]

intercept_ad = model_ad$coefficients[1]
slope_ad = model_ad$coefficient[2]

output<-sprintf("R2_bpd = %f, p-value_bpd = %f, R2_ad = %f, and p-value_ad = %f\n", R2_bpd, p.value_bpd, R2_ad, p.value_ad)
cat(output)

output<-sprintf("slope_bpd = %f, intercept_bpd = %f, slope_ad = %f, intercept_ad = %f",slope_bpd, intercept_bpd, slope_ad, intercept_ad)
cat(output)

png("graph1.png")
plot(bwt~bpd, data = secher)
abline(model_bpd)
dev.off()

png("graph2.png")
plot(bwt~ad, data = secher)
abline(model_ad)
dev.off()