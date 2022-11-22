setwd("path")
source("/home/mundryr/r_functions/drop1_para.r")
source("/home/mundryr/r_functions/helpers.r")

ls() ##shows active workspaces
rm(list=ls()) ##clears the active workspaces

xdata=read.csv(file="D:/kirsty_cat/GAD_FullCleanDataset.txt", header=T, stringsAsFactors=T, dec=".", sep="\t", fill=T)
str(xdata)

source("D:/linear_models/functions/diagnostic_fcns.r")
##inspect the dataset for potential mistakes
table(xdata$Success)
table(xdata$Gesture)
table(xdata$Meaning)
table(xdata$Gesture, xdata$Meaning)
table(xdata$Condition)
table(xdata$Species)
table(xdata$Ambiguous)
xdata$ambig=as.factor(c("no", "yes")[1+as.numeric(xdata$Ambiguous==1)])
table(xdata$Ambiguous, xdata$ambig)
table(xdata$Trial.Number)
xdata$Participant=xdata$Participant.Private.ID
xdata$VideoID=factor(paste("vid", as.numeric(xdata$Video), sep="."))
table(table(xdata$VideoID))
source("/home/roger/r_functions/moved_to_linmodhelpr/diagnostic_fcns.r")
xx.fe.re=fe.re.tab(fe.model="Success~Condition+ambig+Meaning+Species+Trial.Number",
	re="(1|Participant)+(1|Gesture)+(1|VideoID)", data=xdata)
xx.fe.re$summary
xx=fe.re.tab(fe.model="Success~Condition*ambig",
	re="(1|Participant)+(1|Gesture)+(1|VideoID)", data=xdata)
xx$summary
test.data=xx.fe.re$data
nrow(test.data)
nrow(xdata)
names(test.data)

test.data$Condition.Gesture=test.data$Condition.Gesture-mean(test.data$Condition.Gesture)
test.data$ambig.yes=test.data$ambig.yes-mean(test.data$ambig.yes)
test.data$Meaning.Climb.on.my.back=test.data$Meaning.Climb.on.my.back-mean(test.data$Meaning.Climb.on.my.back)
test.data$Meaning.Give.me.that.food=test.data$Meaning.Give.me.that.food-mean(test.data$Meaning.Give.me.that.food)
test.data$Meaning.Groom.me=test.data$Meaning.Groom.me-mean(test.data$Meaning.Groom.me)
test.data$Meaning.Let.s.be.friendly=test.data$Meaning.Groom.me-mean(test.data$Meaning.Groom.me)
test.data$Meaning.Let.s.have.sex=test.data$Meaning.Let.s.have.sex-mean(test.data$Meaning.Let.s.have.sex)
test.data$Meaning.Move.away.from.me=test.data$Meaning.Move.away.from.me-mean(test.data$Meaning.Move.away.from.me)
test.data$Meaning.Move.into.a.new.position=test.data$Meaning.Move.into.a.new.position-
  mean(test.data$Meaning.Move.into.a.new.position)
test.data$Species.Chimp=test.data$Species.Chimp-mean(test.data$Species.Chimp)
test.data$z.Trial.Number=as.vector(scale(test.data$Trial.Number))

#Condition.Gesture
#ambig.yes
#Meaning.Climb.on.my.back+Meaning.Give.me.that.food+Meaning.Groom.me+Meaning.Let.s.be.friendly+Meaning.Let.s.have.sex+Meaning.Move.away.from.me+Meaning.Move.into.a.new.position
#Species.Chimp
##fitting the model
load("/home/mundryr/transfer/GAD_workspace.RData")
library(lme4)
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000))
full.wac=glmer(Success~Condition*ambig+Meaning+Species+z.Trial.Number+
	(1+ambig.yes+Species.Chimp+z.Trial.Number+
    Meaning.Climb.on.my.back+Meaning.Give.me.that.food+Meaning.Groom.me+Meaning.Let.s.be.friendly+
      Meaning.Let.s.have.sex+Meaning.Move.away.from.me+Meaning.Move.into.a.new.position|Participant)+
	(1+Condition.Gesture+Species.Chimp+z.Trial.Number+
    Meaning.Climb.on.my.back+Meaning.Give.me.that.food+Meaning.Groom.me+Meaning.Let.s.be.friendly+
    Meaning.Let.s.have.sex+Meaning.Move.away.from.me+Meaning.Move.into.a.new.position|Gesture)+
	(1+Condition.Gesture+z.Trial.Number|VideoID),
	data=test.data, family=binomial, control=contr)
full.wnc=glmer(Success~Condition*ambig+Meaning+Species+z.Trial.Number+
	(1+ambig.yes+Species.Chimp+z.Trial.Number+
    Meaning.Climb.on.my.back+Meaning.Give.me.that.food+Meaning.Groom.me+Meaning.Let.s.be.friendly+
      Meaning.Let.s.have.sex+Meaning.Move.away.from.me+Meaning.Move.into.a.new.position||Participant)+
	(1+Condition.Gesture+Species.Chimp+z.Trial.Number+
    Meaning.Climb.on.my.back+Meaning.Give.me.that.food+Meaning.Groom.me+Meaning.Let.s.be.friendly+
    Meaning.Let.s.have.sex+Meaning.Move.away.from.me+Meaning.Move.into.a.new.position||Gesture)+
	(1+Condition.Gesture+z.Trial.Number||VideoID),
	data=test.data, family=binomial, control=contr)
null.wnc=glmer(Success~ambig+Meaning+Species+z.Trial.Number+
	(1+ambig.yes+Species.Chimp+z.Trial.Number+
    Meaning.Climb.on.my.back+Meaning.Give.me.that.food+Meaning.Groom.me+Meaning.Let.s.be.friendly+
      Meaning.Let.s.have.sex+Meaning.Move.away.from.me+Meaning.Move.into.a.new.position||Participant)+
	(1+Condition.Gesture+Species.Chimp+z.Trial.Number+
    Meaning.Climb.on.my.back+Meaning.Give.me.that.food+Meaning.Groom.me+Meaning.Let.s.be.friendly+
    Meaning.Let.s.have.sex+Meaning.Move.away.from.me+Meaning.Move.into.a.new.position||Gesture)+
	(1+Condition.Gesture+z.Trial.Number||VideoID),
	data=test.data, family=binomial, control=contr)
save.image("/home/mundryr/transfer/GAD_workspace.RData")
wt.txt(c.tab(as.data.frame(anova(null.wnc, full.wnc, test="Chisq")), 3))
#          npar        AIC        BIC     logLik   deviance Chisq Df Pr(>Chisq)
# null.wnc   36 116054.351 116401.103 -57991.175 115982.351    NA NA         NA
# full.wnc   38 116052.605 116418.621 -57988.302 115976.605 5.746  2      0.057
tests.full.wnc=drop1p(model.res=full.wnc, para=T, data=NULL, contr=contr, n.cores=5)
wt.txt(c.tab(tests.full.wnc$drop1.res, 3))
#                    logLik      AIC Chisq Chi.Df Pr..Chisq.      n                       n.opt.warnings n.fun.warnings conv.code
# none            -57988.30 116052.6    NA     NA         NA 112648                                                            NA
# Meaning         -57991.13 116044.3 5.649      7      0.581 112648 boundary(singular)fit:see?isSingular                        0
# Species         -57988.48 116051.0 0.359      1      0.549 112648 boundary(singular)fit:see?isSingular                        0
# z.Trial.Number  -57988.64 116051.3 0.684      1      0.408 112648 boundary(singular)fit:see?isSingular                        0
# Condition:ambig -57989.70 116053.4 2.791      1      0.095 112648 boundary(singular)fit:see?isSingular                        0
##running on ssh office:
red.wnc=glmer(Success~Condition+ambig+Meaning+Species+z.Trial.Number+
	(1+ambig.yes+Species.Chimp+z.Trial.Number+
    Meaning.Climb.on.my.back+Meaning.Give.me.that.food+Meaning.Groom.me+Meaning.Let.s.be.friendly+
      Meaning.Let.s.have.sex+Meaning.Move.away.from.me+Meaning.Move.into.a.new.position||Participant)+
	(1+Condition.Gesture+Species.Chimp+z.Trial.Number+
    Meaning.Climb.on.my.back+Meaning.Give.me.that.food+Meaning.Groom.me+Meaning.Let.s.be.friendly+
    Meaning.Let.s.have.sex+Meaning.Move.away.from.me+Meaning.Move.into.a.new.position||Gesture)+
	(1+Condition.Gesture+z.Trial.Number||VideoID),
	data=test.data, family=binomial, control=contr)
tests.red.wnc=drop1p(model.res=red.wnc, para=T, data=NULL, contr=contr, n.cores=5)
wt.txt(c.tab(tests.red.wnc$drop1.res, 3))
#                   logLik      AIC Chisq Chi.Df Pr..Chisq.      n                       n.opt.warnings n.fun.warnings conv.code
# none           -57989.70 116053.4    NA     NA         NA 112648                                                            NA
# Condition      -57991.18 116054.4 2.955      1      0.086 112648 boundary(singular)fit:see?isSingular                        0
# ambig          -57990.50 116053.0 1.608      1      0.205 112648 boundary(singular)fit:see?isSingular                        0
# Meaning        -57992.52 116045.0 5.651      7      0.581 112648 boundary(singular)fit:see?isSingular                        0
# Species        -57989.88 116051.8 0.359      1      0.549 112648 boundary(singular)fit:see?isSingular                        0
# z.Trial.Number -57990.04 116052.1 0.683      1      0.409 112648 boundary(singular)fit:see?isSingular                        0
wt2(summary(full.wnc)$coefficients)
wt2(summary(red.wnc)$coefficients)
wt2(tests.full.wnc$drop1.res)
wt2(tests.red.wnc$drop1.res)

##plotting:
#Figure1, from main dataset
bp=ggplot(dictionary, aes(x=Gesture, y=Success, fill=Meaning.s.))+geom_boxplot(outliner.colour=NA, width=.7)+geom_dotplot(binaxis="y", binwidth=.02, stackdir="center", fill=NA)+guides(fill=guide_legend(title=NULL))
bp+theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5, color="black"))+geom_hline(yintercept=.25, linetype="dashed")


#Figure2, import data Figure2data.csv
ce=ddply(AmbiHist, "Gesture", transform, percent_weight=Proportion/sum(Proportion)*100)
bp=ggplot(ce, aes(x=Gesture, y=percent_weight, fill=Response))+geom_bar(stat="identity")
bp+theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5))+ylab("Percentage of responses")+xlab("Gesture type")+scale_color_grey()+scale_fill_grey()

#FigureS2, from main dataset
source("/home/roger/roger/from_old/r_functions/factor_int_plot_fcn.r")
to.plot=aggregate(x=test.data$Success, by=test.data[, c("Participant", "Condition", "ambig")], FUN=mean)
to.plot$N=aggregate(x=test.data$Success, by=test.data[, c("Participant", "Condition", "ambig")], FUN=length)$x
dev.off()
xx=factor.int.plot(
  plot.data=to.plot, factors=c("Condition", "ambig"), coefs=fixef(full.wnc), response=to.plot$x, link="logit",
  conf.int=NULL, yaxt.labels=NULL, yaxt.at=NULL, ylab="success probability", ylim=c(0, 1), size.fac=1,
	factor.seqs=NULL, factor.labels=NULL, to.show=c("data", "boxes"), weights=to.plot$N, which.q=3,
	pch=19, pt.col=grey(level=0.6, alpha=0.25), rect.col=NA, border.col=NULL, est.ci.col=par("fg"), est.ci.lty=1, quiet=F, average.response=F, log.y=F, 
	bg="white", median.col="black", median.lwd=2, fitted.lwd=2, quant.col=NULL,
	cex.lab=1, cex.lab.x=1, cex.axis=1, reset.par=F, percentile.col=NULL, percentile.lwd=NULL, my.par=NULL, cex.axis.lab=1, xlim=NULL,
	add.range=F, range.pch=4)
mtext(at=0.5, side=1, line=0.2, text="cond:")
mtext(at=0.5, side=1, line=1.4, text="ambig:")
savePlot("/home/roger/roger/from_old/2021/to_do/kristy/FIG_ambig_cond.png", type="png")
save.image("/home/roger/roger/from_old/2021/to_do/kristy/GAD_workspace.RData")



save.image("/home/mundryr/transfer/GAD_workspace.RData")
load("/home/roger/roger/from_old/2021/to_do/kirsty/GAD_workspace.RData")

save.image("D:/kristy_cat/GAD_workspace.RData")

##I THINK EVERYTHING BELOW THIS LINE WAS MY OLD STUFF AND SHOULD BE REMOVED:
	
full=glmer(Success~Gesture*Condition*Ambiguous+Meaning+Species+
	(1|Participant), ##participant ID as a random effect, no random slopes
	data=xdata, family=binomial)
##model failed to converge, increasing number iterations
contr=lmerControl(optCtrl=list(maxfun=100000))
full=glmer(Success~Gesture*Condition*Ambiguous+Meaning+Species+
	(1|Participant), data=xdata, family=binomial, control=contr)
##model failed to converge, changing optimizer
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
full=glmer(Success~Gesture*Condition*Ambiguous+Meaning+Species+
	(1|Participant), data=xdata, family=binomial, control=contr)
summary(full)
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000))
full=glmer(Success~Condition*(Gesture+Ambiguous)+Meaning+Species+
	(1|Participant), data=xdata, family=binomial, control=contr)
summary(full)
##still failed to converge, try removing interaction
red1=glmer(Success~Gesture*Condition+Ambiguous+Meaning+Species+
	(1|Participant), data=xdata, family=binomial, control=contr)
red2=glmer(Success~Gesture+Condition+Ambiguous+Meaning+Species+
	(1|Participant), data=xdata, family=binomial, control=contr)
##red2 converges - interaction seems to be the problem
summary(red2)

table(xdata$Gesture, xdata$Condition, xdata$Ambiguous)
table(xdata$Gesture, xdata$Condition)
table(xdata$Gesture, xdata$Ambiguous)
table(xdata$Condition, xdata$Ambiguous)

table(xdata$Gesture)
table(xdata$Condition)
table(xdata$Ambiguous)
table(xdata$Meaning)
table(xdata$Species)
table(xdata$Participant)
table(xdata$Participant, )

##checking for overdispersion
source("C:/Users/kg960/Desktop/Admin/MUNDRY STATS/diagnostic_fcns.r")
overdisp.test(red2) ##close to but less than 1

##checking the random effects
summary(red2)$varcor

##check distribution of random effects
ranef.diagn.plot(red2) ##looks great!

##full-null model comparison I DID THIS FIRST BECAUSE MY LAPTOP COULD HANDLE IT
null2=glmer(Success ~ Species + (1|Participant), data=xdata, family=binomial)
as.data.frame(anova(null2, red2, test="Chisq"))

summary(red2)$coefficients

##check model stability 
source("C:/Users/kg960/Desktop/Admin/MUNDRY STATS/glmm_stability.r")
m.stab=glmm.model.stab(model.res=red2, contr=contr) ##THIS IS WHERE MY LAPTOP GAVE UP!

##AFTER MODEL STABILITY THIS NEEDS RUNNING NEXT
table(m.stab$detailed$lme4.warnings)
table(m.stab$detailed$opt.warnings)
round(m.stab$summary[, -1], 3)
m.stab.plot(m.stab$summary[, -1])

##confidence intervals NEEDS RUNNING
source("C:/Users/kg960/Desktop/Admin/MUNDRY STATS/boot_glmm.r")
boot.bin=boot.glmm.pred(model.res=red2, excl.warnings=F,
	para=F, use=c("Gesture","Condition","Ambiguous","Meaning","Species"))
m.stab.plot(boot.bin$ci.estimates)




setwd("C:/Users/kg960/Desktop/Gorilla Exp/2020 GAD Paper")
save.image("2020_GAD_Paper.RData")
load("2020_GAD_Paper.RData")
load("/home/roger/roger/2021/to_do/kristy/2020_GAD_Paper.RData")
load("E:/kristy/2020_GAD_Paper.RData")


