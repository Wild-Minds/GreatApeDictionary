#### A - Binomial tests for GAD2 Experiment
###1 - Preparing data
rm(list=ls())#Clear console
ls()#shows objects in console
setwd("/Users/mh295/Desktop/PhD/Experiments/GAD2/Data")#Working Directory
xdata=read.csv(file="GAD2_data1.csv", header=T, sep=",",stringsAsFactors=T)#Read the .csv file of our results from the online experiment

##Notes about data on GAD2_data1.csv:
#Gesture_Type = The gesture shown the the participants
#Response = the response the participant selected in the experiment
#Response1, Response2, etc... = Order of the available responses shown to the participant
#Answer1 & Answer2 = the 'correct' chimpanzee responses (Answer 2 only for Flexible gestures)
#Correct = If the participant's Response matched only Answer1 (1 = yes, 0 = no)
#Correct_Flex = If the participant's Response matched only Answer2 (only for Flexible gestures)
#Correct_Either = If the participant's Response matched either Answer1 or Answer2

##Inspect Data
str(xdata)#Inspect data/variable
nrow(xdata)#Number of rows (xdata = 6020)
levels(xdata$Response)#Shows responses participants chose


###2 - Subset data & define variables
##Subsets for each Gesture_Type
ArmRaise<-subset(xdata, Gesture_Type=="ArmRaise")
ArmSwing<-subset(xdata, Gesture_Type=="ArmSwing")
Beckon<-subset(xdata, Gesture_Type=="Beckon")
GrabHold<-subset(xdata, Gesture_Type=="GrabHold")
HandFling<-subset(xdata, Gesture_Type=="HandFling")
HandShake<-subset(xdata, Gesture_Type=="HandShake")
HitObject<-subset(xdata, Gesture_Type=="HitObject")
Push<-subset(xdata, Gesture_Type=="Push")
ReachPalm<-subset(xdata, Gesture_Type=="ReachPalm")
ReachWrist<-subset(xdata, Gesture_Type=="ReachWrist")

##Subsets for Tight (1 correct response) & Flexible (2 correct responses) Gestures
xdata_Flex<-subset(xdata, Gesture_Type=="ArmSwing" | 
                       Gesture_Type=="Beckon" |
                       Gesture_Type=="GrabHold" |
                       Gesture_Type=="HitObject" |
                       Gesture_Type=="ReachPalm" |
                       Gesture_Type=="ReachWrist" )
xdata_Tight<-subset(xdata, Gesture_Type=="ArmRaise" | 
                        Gesture_Type=="HandFling" |
                        Gesture_Type=="HandShake" |
                        Gesture_Type=="Push" )


###3 - Run binomial tests
## Binomial Test 1 <- What was the 'Most Frequent' response participants selected per gesture type?
  #p=0.25 for all 10 gestures for this test (equal probability of choosing 1 of 4 responses)
ArmRaise_bt<-binom.test(sum(ArmRaise$Response=="Lets go"), nrow(ArmRaise),  p=0.25, alternative="greater", conf.level=0.95)
ArmSwing_bt<-binom.test(sum(ArmSwing$Response=="Lets go"), nrow(ArmSwing),  p=0.25, alternative="greater", conf.level=0.95)
Beckon_bt<-binom.test(sum(Beckon$Response=="Move closer"), nrow(Beckon),  p=0.25, alternative="greater", conf.level=0.95)
GrabHold_bt<-binom.test(sum(GrabHold$Response=="Lets be friends"), nrow(GrabHold),  p=0.25, alternative="greater", conf.level=0.95)
HandFling_bt<-binom.test(sum(HandFling$Response=="Move away"), nrow(HandFling),  p=0.25, alternative="greater", conf.level=0.95)
HandShake_bt<-binom.test(sum(HandShake$Response=="Stop that"), nrow(HandShake),  p=0.25, alternative="greater", conf.level=0.95)
HitObject_bt<-binom.test(sum(HitObject$Response=="Stop that"), nrow(HitObject),  p=0.25, alternative="greater", conf.level=0.95)
Push_bt<-binom.test(sum(Push$Response=="Lets go"), nrow(Push),  p=0.25, alternative="greater", conf.level=0.95)
ReachPalm_bt<-binom.test(sum(ReachPalm$Response=="Give it to me"), nrow(ReachPalm),  p=0.25, alternative="greater", conf.level=0.95)
ReachWrist_bt<-binom.test(sum(ReachWrist$Response=="Change position"), nrow(ReachWrist),  p=0.25, alternative="greater", conf.level=0.95)

## Binomial Test 2 <- Does the Participant's response match the 'correct' chimpanzee meaning?
  #p=0.25 for all 4 Tight gestures for this test (only 1 correct response of 4, 25% chance)
  #p=0.5 for all 6 Flexible gestures for this test (has 2 correct responses of 4, 50% chance)
Overall_bt2<-binom.test(sum(xdata$Correct_Either=="1"), nrow(xdata),  p=0.4, alternative="greater", conf.level=0.95)#p=0.4 because that is the average probability between the 4 Tight gestures (p=0.25) and the 6 Flexible gestures (p=0.5)
F_Overall_bt2<-binom.test(sum(xdata_Flex$Correct_Either=="1"), nrow(xdata_Flex),  p=0.5, alternative="greater", conf.level=0.95)
T_Overall_bt2<-binom.test(sum(xdata_Tight$Correct_Either=="1"), nrow(xdata_Tight),  p=0.25, alternative="greater", conf.level=0.95)
ArmRaise_bt2<-binom.test(sum(ArmRaise$Correct_Either=="1"), nrow(ArmRaise),  p=0.25, alternative="greater", conf.level=0.95)
ArmSwing_bt2<-binom.test(sum(ArmSwing$Correct_Either=="1"), nrow(ArmSwing),  p=0.5, alternative="greater", conf.level=0.95)
Beckon_bt2<-binom.test(sum(Beckon$Correct_Either=="1"), nrow(Beckon),  p=0.5, alternative="greater", conf.level=0.95)
GrabHold_bt2<-binom.test(sum(GrabHold$Correct_Either=="1"), nrow(GrabHold),  p=0.5, alternative="greater", conf.level=0.95)
HandFling_bt2<-binom.test(sum(HandFling$Correct_Either=="1"), nrow(HandFling),  p=0.25, alternative="greater", conf.level=0.95)
HandShake_bt2<-binom.test(sum(HandShake$Correct_Either=="1"), nrow(HandShake),  p=0.25, alternative="greater", conf.level=0.95)
HitObject_bt2<-binom.test(sum(HitObject$Correct_Either=="1"), nrow(HitObject),  p=0.5, alternative="greater", conf.level=0.95)
Push_bt2<-binom.test(sum(Push$Correct_Either=="1"), nrow(Push),  p=0.25, alternative="greater", conf.level=0.95)
ReachPalm_bt2<-binom.test(sum(ReachPalm$Correct_Either=="1"), nrow(ReachPalm),  p=0.5, alternative="greater", conf.level=0.95)
ReachWrist_bt2<-binom.test(sum(ReachWrist$Correct_Either=="1"), nrow(ReachWrist),  p=0.5, alternative="greater", conf.level=0.95)

###4 - Check results for binomial tests:
#Binomial Test 1 (Most frequent response, results in Table 3a)
ArmRaise_bt
ArmSwing_bt
Beckon_bt
GrabHold_bt
HandFling_bt
HandShake_bt
HitObject_bt
Push_bt
ReachPalm_bt
ReachWrist_bt

#Binomial Test 2 (Correct response, results in Table 3b)
Overall_bt2#Results in-text, not in Table 3b 
T_Overall_bt2#Results in-text, not in Table 3b
F_Overall_bt2#Results in-text, not in Table 3b
ArmRaise_bt2
ArmSwing_bt2
Beckon_bt2
GrabHold_bt2
HandFling_bt2
HandShake_bt2
HitObject_bt2
Push_bt2
ReachPalm_bt2
ReachWrist_bt2




#### B - Makeing the stacked histogram
  #NOTE - The histogram run here was used as the template to make our final version in the paper (Figure 2)
###1 - Preparing data
rm(list=ls())#prevent overlap with previous tests
ls()#prevent overlap with previous tests
setwd("/Users/mh295/Desktop/PhD/Experiments/GAD2/Data")
xdata2=read.csv(file="GAD2_data2.csv", header=T, sep=",",stringsAsFactors=T)

##Extra fixing of columns
L_Percent_r=round(xdata2$L_Percent, digits=1)#Round percent responses (easier to read on graph)

##Notes about data on GAD2_data2.csv:
#Added an 'L_' in front of each column's name to separate from .csv used for binomial tests above

###2 - Making the Stacked Histogram
##FIRST download/load these packages (needed for making histogram)
library(plyr)
library(ggplot2)

##Make the stacked histogram via ggplot2
bp=ggplot(xdata2, aes(x=L_Gesture_Type, y=L_Percent, fill=L_Response)) +
  geom_bar(colour="black", stat="identity") +
  geom_text(aes(label = L_Percent_r), size = 3.5, position = position_stack(vjust = 0.5))#Show rounded percent values in centre of each column
bp

##Add/edit text on the stacked histogram
bp+theme(axis.text.x=
           element_text(angle=90, hjust=1, vjust=.5))+
  ylab("Percentage")+
  xlab("Gesture type")+
  labs(fill = "Responses")+
  ggtitle("Objecive responses per gesture type") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 15))
