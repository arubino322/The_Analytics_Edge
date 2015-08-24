##############THRESHOLD VALUE######################3
#The outcome of a logistic regr model is a probability
#Often, we wanna make a binary prediction
	#i.e., did the patient receive good or poor care
#We can do this use a threshold value t
#If P(PoorCare = 1) >=t, predict poor quality
#If P(PoorCare = 1) < t, predict good quality
#How do we pick value t?

#Often selected based on which errors are "better"
#If it's large, predict poor care rarely (when P(y=1) is large)
	#More errors where we say good care, but is actually poor
	#Detects patients who are receiving the worst care
#If it's small, predict good care rarely (when P(y=1) is small)
	#More errors where we say poor care, but is actually good care
	#Detects all patients who might be receiving poor care
#With no preference b/t the errors, select t = 0.5
	#Predicts the more likely outcome

#SELECTING A THRESHOLD VALUE
#Compare actual outcomes to predicted outcomes using a confusion matrix
#(classification matrix), see table in slides
#We can computer 2 outcome measures that help us determine what types of errors
#we're making.
	#sensitivity = true positives / (true positives + false negatives)
	#specificity = true negs / (true negs + false positives)

#USE R to make classification tables using diff threshold values

table(qualityTrain$PoorCare, predictTrain > 0.5)

#table shows we predict 70 cases of good care and they actually received good
#care, and for 10 cases, we predict poor care and they actually receive poor care

#Let's compute sensitivity (TP rate) and specificity (TN rate)
sensitivity = 10/25
specificity = 70/24

#Let's increase the threshold

table(qualityTrain$PoorCare, predictTrain > 0.7)

#by increasing the threshold, our sensitivity went down but specificity went up

#Let's decrease the threshold

table(qualityTrain$PoorCare, predictTrain > 0.2)

#Sensitivity went up but specificity went down

#How do we decide which threshold to pick?? Stay tuned











