ensemble=(0.4*GBMProbs+0.4*downProbs)/2

SubmitData = data.frame(CAX_ID = COPDTest$CAX_ID)
SubmitData$Exacer = ensemble
write.csv(SubmitData, file = "submission_17_01.csv", row.names = FALSE) 
