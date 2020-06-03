library(RMySQL)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(RODBC)
library(orddom)
library(dplyr)
library(devtools)
library(easyGgplot2)
library(MBESS)
library(pROC)
library(car)
library(effsize)

con<-dbConnect(MySQL(),dbname='stackoverflow2018new',host='localhost',password='******',user='root')

# FA threshold
response_time = dbGetQuery(con,"select response_time from AnswerRegressionModel")
quantile(response_time$response_time,0.2) # 439s 20%:559s
p<-ggplot(data=res_time,aes(x=res_time$response_time))+geom_histogram(bins = 100,fill="#5ab4ac")+scale_x_log10()
p + theme(axis.title = element_text(size = 14),axis.text = element_text(size = 13)) + xlab("Response time (log scale)") + ylab("Number of answers")

# RQ1: popularity of FAs
# ***annual growth of FAs and related indicators
# annual number of questions
question_num_by_year = dbGetQuery(con,"select LEFT(CreationDate,4) as year, count(*) as count from Posts where PostTypeId = 1 group by LEFT(CreationDate,4)")
# annual number of answers
answer_num_by_year = dbGetQuery(con,"select LEFT(CreationDate,4) as year, count(*) as count from Posts where PostTypeId = 2 group by LEFT(CreationDate,4)")
# annual number of FAs
rr_question_num_by_year <- dbGetQuery(con,"select LEFT(CreationDate,4) as year, count(*) as count from Posts where PostTypeId = 2 and response_time <= 439 group by LEFT(CreationDate,4)")
# annual number of FA questions
question_rr_by_year <- dbGetQuery(con,"select LEFT(CreationDate,4) as year, count(*) as count from Posts where PostTypeId = 1 and rapid_answer_num > 0 group by LEFT(CreationDate,4)")
# annual number of FA answerers
rr_devs_by_years <- dbGetQuery(con,"select LEFT(CreationDate,4) as year, count(distinct OwnerUserId) as count from Posts where PostTypeId = 2 and response_time <= 439 group by LEFT(CreationDate,4)")
# annual number of all answerers
all_answerers_by_year <- dbGetQuery(con,"select LEFT(CreationDate,4) as year, count(distinct OwnerUserId) as count from Posts where PostTypeId = 2 group by LEFT(CreationDate,4)")
# all indicators
all_indicators_by_year <- data.frame(question_num_by_year,answer_num_by_year$count,rr_question_num_by_year$count,question_rr_by_year$count,rr_devs_by_years$count,all_answerers_by_year$count)
names(all_indicators_by_year) = c("Year","questions","answers","FAs","FA questions","FA answerers","all answerers")
all_indicators_by_year_melted <- melt(all_indicators_by_year,id.vars = c("Year"), variable.name = "Indicators", value.name = "Quantity")
all_indicators_by_year_melted
ggplot(all_indicators_by_year_melted,aes(x=year,y=quantity,group=indicators,color=indicators))+geom_line()+geom_point()
ggplot(all_indicators_by_year_melted,aes(x=Year,y=log(Quantity),group=Indicators,color=Indicators))+geom_line()+geom_point(aes(shape=Indicators),size = 4)+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 19),legend.text = element_text(size = 18),legend.title = element_text(size = 19))

# ***annual growth of FA ratios
dev_count <- data.frame(rr_devs_by_years$year,round(rr_question_num_by_year$count/answer_num_by_year$count,2))
names(dev_count)=c("Year","FA_answers_ratio")
ggplot(dev_count,aes(x=Year,y=FA_answers_ratio,group=1))+geom_line(colour="red")+geom_point(colour="red",size =4)+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 19),legend.text = element_text(size = 18),legend.title = element_text(size = 19))+ylab("Ratio")


# RQ2: user behaviors in response to FAs
# *** Answerers' behaviors
# FA_membership
fa_membership <- dbGetQuery(con,"select answer_membership from Posts where answer_membership > 0")
summary(fa_membership$answer_membership)
ggplot(fa_membership,aes(x=answer_membership/86400))+geom_histogram(bins = 30,fill="#f8766d")+xlab("")+ylab("")
# first_last_FA_interval
first_last_FA_spans <- dbGetQuery(con,"select FirstLastFASpan from FirstLastFASpan")
summary(first_last_FA_spans$FirstLastFASpan)
ggplot(first_last_FA_spans,aes(x=FirstLastFASpan/86400))+geom_histogram(bins = 30,fill="#f8766d")+xlab("")+ylab("")
# FA_user_life_span
fa_membership <- dbGetQuery(con,"select reserve8 as AnswererMembership from AnswerRegressionModel where isFastAnswer = 1 and reserve8 > 0")
summary(fa_membership$AnswererMembership)
# FA_interval
rr_spans <- dbGetQuery(con,"select AnswerSpan from FastAnswerSpan")
summary(rr_spans$AnswerSpan)
ggplot(rr_spans,aes(x=AnswerSpan))+geom_histogram(bins = 30,fill="#f8766d")+xlab("")+ylab("")
# FA_question_complexity
tag_complexity <- dbGetQuery(con,"select TagNumber from FastAnswers")
summary(tag_complexity$TagNumber)
sd(tag_complexity$TagNumber)
ggplot(tag_complexity)+geom_histogram(aes(x=as.numeric(TagNumber)),bins = 30,fill="#f8766d")+xlab("")+ylab("")
nfa_tag_complexity <- dbGetQuery(con,"select PostId from NFATags")
wilcox.test(tag_complexity$TagNumber,nfa_tag_complexity$PostId)
summary(nfa_tag_complexity$PostId)
sd(nfa_tag_complexity$PostId)
cohd2delta(0.280704)
# FA_acceptance_time
all_ques_with_accepted_ans <- dbGetQuery(con,"select AcceptanceTime from AcceptanceTime where response_time > 439")
summary(all_ques_with_accepted_ans$AcceptanceTime)
sd(all_ques_with_accepted_ans$AcceptanceTime)
quantile(all_ques_with_accepted_ans$AcceptanceTime,0.8)
ggplot(all_ques_with_accepted_ans)+geom_histogram(aes(x=log(as.numeric(AcceptanceTime))),bins = 30,fill="#f8766d")+xlab("")+ylab("")
all_ques_with_accepted_fas <- dbGetQuery(con,"select * from AcceptanceTime where response_time <= 439")
summary(all_ques_with_accepted_fas$AcceptanceTime)
sd(all_ques_with_accepted_fas$AcceptanceTime)
# accepted_answer_rank
ques_accepted_rank <- dbGetQuery(con,"select AcceptanceRank from AcceptanceTime where response_time <= 439")
ggplot(ques_accepted_rank)+geom_histogram(aes(x=as.numeric(AcceptanceRank)),bins = 30,fill="#f8766d")+xlab("")+ylab("")
summary(ques_accepted_rank$AcceptanceRank)
all_ques_accepted_rank <- dbGetQuery(con,"select AcceptanceRank from AcceptanceTime")
ggplot(all_ques_accepted_rank)+geom_histogram(aes(x=as.numeric(AcceptanceRank)),bins = 30,fill="#f8766d")+xlab("")+ylab("")
# voting_time
all_voting_time <- dbGetQuery(con,"select TIMESTAMPDIFF(DAY, p.CreationDate, v.CreationDate), p.response_time from Posts p, Votes v where (v.VoteTypeId = 2 or v.VoteTypeId = 3) and v.PostId = p.Id and p.PostTypeId = 2")
summary(all_voting_time$time_span)
names(all_voting_time) = c("time_span","response_time")
ggplot(all_voting_time)+geom_histogram(aes(x=as.numeric(time_span)),bins = 30,fill="#f8766d")+xlab("")+ylab("")
fa_voting_time <- subset(all_voting_time,response_time <= 439)
summary(fa_voting_time$time_span)
ggplot(fa_voting_time)+geom_histogram(aes(x=as.numeric(time_span)),bins = 30,fill="#f8766d")+xlab("")+ylab("")
# voted_answer_rank
all_voting_rank <- dbGetQuery(con,"select VotedAnswerRank from Votes where VotedAnswerRank is not NULL")
summary(all_voting_rank$VotedAnswerRank)
ggplot(all_voting_rank)+geom_histogram(aes(x=as.numeric(VotedAnswerRank)),bins = 30,fill="#f8766d")+xlab("")+ylab("")
# FA_score_rank
edit_times=dbGetQuery(con,"select ScoreRank from FastAnswers")
summary(edit_times$ScoreRank)
ggplot(edit_times)+geom_histogram(aes(x=as.numeric(ScoreRank)),bins = 40,fill="#f8766d")+xlab("")+ylab("")
# FA_answerer_edits
authorEditTimes=dbGetQuery(con,"select authorEditTimes from FastAnswers")
summary(authorEditTimes$authorEditTimes)
ggplot(authorEditTimes)+geom_histogram(aes(x=as.numeric(authorEditTimes)),bins = 30,fill="#f8766d")+xlab("")+ylab("")
# FA_others_edits
otherEditTimes=dbGetQuery(con,"select otherEditTimes from FastAnswers")
summary(otherEditTimes$otherEditTimes)
ggplot(otherEditTimes)+geom_histogram(aes(x=as.numeric(otherEditTimes)),bins = 30,fill="#f8766d")+xlab("")+ylab("")

# RQ3：Quality of FAs
# ***Answer body quality of FAs and NFAs:
# Length of answers
TextLengthFA <- dbGetQuery(con,"select TextLength from AnswerRegressionModel where isFastAnswer = 1 and TextLength is not NULL")
TextLengthNFA <- dbGetQuery(con,"select TextLength from AnswerRegressionModel where isFastAnswer = 0 and TextLength is not NULL")
wilcox.test(TextLengthFA$TextLength,TextLengthNFA$TextLength)
sd(TextLengthFA$TextLength)
mean(TextLengthFA$TextLength)
sd(TextLengthNFA$TextLength)
mean(TextLengthNFA$TextLength)
summary(TextLengthNFA$TextLength)
cohd2delta(-0.3984965)
# Number of code snippets
nCodeSnippetsFA <- dbGetQuery(con,"select Reserve1 from AnswerRegressionModel where isFastAnswer = 1 and Reserve1 is not NULL")
nCodeSnippetsNFA <- dbGetQuery(con,"select Reserve1 from AnswerRegressionModel where isFastAnswer = 0 and Reserve1 is not NULL")
wilcox.test(TextLengthFA$Reserve1,TextLengthNFA$Reserve1)
summary(nCodeSnippetsFA$Reserve1)
sd(nCodeSnippetsFA$Reserve1)
summary(nCodeSnippetsNFA$Reserve1)
sd(nCodeSnippetsNFA$Reserve1)
cohd2delta(-0.0174875)
# Length of code snippets
CodeSnippetLengthFA <- dbGetQuery(con,"select CodeSnippetLength from AnswerRegressionModel where isFastAnswer = 1 and CodeSnippetLength is not NULL")
CodeSnippetLengthNFA <- dbGetQuery(con,"select CodeSnippetLength from AnswerRegressionModel where isFastAnswer = 0 and CodeSnippetLength is not NULL")
wilcox.test(CodeSnippetLengthFA$Reserve1,CodeSnippetLengthNFA$Reserve1)
sd(CodeSnippetLengthNFA$CodeSnippetLength)
mean(CodeSnippetLengthNFA$CodeSnippetLength)
sd(CodeSnippetLengthFA$CodeSnippetLength)
mean(CodeSnippetLengthFA$CodeSnippetLength)
cohd2delta(-0.2647143)
# Number of URLs
nURLsFA <- dbGetQuery(con,"select Reserve2 from AnswerRegressionModel where isFastAnswer = 1 and Reserve2 is not NULL")
nURLsNFA <- dbGetQuery(con,"select Reserve2 from AnswerRegressionModel where isFastAnswer = 0 and Reserve2 is not NULL")
wilcox.test(TextLengthFA$TextLength,TextLengthNFA$TextLength)
sd(nURLsFA$Reserve2)
summary(nURLsFA$Reserve2)
sd(nURLsNFA$Reserve2)
summary(nURLsNFA$Reserve2)
cohd2delta(-0.1246794)
# readability index
readFA <- dbGetQuery(con,"select ReadabilityIndex from AnswerRegressionModel where isFastAnswer = 1 and ReadabilityIndex is not NULL")
readNFA <- dbGetQuery(con,"select ReadabilityIndex from AnswerRegressionModel where isFastAnswer = 0 and ReadabilityIndex is not NULL")
wilcox.test(readFA$ReadabilityIndex,readNFA$ReadabilityIndex)
sd(readFA$ReadabilityIndex)
mean(readFA$ReadabilityIndex)
sd(readNFA$ReadabilityIndex)
mean(readNFA$ReadabilityIndex)
cohd2delta(-0.2242456)
# Crowd assessment model
all_response_time=dbGetQuery(con,"select response_time,TextLength,CodeSnippetLength,ReadabilityIndex,WilsonScore,IsAccepted,Reserve1,Reserve2,nAnswers,nQuestions,nUpvotesPerAnswer,nAcceptanceRatio,reserve8,reserve9,reserve10,reserve12,isFastAnswer from AnswerRegressionModel where OwnerUserId is not NULL and QuestionScore is NULL and IsPostedAfterAcceptance is NULL and AcceptedAnswerId is not NULL and HasFA is NULL")
names(all_response_time) = c("response_time","TextLength","CodeSnippetLength","ReadabilityIndex","WilsonScore","IsAccepted","nCodeSnippets","nURLs","nAnswers","nQuestions","nUpvotesPerAnswer","nAcceptanceRatio","AnswererMembership","QuestionWilsonScore","nAnswersForQuestion","nCommentsForAnswer","isFastAnswer")
all_response_time_final <- all_response_time
outliers <- quantile(all_response_time$WilsonScore,0.99)
all_response_time_final <- subset(all_response_time,WilsonScore<outliers)
scaled_metrics <- data.frame(all_response_time_final$response_time,all_response_time_final$TextLength,all_response_time_final$CodeSnippetLength,all_response_time_final$WilsonScore,all_response_time_final$nCodeSnippets,all_response_time_final$nURLs,all_response_time_final$nAnswers,all_response_time_final$nQuestions,all_response_time_final$AnswererMembership,all_response_time_final$QuestionWilsonScore,all_response_time_final$nAnswersForQuestion,all_response_time_final$nCommentsForAnswer)
scaled_metrics_logged <- log(scaled_metrics+0.5)
scaled_metrics_logged_scaled <- scale(scaled_metrics_logged)
regression_metrics <- data.frame(scaled_metrics_logged_scaled,all_response_time_final$nAcceptanceRatio,all_response_time_final$nUpvotesPerAnswer,all_response_time_final$isFastAnswer,all_response_time_final$ReadabilityIndex,all_response_time_final$IsAccepted)
names(regression_metrics) = c("response_time","TextLength","CodeSnippetLength","ReadabilityIndex","WilsonScore","nCodeSnippets","nURLs","nAnswers","nQuestions","AnswererMembership","QuestionWilsonScore","nAnswersForQuestion","nCommentsForAnswer","nAcceptanceRatio","nUpvotesPerAnswer","isFastAnswer","IsAccepted")
crowd_assessment_model <- lm(WilsonScore~nAnswers+AnswererMembership+QuestionWilsonScore+nAnswersForQuestion+nCommentsForAnswer+nUpvotesPerAnswer+nAcceptanceRatio+TextLength+CodeSnippetLength+nCodeSnippets+nURLs+ReadabilityIndex+isFastAnswer,data=regression_metrics)
summary(crowd_assessment_model)
crowd_assessment_model$coefficients
anova_res <- anova(crowd_assessment_model)
anova_res
vif(WilsonScore_model_scaled,digits = 3)
# Asker assessment model
asker_model_scaled <- glm(IsAccepted~nAnswers+AnswererMembership+QuestionWilsonScore+nAnswersForQuestion+nCommentsForAnswer+nUpvotesPerAnswer+nAcceptanceRatio+TextLength+CodeSnippetLength+nCodeSnippets+nURLs+ReadabilityIndex+isFastAnswer,family=binomial(link='logit'),data=regression_metrics)
summary(asker_model_scaled)
vif(asker_model_scaled,digits = 3)
pR2(asker_model_scaled)
asker_model_scaled <- step(asker_model_scaled)
# Cox & snell R2, Nagelkerke R2
asker_model_scaled <- anova(asker_model_scaled, test="Chisq")
asker_model_scaled
asker_model_scaled$Deviance
pre=predict(asker_model_scaled,type='response')
modelroc=roc(asker_model_scaled$model$IsAccepted,pre)
modelroc$auc
anova(edits_model) # >0.1%
# Benjamini-Hochberg step-down procedure
models <- c(summary(crowd_assessment_model)$coefficients[,4],summary(asker_model_scaled)$coefficients[,4])
models_corrected <- p.adjust(models,"BH")
models_corrected
