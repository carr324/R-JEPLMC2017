#Define the exemplar model as per Nosofsky's papers

exemplar_model=function(train,test,labels,spec,gamma){

#Place to store familiarities
familiarity_test=numeric(length(test))
#Place to store categorization probabilities
prA_test=numeric(length(test))

for(i in 1:length(test)){
	
	#Similarity to category A calc
	sim_catA=sum(exp(-spec*(abs(test[i]-train[labels=='A']))))
	
	
	#Similarity to category B calc
	sim_catB=sum(exp(-spec*(abs(test[i]-train[labels=='B']))))
	
	#familiarity calc
	familiarity_test[i]=sim_catA+sim_catB
	
	#Probability of category A calc
	prA_test[i]=sim_catA^gamma/(sim_catA^gamma+sim_catB^gamma)
	
	
}

#return item wise familiarity and probability of A
return(list(fam=familiarity_test,prA=prA_test))

}


#Exemplar model parameters
#spec controls the width of generalization gradient 
spec=0.4
#gamma controls how deterministic/probabilistic participants are
gamma=1


#Generate training and test stimuli for each scenario
#The grand mean is always 0; category A is negative, B is positive
#The exemplars are always spaced evenly within categories and have the same category width
# The only difference is the discriminability between categories, how far they are from grand mean

#Distinctiveness 1
train_stim_l1=c(seq(-3,-1,length=10),seq(1,3,length=10))
#Because A & B are symmetric, we can just simulate everything for A
#order is ideal, local avg, anti-ideal, global average
test_stim_l1=c(-3,-2,-1,0)

#Distinctiveness 2
train_stim_l2=c(seq(-4,-2,length=10),seq(2,4,length=10))
test_stim_l2=c(-4,-3,-2,0)

#Distinctiveness 3
train_stim_l3=c(seq(-5,-3,length=10),seq(3,5,length=10))
test_stim_l3=c(-5,-4,-3,0)

#Distinctiveness 4
train_stim_l4=c(seq(-6,-4,length=10),seq(4,6,length=10))
test_stim_l4=c(-6,-5,-4,0)

train_labels=rep(c('A','B'),each=10)



#Run the exemplar model on each distinctiveness level
l1_e=exemplar_model(train_stim_l1,test_stim_l1,train_labels,spec,gamma)
l2_e=exemplar_model(train_stim_l2,test_stim_l2,train_labels,spec,gamma)
l3_e=exemplar_model(train_stim_l3,test_stim_l3,train_labels,spec,gamma)
l4_e=exemplar_model(train_stim_l4,test_stim_l4,train_labels,spec,gamma)


#Plot results for familiarity and prob correct (probability A for A items)
quartz()
plot(1:4,c(l1_e$prA[4],l2_e$prA[4],l3_e$prA[4],l4_e$prA[4]),type='l',col='red',ylab='PrClass',ylim=c(0.5,1))
lines(1:4,c(l1_e$prA[3],l2_e$prA[3],l3_e$prA[3],l4_e$prA[3]),type='l',col='blue')
lines(1:4,c(l1_e$prA[2],l2_e$prA[2],l3_e$prA[2],l4_e$prA[2]),type='l',col='green')
lines(1:4,c(l1_e$prA[1],l2_e$prA[1],l3_e$prA[1],l4_e$prA[1]),type='l',col='black')
legend(3,0.7,c('Global Avg','Anti-ideal','Local Avg','Ideal'),col=c('red','blue','green','black'),lty=c(1,1,1,1))

quartz()
plot(1:4,c(l1_e$fam[4],l2_e$fam[4],l3_e$fam[4],l4_e$fam[4]),type='l',col='red',ylab='Familiarity',ylim=c(0,10))
lines(1:4,c(l1_e$fam[3],l2_e$fam[3],l3_e$fam[3],l4_e$fam[3]),type='l',col='blue')
lines(1:4,c(l1_e$fam[2],l2_e$fam[2],l3_e$fam[2],l4_e$fam[2]),type='l',col='green')
lines(1:4,c(l1_e$fam[1],l2_e$fam[1],l3_e$fam[1],l4_e$fam[1]),type='l',col='black')

legend(2.5,2,c('Global Avg','Anti-ideal','Local Avg','Ideal'),col=c('red','blue','green','black'),lty=c(1,1,1,1))
