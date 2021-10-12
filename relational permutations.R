library(tidyr)
library(combinat)
library(dplyr)
# directTrain<- function(x){
#   as.vector(unique(x))
# }
# 
# d<- c("1","2","3","4","2","3")
# 
# length(directTrain(d))

mutualEntail <- function(a){
  if("k" %in% substr(a,2,2)){return(paste(substr(a,4,4),"ku",substr(a,1,1), sep = ""))}
  x<- substr(a,1,1)
  y<- substr(a,3,3)
  z<- substr(a,2,2)
  w<- if_else(z=="=",
              "=",
              if_else(z== "<",
                      ">",
                      "<")
  )
  paste(y,w,x, sep = "")
}

combEntail <- function(a){
  e<-  if_else(substr(a[1],2,2) != "k",substr(a[1],1,1),substr(a[1],1,1))
  r1<- if_else(substr(a[1],2,2) != "k",substr(a[1],2,2),substr(a[1],2,3))
  f<- if_else(substr(a[1],2,2) != "k",substr(a[1],3,3),substr(a[1],4,4))
  if(e==f) {return()}
  g<- if_else(substr(a[2],2,2) != "k",substr(a[2],1,1),substr(a[2],1,1))
  r2<- if_else(substr(a[2],2,2) != "k",substr(a[2],2,2),substr(a[2],2,3))
  h<- if_else(substr(a[2],2,2) != "k",substr(a[2],3,3),substr(a[2],4,4))
  if(g==h) {return()}
  if(g%in%c(e,f) & h%in%c(e,f)){return()}
  r3<-NA
  if(TRUE%in%(c(g,h)%in%c(e,f))){
    r3<-  if_else(f==h,
                  if_else(r1==r2,if_else(r1=="=",r1,"ku"),if_else(r1=="=", if_else(r2==">","<",">"),r1)),
                  if_else(r1==r2,r1,if_else(r1=="=",r2,if_else(r2=="=",r1,"ku"))))
  }
  if(is.na(r3)){return()}
  if_else(f==g|f==h, paste(e,r3,h, sep = ""), paste(g,r3,f, sep = ""))
  }


relationTrain <- function(a){
  
  b<- as.character(lapply(a,mutualEntail))
  trainedRelations <- mutate(data.frame(a), Derivation_Level="Directly Trained",.before = a)
  names(trainedRelations)[names(trainedRelations)=="a"]<-"Relation" 
  mutuallyDerivedRelations <- mutate(pivot_longer(data.frame(lapply(a,mutualEntail)), cols = starts_with("X"), names_to = "Derivation_Level", values_to = "Relation"), Derivation_Level="Mutually Entailed")
  if(length(b)==1){return(rbind(trainedRelations, mutuallyDerivedRelations))}
  c<- as.character(if(length(a)<=2){combEntail((combn(a,m=2)))} else(apply(combn(a, m=2),2,combEntail)[!sapply(apply(combn(a, m=2),2,combEntail),is.null)]))
  c<- c[!is.na(c)]
  q<- unique(apply(combn(c(c,a),m=2),2,combEntail))
  q<- as.character(q[2:length(q)])
  q<- q[!is.na(q)]
  d<- as.character(if(length(unique(c(c,q)))<=1){mutualEntail(c)} else(lapply(c(c,q),mutualEntail)))
  
  combinatorialDerivedRelations <- data.frame(unique(c(c,q,d)))
  names(combinatorialDerivedRelations)[names(combinatorialDerivedRelations)=="unique.c.c..q..d.."]<-"Relation"
  combinatorialDerivedRelations<- mutate(combinatorialDerivedRelations,Derivation_Level="Combinatorially Entailed", .before="Relation")
  #knownUnknownRelations <-
  rbind(trainedRelations,rbind(combinatorialDerivedRelations, mutuallyDerivedRelations))
}





a <- c("A<B",
       "B>C"#,
       # "C<D",
       # "E>D",
       # "F>E",
       # "F<G"
       )

rel<-relationTrain(a)


b <- c("A<B",
       "B>C",
       "C<D",
       "E>D",
       "F>E",
       "F<G"
)

relb<-relationTrain(b)

c <- c("A<B"#,
       #"B>C",
       # "C<D",
       # "E>D",
       # "F>E",
       # "F<G"
)

relc<-relationTrain(c)

# a[2]
# as.list(a[2])
# 
# combinatorialDerivedRelations <- data.frame(unique(c(c,q,d)))
# names(combinatorialDerivedRelations)[names(combinatorialDerivedRelations)=="unique.c.c..q..d.."]<-"Relation"
# combinatorialDerivedRelations<- mutate(combinatorialDerivedRelations,Derivation_Level="Combinatorially Entailed", .before="Relation")
# 
# 
# q<- unique(apply(combn(c(c,a),m=2),2,combEntail))
# as.character(q[2:length(q)])
# 
