library(tidyr)
library(combinat)
library(dplyr)

#Test datasets
a <- c("A<B", "B>C")
b <- c("A<B", "B>C", "C<D", "E>D", "F>E", "F<G")
c <- c("A<B")
d <- c("A<B", "B<C", "C<D", "D<E", "E<F", "F<G")
e <- c("A<B", "B<C")
t<- c("gkuj", "h<i", "i<k", "k<j", "l<o", "mkuk", "n=o", "p>o")
u<-c("A<B", "B>C", "C<D", "E>D", "F>E", "F<G","g=j", "h<i", "i<k", "k<j", "l<o", "m=k", "n=o", "p>o", "D=m","i=o")
#The a,b,c, and t variables above are examples of lists of relational statements.
#You can provide as many statements as you like in the form of Single_letter_Relation(=,<,>, or ku)_Single_letter (e.g. A>B)
#Once your list is saved to a variable name, running the v0.1 code at the bottom's relationTrain(variablename) 
# will output a table of relations and their general derivation group.
# Running the v0.2 code just below will output an edge list table ready to be imported into a (network) or (iGraph) data structure.
#   Running the Network Graph visualizer.R script will produce a visual graph plot of the trained and derived network.

## Version 0.2
mutEnt<- function(a){
  b<- substr(a,1,1)
  r<- case_when(
    nchar(a)==3 ~ substr(a,2,2),
    nchar(a)==4 ~ substr(a,2,3)
    )
  d<-  case_when(
    nchar(a)==3 ~ substr(a,3,3),
    nchar(a)==4 ~ substr(a,4,4)
    )
  s<- case_when(
    r=="="~r,
    r=="<"~">",
    r==">"~"<",
    r=="ku"~r
  )
  data.frame(From=c(b,d),
             To=c(d,b),
             Relation_Type=c(r,s),
             Derivation_Level=c("Directly Trained", "Mutually Entailed"),
             Relation=c(paste(b,r,d, sep = ""), paste(d,s,b, sep = "")),
             edge_color=c("Blue", "Dark Red")
             )
}
combEnt<- function (a){
  b<-a[1]
  d<-a[2]
  e<-substr(b,1,1)
  r1<- case_when(
    nchar(b)==3 ~ substr(b,2,2),
    nchar(b)==4 ~ substr(b,2,3)
  )
  g<- case_when(
    nchar(b)==3 ~ substr(b,3,3),
    nchar(b)==4 ~ substr(b,4,4)
  )
  if(e==g){return()} #Testing first relational statement for identity
  h<-substr(d,1,1)
  r2<- case_when(
    nchar(d)==3 ~ substr(d,2,2),
    nchar(d)==4 ~ substr(d,2,3)
  )
  j<- case_when(
    nchar(d)==3 ~ substr(d,3,3),
    nchar(d)==4 ~ substr(d,4,4)
  )
  if(r1=="ku"){return()}
  if(r2=="ku"){return()}
  if(h==j){return()} #Testing second relational statement for identity
  if(h%in%c(e,g) & j%in%c(e,g)){return()} #Testing between relational statements for two of the same
  if(!(h%in%c(e,g)) & !(j%in%c(e,g))){return()} #Testing between relational statements for no shared stimuli (This may be a source for writing to a separate list that is recombined later)
  r3<-NA 
  r3<-if_else(e==h,
              if_else(r1==r2,if_else(r1=="=",r1, "ku"),
                      if_else(r1=="=", r2, if_else(r2=="=",if_else(r1=="<",">","<"),r2))),
              if_else(e==j,
                      if_else(r1==r2,if_else(r1=="=",r1,if_else(r1=="<",">","<")),
                              if_else(r1=="=",if_else(r2=="<",">","<"),
                                      if_else(r2=="=",if_else(r1=="<",">","<"),"ku"))),
                      if_else(g==j,
                              if_else(r1==r2, if_else(r1=="=", r1,"ku"),
                                      if_else(r1=="=", if_else(r2=="<",">","<"),
                                              r1 )),
                              if_else(r1==r2,r1,if_else(r1=="=",r2,if_else(r2=="=",r1,"ku"))))))
  if(is.na(r3)){return()}
  l<- if_else(e%in%c(h,j),g,e)
  m<- if_else(h%in%c(e,g),j,h)
  r4<- case_when(
    r3=="="~r3,
    r3=="<"~">",
    r3==">"~"<",
    r3=="ku"~r3
  )
  data.frame(From=c(l,m),
             To=c(m,l),
             Relation_Type=c(r3,r4),
             Derivation_Level=c("Combinatorially Entailed", "Combinatorially Mutually Entailed"),
             Relation=c(paste(l,r3,m, sep = ""), paste(m,r4,l, sep = "")),
             edge_color=c("Dark Green", "Dark Green"),
             Derived_from=c(paste(a[1],a[2], sep = ","),paste(a[1],a[2], sep = ","))
  )
  
} #this currently may not handle KU+nonKU relations correctly. Proceed with caution.
relTrain<- function(a){
  if(length(a)<2){return(mutEnt(a))} #Handling of single relational statements as a special case.
  u<-Reduce(function(...) merge(...,all=T),lapply(a, mutEnt))
  v<-combn(filter(u,Derivation_Level=="Directly Trained")[,5],m=2)
  w<-if(length(v)<=2){
    combEnt(as.list(v))
  }else{
    Reduce(function(...) merge(..., all=T),Filter(Negate(is.null), apply(v,2,combEnt)))
  }
  x<-full_join(u,w)
  
  ##The following rows should be repeated until diff.join is equal to zero.
  i<-1
  while (i>0) {
    y<-combn(filter(x,Derivation_Level=="Directly Trained"|Derivation_Level=="Combinatorially Entailed")[,5],m=2)
    w<-Reduce(function(...) merge(..., all=T),Filter(Negate(is.null), apply(y,2,combEnt)))
    pre.join<-nrow(x)
    x<-full_join(x,w)
    x%>%
      filter(!duplicated(Relation))->
      x
    diff.join<-nrow(x)-pre.join
    i<-diff.join}
  return(x)
} #This now does all of the below in v0.2 automatically.

x<-relTrain(u) #use the variable name "x" to pass to the visualizer without issue

# ############ All of the below until the V0.1 code is just the detailed commenting of the relTrain function internals above.############
# ##The first instances of u,v,w, & x below should only be run once.
# #This generates the mutually entailed relations from the trained set and translates the strings into data tables.
# #   Change the input_data_set_variable in lapply(<input_data_set_variable>, mutEnt) to try different lists
# u<-Reduce(function(...) merge(...,all=T),lapply(b, mutEnt))
# #This pulls the directly trained relations from the above data tables 
# # and creates a list of pairs of all the possible combinations of those directly trained.
# v<-combn(filter(u,Derivation_Level=="Directly Trained")[,5],m=2)
# #This takes those pairs and returns all the combinatorially derived, and mutually combinatorially derived relations.
# w<-if(length(v)<=2){combEnt(as.list(v))}else{Reduce(function(...) merge(..., all=T),Filter(Negate(is.null), apply(v,2,combEnt)))}
# #This merges rows not previously occuring in the original data tables from the combinatorial derivation table.
# x<-full_join(u,w)
# 
# 
# ##The following rows should be repeated until diff.join is equal to zero.
# ## That will ensure that the possible combinations of the list have been fully derived through
# #This pulls the unique relations that were directly trained of combinatorially derived
# # and generates all possible pair combinations.
# y<-combn(filter(x,Derivation_Level=="Directly Trained"|Derivation_Level=="Combinatorially Entailed")[,5],m=2)
# #This takes those pairs and returns all the combinatorially derived, and mutually combinatorially derived relations.
# w<-Reduce(function(...) merge(..., all=T),Filter(Negate(is.null), apply(y,2,combEnt)))
# #This stores the pre-merged number of rows in the master table to check against if new unique rows were added.
# pre.join<-nrow(x)
# #This merges rows not previously occuring in the original data tables from the new combinatorial derivation table.
# x<-full_join(x,w)
# #This filters out all duplicate instances of relations that may be combinatorially and mutually combinatorially derived
# # a second or more times.
# ## Those second and more incidences may be worth counting toward a differential edge weight in the future.  
# ##  They might provide insight into how networks morph as new information is incorporated or old information is reviewed.
# x%>%
#   filter(!duplicated(Relation))->
#   x
# #This compares the current number of rows in the master table against it's pre-joined and filtered state.
# diff.join<-nrow(x)-pre.join
# #Iterate from the y assignment line above through here until diff.join is equal to zero


# ############ Version 0.1 below ############
#In V0.1,longer lists(>3 elements) do not iterate through all possible combinatorially derived relations.

# mutualEntail <- function(a){
#   if("k" %in% substr(a,2,2)){return(paste(substr(a,4,4),"ku",substr(a,1,1), sep = ""))}
#   x<- substr(a,1,1)
#   y<- substr(a,3,3)
#   z<- substr(a,2,2)
#   w<- if_else(z=="=",
#               "=",
#               if_else(z== "<",
#                       ">",
#                       "<")
#   )
#   paste(y,w,x, sep = "")
# }
# 
# combEntail <- function(a){
#   e<-  if_else(substr(a[1],2,2) != "k",substr(a[1],1,1),substr(a[1],1,1))
#   r1<- if_else(substr(a[1],2,2) != "k",substr(a[1],2,2),substr(a[1],2,3))
#   f<- if_else(substr(a[1],2,2) != "k",substr(a[1],3,3),substr(a[1],4,4))
#   if(e==f) {return()}
#   g<- if_else(substr(a[2],2,2) != "k",substr(a[2],1,1),substr(a[2],1,1))
#   r2<- if_else(substr(a[2],2,2) != "k",substr(a[2],2,2),substr(a[2],2,3))
#   h<- if_else(substr(a[2],2,2) != "k",substr(a[2],3,3),substr(a[2],4,4))
#   if(g==h) {return()}
#   if(g%in%c(e,f) & h%in%c(e,f)){return()}
#   r3<-NA
#   if(TRUE%in%(c(g,h)%in%c(e,f))){
#     r3<-  if_else(e==g,
#                   if_else(r1==r2,if_else(r1=="=",r1, "ku"),
#                           if_else(r1=="=", r2, if_else(r2=="=",if_else(r1=="<",">","<"),r2))),
#                   if_else(e==h,
#                           if_else(r1==r2,if_else(r1=="=",r1,if_else(r1=="<",">","<")),
#                                   if_else(r1=="=",if_else(r2=="<",">","<"),
#                                           if_else(r2=="=",if_else(r1=="<",">","<"),"ku"))),
#                           if_else(f==g,
#                                   if_else(r1==r2, if_else(r1=="=", r1,"ku"),
#                                           if_else(r1=="=", if_else(r2=="<",">","<"),
#                                                   r1 )),
#                                   if_else(r1==r2,r1,if_else(r1=="=",r2,if_else(r2=="=",r1,"ku"))))))
#   }
#   if(is.na(r3)){return()}
#   if_else(f==g|f==h, paste(e,r3,h, sep = ""), paste(g,r3,f, sep = ""))
#   }
# 
# relationTrain <- function(a){
#   
#   b<- as.character(lapply(a,mutualEntail))
#   trainedRelations <- mutate(data.frame(a), Derivation_Level="Directly Trained",.before = a)
#   names(trainedRelations)[names(trainedRelations)=="a"]<-"Relation" 
#   mutuallyDerivedRelations <- mutate(pivot_longer(data.frame(lapply(a,mutualEntail)), cols = starts_with("X"), names_to = "Derivation_Level", values_to = "Relation"), Derivation_Level="Mutually Entailed")
#   if(length(b)==1){return(rbind(trainedRelations, mutuallyDerivedRelations))}
#   c<- as.character(if(length(a)<=2){combEntail((combn(a,m=2)))} else(apply(combn(a, m=2),2,combEntail)[!sapply(apply(combn(a, m=2),2,combEntail),is.null)]))
#   c<- c[!is.na(c)]
#   q<- unique(apply(combn(c(c,a),m=2),2,combEntail))
#   q<- as.character(q[2:length(q)])
#   q<- q[!is.na(q)]
#   d<- as.character(if(length(unique(c(c,q)))<=1){mutualEntail(c)} else(lapply(c(c,q),mutualEntail)))
#   
#   combinatorialDerivedRelations <- data.frame(unique(c(c,q,d)))
#   names(combinatorialDerivedRelations)[names(combinatorialDerivedRelations)=="unique.c.c..q..d.."]<-"Relation"
#   combinatorialDerivedRelations<- mutate(combinatorialDerivedRelations,Derivation_Level="Combinatorially Entailed", .before="Relation")
#   #knownUnknownRelations <-
#   rbind(trainedRelations,rbind(combinatorialDerivedRelations, mutuallyDerivedRelations))
# }
# 
# 
# ##Various demonstrations of v0.1 with the test datasets above
# rel<-relationTrain(a)
# relb<-relationTrain(b)
# relc<-relationTrain(c)
# relt<-relationTrain(t)
# 
# #The a,b,c, and t variables below are examples of lists of relational statements.
#   #You can provide as many statements as you like in the form of Single_letter_Relation(=,<,>, or ku)_Single_letter
#   #Once your list is saved to a variable name, running relationTrain(variablename) will output a table of relations
#   # and their general derivation group.
#   #Currently,longer lists(>3 elements) do not iterate through all possible combinatorially derived relations yet.
#     #need to write some sort of recursion that adds new relations to q(or c), reruns combEntail,
#     #and tests the output against the prior state of the list until there are no new statements being generated.
# 
# #Long term, it would be nice if this would take statements as an array i.e. [stimuli_name, relation, stimuli_name]
#   #and thus could handle longer names.
#   #additionally, adding ability to handle more relational types (i.e. difference, opposition, heirarchy, etc) will add
#     #functionality to this script.
# 
# 
# #Please leave the below comments.
#   #this is remnant from testing out the ability to produce a report about the characteristics of the output table
# 
# # directTrain<- function(x){
# #   as.vector(unique(x))
# # }
# # 
# # d<- c("1","2","3","4","2","3")
# # 
# # length(directTrain(d))