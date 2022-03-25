library(tidyverse)


#Test datasets
a <- c("A<B", "B>C")
b <- c("A<B", "B>C", "C<D", "E>D", "F>E", "F<G")
c <- c("A<B")
d <- c("A<B", "B<C", "C<D", "D<E", "E<F", "F<G")
e <- c("A<B", "B<C")
f <- c("A<B", "B>C", "C=D")
g<- c("c<a","b<a","e<d","f<d","d<a")
t<- c("gkuj", "h<i", "i<k", "k<j", "l<o", "mkuk", "n=o", "p>o")
u<-c("A<B", "B>C", "C<D", "E>D", "F>E", "F<G","g=j", "h<i", "i<k", "k<j", "l<o", "m=k", "n=o", "p>o", "D=m","i=o")
#The a,b,c, and t variables above are examples of lists of relational statements.
#You can provide as many statements as you like in the form of Single_letter_Relation(=,<,>, or ku)_Single_letter (e.g. A>B)
#Once your list is saved to a variable name, running the v0.1 code at the bottom's relationTrain(variablename) 
# will output a table of relations and their general derivation group.
# Running the v0.2 code just below will output an edge list table ready to be imported into a (network) or (iGraph) data structure.
#   Running the Network Graph visualizer.R script will produce a visual graph plot of the trained and derived network.

#### Version 1.0
#relationParse input 1 value (relStatement)
##  and output 8 values in 1 row
##      (From(Stim1), Relation, To(Stim2), Relation, Relation Type, Edge Color, Derivation Level, Derivation Degree, Derived From{defaul none})

relationParse <- function(relStatement){
  a<- as.character(relStatement)
  b<- substr(a,1,1)
  r<- case_when(
    nchar(a)==3 ~ substr(a,2,2),
    nchar(a)==4 ~ substr(a,2,3)
  )
  d<-  case_when(
    nchar(a)==3 ~ substr(a,3,3),
    nchar(a)==4 ~ substr(a,4,4)
  )
  data.frame(From=b,
             To=d,
             RelationType=r,
             Relation=a,
             DerivationLevel="Directly Trained",
             DerivationDegree = 0,
             EdgeColor="Blue",
             DerivedFrom = "NA"
  )
}

#mutualEntail input 4 values (From(Stim 1), Relation, To(Stim2), Derivation Degree{default = 0}) 
##   and output 8 values 1 rows
##      (From(Stim2), Entailed Relation, To(Stim1), Entailed Relation, Relation Type, Edge Color, Derivation Level, Derivation Degree, Derived From)
mutualEntail <- function(from, relation, to, derivationDegree=0){
  r<- case_when(
    as.character(relation) == "="~relation,
    as.character(relation) == "<"~">",
    as.character(relation) == ">"~"<",
    as.character(relation) == "ku"~relation,
    TRUE ~ "notDerivable"
  )
  
  data.frame(From=to,
             To=from,
             RelationType=r,
             Relation=paste(to, r, from, sep = ""),
             DerivationLevel="Mutually Entailed",
             DerivationDegree = derivationDegree+1,
             EdgeColor="Deep Red",
             DerivedFrom = paste(from, relation, to, sep = "")
  )
}

#combinatorialEntail input 8 values (From(Stim 1), Relation, To(Stim2), Derivation Degree1{default = 0}, From(Stim 3), Relation2, To(Stim4), Derivation Degree2{default = 0})
##  and output 8 values 2 rows
##      (From(StimA), Relation, To(StimB), Relation, Relation Type, Edge Color, Derivation Level, Derivation Degree, Derived From)
##      Mutual Entail Output: (From(StimB), Entailed Relation, To(StimA), Entailed Relation, Relation Type, Edge Color, Derivation Level, Derivation Degree, Derived From)
combinatorialEntail <- function(From1, Relation1, To1, derivationDegree1=0, From2, Relation2, To2, derivationDegree2=0){
  #filter out identity relations, Fail if either is identity
  #filter out pairs that do not intersect, Fail if no shared stimuli
  if(From1 %in% To1){return()} #Test for identity in first pair
  if(From2 %in% To2) {return()} #Test for identity in second pair
  if(From1 %in% c(From2, To2) & To1 %in% c(From2, To2)){return()} #Test for two of the same pair
  if(!(From1 %in% c(From2, To2)) & !(To1 %in% c(From2, To2))){return()} #test for non-overlapping pairs
  d<- max(derivationDegree1, derivationDegree2)
  e<- From1
  r1<- Relation1
  g<- To1
  h<- From2
  r2<- Relation2
  j<- To2
  r3<-NA #declaring a placeholder variable before assigning a value
  r3<- if_else("ku" %in% c(r1,r2),
               "ku",
               if_else(e==h,
                       if_else(r1==r2,
                               if_else(r1=="=",
                                       r1,
                                       "ku"),
                               if_else(r1=="=",
                                       r2,
                                       if_else(r2=="=",
                                               if_else(r1=="<",
                                                       ">",
                                                       "<")
                                               ,r2))),
                       if_else(e==j,
                               if_else(r1==r2,
                                       if_else(r1=="=",
                                               r1,
                                               if_else(r1=="<",
                                                       ">",
                                                       "<")),
                                       if_else(r1=="=",
                                               if_else(r2=="<",
                                                       ">",
                                                       "<"),
                                               if_else(r2=="=",
                                                       if_else(r1=="<",
                                                               ">",
                                                               "<"),
                                                       "ku"))),
                               if_else(g==j,
                                       if_else(r1==r2, 
                                               if_else(r1=="=", 
                                                       r1,
                                                       "ku"),
                                               if_else(r1=="=", 
                                                       if_else(r2=="<",
                                                               ">",
                                                               "<"),
                                                       r1)),
                                       if_else(r1==r2,
                                               r1,
                                               if_else(r1=="=",
                                                       r2,
                                                       if_else(r2=="=",
                                                               r1,
                                                               "ku")))))))
  if(is.na(r3)){return()}
  l<- if_else(e%in%c(h,j),g,e)
  m<- if_else(h%in%c(e,g),j,h)
  
  w<- data.frame(From=l,
                 To=m,
                 RelationType=r3,
                 Relation=paste(l, r3, m, sep = ""),
                 DerivationLevel="Combinatorially Entailed",
                 DerivationDegree = max(2,d+1), 
                 # The above line ensures that combinatorially entailed relations will be at least derivation degree 2. 
                 ## Otherwise they would be DD1 for combining two directly trained relations.
                 ## That arbitrary distinction has interesting implications on if a mutually entailed relation has the same 
                 ## probability of response as a combinatorially entailed response from the same directly trained relations.
                 ## That assumes that derivation degree is a metaphorical measure of behavioral probability. (Higher degree = Lower Probability)
                 EdgeColor="Deep Orange",
                 DerivedFrom = paste(e, r1, g,",",h,r2,j, sep = "")
  )
  x<- mutualEntail(from = w$From, relation = w$RelationType, to= w$To, derivationDegree = w$DerivationDegree-1)
  # Related to the above note regarding derivation degree, this d-11 assignment ensures that mutually combinatorially entailed relations
  ## are assigned the same derivation degree value as the combinatorially entailed relation they are derived from.
  rbind(w,x)
  
}


#relationTrain input 3 values (list(relStatements) | Relational data frame, maxDerivationDegree{default = 2 (i.e, first combinatorial mutual entailment)})
##  and output dataframe of all derivable relational statements up to selected max derivation degree
relationTrain <- function(relationList, maxDerivationDegree = 2){
  #detect if relation list is a table or character string list
  #IF string list, split string and feed through relParse
  #IF not string list, assign data frame to formatted table and fill in default values
  rL<- when(relationList,
            is.null(dim(relationList)) ~ bind_rows(lapply(relationList, relationParse)),
            !is.null(dim(relationList)) ~ relationList)
  mDD <- maxDerivationDegree
  #Feed rows of table to Mutual Entail
  tRl <- mutualEntail( from = rL$From, relation = rL$RelationType, to = rL$To, derivationDegree = rL$DerivationDegree)
  rL<- bind_rows(rL, tRl)
  #Generate list of permutations of stimuli pairs and their derivation level
  cL<- filter(distinct(merge(rL$From, rL$To)), x!=y)
  names(cL)<- c("From", "To")
  #Trim list to desired derivation level (not required if the list is only one instance of each pairing)
  ## by using a pre-planned list, this (1) assumes everything may be connected
  ## and (2) prevents generating combinatorially entailed relations where a relation has already been established.
  ## Number (1) will require a check of some sort to see if nrows of cL has minimized if it isn't 0.
  ## Number (2) will simplify other user's experience but may be revisisted in later versions.
  #Trim permutations to those not already in the main table
  cL<-setdiff(cL, rL[,c(1,2)])
  if(nrow(cL)==0){return(rL)}
  cL<- mutate(cL,
              stimPair = case_when(
                cL$From > cL$To ~paste(cL$To, cL$From, sep = ""),
                TRUE ~ paste(cL$From, cL$To, sep = "")
              )
  )
  cL<- cL[!duplicated(cL$stimPair),]
  
  #The below IF function handles where only two relations are trained and mapply is not needed during combinatorial entailment
  if(nrow(cL)==1){
    tRl<-filter(rL,DerivationLevel=="Directly Trained"|DerivationLevel=="Combinatorially Entailed")[,c(1:3,6)]
    tmpX<- tRl
    #rename the new table to prevent overlap in the next step
    names(tmpX)<- c("From2", "To2","RelationType2","DerivationDegree2")
    #merge the two temp tables to produce all combinations or rows
    tRl<- merge(tRl,tmpX)
    #add a stimulus set identifier to each relation set
    tRl<- mutate(tRl,
                 stimSet1 = case_when(
                   tRl$From > tRl$To ~ paste(tRl$To, tRl$From, sep = ""),
                   TRUE ~ paste(tRl$From, tRl$To, sep = "")
                 ),
                 stimSet2 =  case_when(
                   tRl$From2 > tRl$To2 ~ paste(tRl$To2, tRl$From2, sep = ""),
                   TRUE ~ paste(tRl$From2, tRl$To2, sep = "")
                 )
    )
    #filter the new merged table to exclude combinations of the same relations
    tRl <- filter(tRl, stimSet1 != stimSet2)
    #combine individual stim set identifier to make a combinatorial stimulus set identifier
    tRl<- mutate(tRl,
                 stimSet = case_when(
                   tRl$stimSet1 > tRl$stimSet2 ~ paste(tRl$stimSet2, tRl$stimSet1, sep = ""),
                   TRUE ~ paste(tRl$stimSet1, tRl$stimSet2, sep = "")
                 )
    )
    #Filter for duplicate combinatorial stimulus sets
    tRl<- tRl[!duplicated(tRl$stimSet),]
    #create a list of combinatorial pair row numbers that share one stimuli (3 unique stimulus in the stim set)
    #tmpX<- lapply((lapply(str_split(tRl$stimSet,  pattern = ""), unique)), length)
    #use the above list to filter the rows of TRl
    #tRl<- tRl[which(sapply(tmpX, FUN=function(X) 3 %in% X)),] #This and the above line are breaking the combinatorial entail block below
    #Feed tRl to combinatorial entail function and row bind output to rL table
    tRl<- combinatorialEntail(
      From1 = tRl$From,
      From2=tRl$From2,
      To1 = tRl$To, 
      To2 = tRl$To2,
      Relation1 = tRl$RelationType, 
      Relation2 = tRl$RelationType2, 
      derivationDegree1 = tRl$DerivationDegree, 
      derivationDegree2 = tRl$DerivationDegree2)
    rL<- bind_rows(rL, tRl)
    rL<- filter(rL, !duplicated(rL$Relation))
    rL<- mutate(rL,
                stimPair = case_when(
                  rL$From > rL$To ~paste(rL$To, rL$From, sep = ""),
                  TRUE ~ paste(rL$From, rL$To, sep = "")
                )
    )
    return(rL)}
  
  
  i<-1
  while (i>0) {
    pre.loop<- nrow(cL)
    #create a temp table of relations with only direct or combinatorially entailed relations
    tRl<-filter(rL,DerivationLevel=="Directly Trained"|DerivationLevel=="Combinatorially Entailed")[,c(1:3,6)]
    #duplicate that table in a temporary holder
    tmpX<- tRl
    #rename the new table to prevent overlap in the next step
    names(tmpX)<- c("From2", "To2","RelationType2","DerivationDegree2")
    #merge the two temp tables to produce all combinations or rows
    tRl<- merge(tRl,tmpX)
    #add a stimulus set identifier to each relation set
    tRl<- mutate(tRl,
                 stimSet1 = case_when(
                   tRl$From > tRl$To ~ paste(tRl$To, tRl$From, sep = ""),
                   TRUE ~ paste(tRl$From, tRl$To, sep = "")
                 ),
                 stimSet2 =  case_when(
                   tRl$From2 > tRl$To2 ~ paste(tRl$To2, tRl$From2, sep = ""),
                   TRUE ~ paste(tRl$From2, tRl$To2, sep = "")
                 )
    )
    #filter the new merged table to exclude combinations of the same relations
    tRl <- filter(tRl, stimSet1 != stimSet2)
    #combine individual stim set identifier to make a combinatorial stimulus set identifier
    tRl<- mutate(tRl,
                 stimSet = case_when(
                   tRl$stimSet1 > tRl$stimSet2 ~ paste(tRl$stimSet2, tRl$stimSet1, sep = ""),
                   TRUE ~ paste(tRl$stimSet1, tRl$stimSet2, sep = "")
                 )
    )
    #Filter for duplicate combinatorial stimulus sets
    tRl<- tRl[!duplicated(tRl$stimSet),]
    #create a list of combinatorial pair row numbers that share one stimuli (3 unique stimulus in the stim set)
    #tmpX<- lapply((lapply(str_split(tRl$stimSet,  pattern = ""), unique)), length)
    #use the above list to filter the rows of TRl
    #tRl<- tRl[which(sapply(tmpX, FUN=function(X) 3 %in% X)),] #This and the above line are breaking the combinatorial entail block below
    #Feed tRl to combinatorial entail function and row bind output to rL table
    
    
    tRl<- bind_rows(mapply(combinatorialEntail,
                           From1 = tRl$From,
                           From2=tRl$From2,
                           To1 = tRl$To, 
                           To2 = tRl$To2,
                           Relation1 = tRl$RelationType, 
                           Relation2 = tRl$RelationType2, 
                           derivationDegree1 = tRl$DerivationDegree, 
                           derivationDegree2 = tRl$DerivationDegree2)
    )
    
    
    rL<- bind_rows(rL, tRl)
    rL<- filter(rL, !duplicated(rL$Relation))
    rL<- mutate(rL,
                stimPair = case_when(
                  rL$From > rL$To ~paste(rL$To, rL$From, sep = ""),
                  TRUE ~ paste(rL$From, rL$To, sep = "")
                )
    )
    
    #update comparison list
    cL<-setdiff(cL, rL[,c(1,2,9)])
    post.loop<- nrow(cL)
    i<- if_else(post.loop>0, pre.loop-post.loop, post.loop)
  }
  rL<- filter(rL, rL$DerivationDegree<=maxDerivationDegree)
  return(rL)
  
  #Select relational pairs that will combine to fill in blanks
  #Feed this list to combEntail
  #Update permutation list
  #Repeat select and combEnt until permutation list is complete
  #Return completed datatable
}



x<-relTrain(b) #use the variable name "x" to pass to the visualizer without issue


# 
# #### Version 0.2
# library(tidyr)
# library(combinat)
# library(dplyr)
# mutEnt<- function(a){
#   b<- substr(a,1,1)
#   r<- case_when(
#     nchar(a)==3 ~ substr(a,2,2),
#     nchar(a)==4 ~ substr(a,2,3)
#     )
#   d<-  case_when(
#     nchar(a)==3 ~ substr(a,3,3),
#     nchar(a)==4 ~ substr(a,4,4)
#     )
#   s<- case_when(
#     r=="="~r,
#     r=="<"~">",
#     r==">"~"<",
#     r=="ku"~r
#   )
#   data.frame(From=c(b,d),
#              To=c(d,b),
#              Relation_Type=c(r,s),
#              Derivation_Level=c("Directly Trained", "Mutually Entailed"),
#              Relation=c(paste(b,r,d, sep = ""), paste(d,s,b, sep = "")),
#              edge_color=c("Blue", "Dark Red")
#              )
# }
# combEnt<- function (a){
#   b<-a[1]
#   d<-a[2]
#   e<-substr(b,1,1)
#   r1<- case_when(
#     nchar(b)==3 ~ substr(b,2,2),
#     nchar(b)==4 ~ substr(b,2,3)
#   )
#   g<- case_when(
#     nchar(b)==3 ~ substr(b,3,3),
#     nchar(b)==4 ~ substr(b,4,4)
#   )
#   if(e==g){return()} #Testing first relational statement for identity
#   h<-substr(d,1,1)
#   r2<- case_when(
#     nchar(d)==3 ~ substr(d,2,2),
#     nchar(d)==4 ~ substr(d,2,3)
#   )
#   j<- case_when(
#     nchar(d)==3 ~ substr(d,3,3),
#     nchar(d)==4 ~ substr(d,4,4)
#   )
#   if(r1=="ku"){return()}
#   if(r2=="ku"){return()}
#   if(h==j){return()} #Testing second relational statement for identity
#   if(h%in%c(e,g) & j%in%c(e,g)){return()} #Testing between relational statements for two of the same
#   if(!(h%in%c(e,g)) & !(j%in%c(e,g))){return()} #Testing between relational statements for no shared stimuli (This may be a source for writing to a separate list that is recombined later)
#   r3<-NA 
#   r3<-if_else(e==h,
#               if_else(r1==r2,if_else(r1=="=",r1, "ku"),
#                       if_else(r1=="=", r2, if_else(r2=="=",if_else(r1=="<",">","<"),r2))),
#               if_else(e==j,
#                       if_else(r1==r2,if_else(r1=="=",r1,if_else(r1=="<",">","<")),
#                               if_else(r1=="=",if_else(r2=="<",">","<"),
#                                       if_else(r2=="=",if_else(r1=="<",">","<"),"ku"))),
#                       if_else(g==j,
#                               if_else(r1==r2, if_else(r1=="=", r1,"ku"),
#                                       if_else(r1=="=", if_else(r2=="<",">","<"),
#                                               r1 )),
#                               if_else(r1==r2,r1,if_else(r1=="=",r2,if_else(r2=="=",r1,"ku"))))))
#   if(is.na(r3)){return()}
#   l<- if_else(e%in%c(h,j),g,e)
#   m<- if_else(h%in%c(e,g),j,h)
#   r4<- case_when(
#     r3=="="~r3,
#     r3=="<"~">",
#     r3==">"~"<",
#     r3=="ku"~r3
#   )
#   data.frame(From=c(l,m),
#              To=c(m,l),
#              Relation_Type=c(r3,r4),
#              Derivation_Level=c("Combinatorially Entailed", "Combinatorially Mutually Entailed"),
#              Relation=c(paste(l,r3,m, sep = ""), paste(m,r4,l, sep = "")),
#              edge_color=c("Dark Green", "Dark Green"),
#              Derived_from=c(paste(a[1],a[2], sep = ","),paste(a[1],a[2], sep = ","))
#   )
#   
# } #this currently may not handle KU+nonKU relations correctly. Proceed with caution.
# relTrain<- function(a){
#   if(length(a)<2){return(mutEnt(a))} #Handling of single relational statements as a special case.
#   u<-Reduce(function(...) merge(...,all=T),lapply(a, mutEnt))
#   v<-combn(filter(u,Derivation_Level=="Directly Trained")[,5],m=2)
#   w<-if(length(v)<=2){
#     combEnt(as.list(v))
#   }else{
#     Reduce(function(...) merge(..., all=T),Filter(Negate(is.null), apply(v,2,combEnt)))
#   }
#   x<-full_join(u,w)
#   
#   ##The following rows should be repeated until diff.join is equal to zero.
#   i<-1
#   while (i>0) {
#     y<-combn(filter(x,Derivation_Level=="Directly Trained"|Derivation_Level=="Combinatorially Entailed")[,5],m=2)
#     w<-Reduce(function(...) merge(..., all=T),Filter(Negate(is.null), apply(y,2,combEnt)))
#     pre.join<-nrow(x)
#     x<-full_join(x,w)
#     x%>%
#       filter(!duplicated(Relation))->
#       x
#     diff.join<-nrow(x)-pre.join
#     i<-diff.join}
#   return(x)
# } #This now does all of the below in v0.2 automatically.
# 
# x<-relTrain(b) #use the variable name "x" to pass to the visualizer without issue

# ###V0.2 code detailed commenting of the relTrain function internals above.############
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


##### Version 0.1 below ############
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