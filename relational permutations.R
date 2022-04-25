library(tidyverse)


#Test datasets
a <- c("A<B", "B>C")
b <- c("A<B", "B>C", "C<D", "E>D", "F>E", "F<G")
c <- c("A<B")
d <- c("A<B", "B<C", "C<D", "D<E", "E<F", "F<G")
e <- c("A<B", "B<C")
f <- c("A<B", "B>C", "C=D")
g<- c("c<a","b<a","e<d","f<d","d<a")
h<- c("A=B", "B=C")
i<- c("A>B", "B=C")
t<- c("gkuj", "h<i", "i<k", "k<j", "l<o", "mkuk", "n=o", "p>o")
u<-c("A<B", "B>C", "C<D", "E>D", "F>E", "F<G","g=j", "h<i", "i<k", "k<j", "l<o", "m=k", "n=o", "p>o", "D=m","i=o")
#The variables above are examples of lists of relational statements.
#Variables h, i, & t, are the examples 1-3 respectively given in the Smith & Hayes (2022) manuscript introducing this script.
## to work through the examples from the paper,
## assign one of the lists to a variable name,
## load the custom functions below into the environment,
## and run relationTrain(VariableName) to get output to the console.
## 
###  Additional functionality since publication is described in comments in the code here.
###   Derivation Degree has been added to allow for control of how deeply derivation recurs on previously derived relations.
###   Child relational Derivation Degree values are inherited from the parent relations used in their derivation and increment up using the following methods:
###    Trained Relations: DD =0
###    Mutually Entailed Relations: DD= ParentDD+1
###    Combinatorially Entailed Relations: DD = max((MaxParentDD +1) OR 2)
###   The default parent DD values for the Mutual and Combinatorial Entailment functions are both 0.
###   The default max derivation degree value in RelationTrain() is 2.
###    This value can be changed if the user wants to restrict the output or explore topological differences associated with constrained or extended derivation.
#
#If you would like to input your own relational list,
## you can provide as many statements as you like in the form of Single_letter_Relation(=,<,>, or ku)_Single_letter (e.g. A>B)
## Once your list is saved to a variable name, running the v0.1 code at the bottom's relationTrain(variablename) 
## will output a table of relations and their general derivation group.
# Running the v1.0 code just below will output an edge list table ready to be imported into a (network) or (iGraph) data structure.
#   Running the Network Graph visualizer.R script will produce a visual graph plot of the trained and derived network.

## Version 1.0
#relationParse input 1 value (relStatement)
##  and output 8 values in 1 row
##      (From(Stim1), Relation, To(Stim2), Relation, Relation Type, Edge Color, Derivation Level, Derivation Degree, Derived From{defaul none})
# This function handles converting the character string input of a single relational statement into a list of elements 
# and appends additional metadata for user readability, graph network visualizing, and diagnostics.

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
# This function handles deriving mutually entailed relations from a single input relation.

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
             EdgeColor="Purple",
             DerivedFrom = paste(from, relation, to, sep = "")
  )
}

#combinatorialEntail input 8 values (From(Stim 1), Relation, To(Stim2), Derivation Degree1{default = 0}, From(Stim 3), Relation2, To(Stim4), Derivation Degree2{default = 0})
##  and output 8 values 2 rows
##      (From(StimA), Relation, To(StimB), Relation, Relation Type, Edge Color, Derivation Level, Derivation Degree, Derived From)
##      Mutual Entail Output: (From(StimB), Entailed Relation, To(StimA), Entailed Relation, Relation Type, Edge Color, Derivation Level, Derivation Degree, Derived From)
# This function handles deriving combinatorially entailed relations from two input relations.
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
                 EdgeColor="Orange",
                 DerivedFrom = paste(e, r1, g,",",h,r2,j, sep = "")
  )
  x<- mutualEntail(from = w$From, relation = w$RelationType, to= w$To, derivationDegree = w$DerivationDegree-1)
  # Related to the above note regarding derivation degree, this d-1 assignment ensures that mutually combinatorially entailed relations
  ## are assigned the same derivation degree value as the combinatorially entailed relation they are derived from.
  x$DerivationLevel <- "Combinatorially Mutually Entailed"
  x$EdgeColor <- "Orange"
  rbind(w,x)
  
}


#relationTrain input 3 values (list(relStatements) | Relational data frame, maxDerivationDegree{default = 2 (i.e, first combinatorial mutual entailment)})
##  and output dataframe of all derivable relational statements up to selected max derivation degree
# This function brings all of the above functions together under one master function and handles efficiently permuting possible stimulus pairs to the derivation degree specified.

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
  #Return completed datatable
  return(rL)
}



x<-relationTrain(i, maxDerivationDegree = 5) #use the variable name "x" to pass to the visualizer without issue


### Code testing

