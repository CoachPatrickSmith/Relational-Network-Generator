library(network)
library(igraph)
library(dplyr)
library(intergraph)

#This does the same for the v0.2 script, no mutate required since the newer script does the parsing already
x%>%
  #filter(!Relation_Type=="ku")%>% #use this to supress "ku" relations. Comment this line to show "ku" relations
  #filter(Relation_Type=="ku")%>%#use this to supress all but "ku" relations. Comment this line to show "ku" relations
  #filter(!substr(Derivation_Level,1,4)=="Comb")%>% #use this to supress combinatorially derived relations. Comment this line to show combinatorially derived relations
  filter(!duplicated(Relation))->
  edgeList

#This coerces the data frame output above, in the form of an edge list, into network data type.
# 

relTnet<- network(edgeList,
                  matrix.type='edgelist',
                  ignore.eval=FALSE,
                  loops=TRUE,
                  #directed = FALSE ensure the edges do not overlap.  TRUE overlaps edges but provides arrows.
                  directed = FALSE,
                  #The line below should be multiple=FALSE.  
                  #That is currently erroring because the output from the relational permutations.R 
                  # file has multiple instances of the same two vertex with a 
                  # different relation (e.g. A<B & AkuB)
                  # This may be a product of using an incoherent(not previously vetted for consistency) start list.  
                  # That may need to be explored independently.
                  ##   Following up on above, this is not a logic error. 
                  ##    Multiple relations between vertex is reasonable given the pairs of relations used to combine.
                  multiple = TRUE)

rel_graph<- asIgraph(relTnet)

plot(rel_graph,
     vertex.size=12,
     vertex.label=V(rel_graph)$vertex.names,
     vertex.label.color = "white",
     vertex.label.size = 20,
     vertex.color = "black",
     edge.label=E(rel_graph)$Relation,
     edge.color=E(rel_graph)$EdgeColor,
     #edge.arrow.size=1,
     edge.weight=(1+E(rel_graph)$DerivationDegree)^-1,
     edge.width = ((1+E(rel_graph)$DerivationDegree)^-1)*5
)

#### For making a plot image of the table
# Only works for tables less than 30 rows.  
#   I don't know why it clips tops and bottoms otherwise.
# library(gridExtra)
# x.table<-tableGrob(x)
# grid.arrange(x.table)
####



#### Historical code for previous attempts
# #### This function pulls the end vertex from the string
# parseTo<-function(a){
#   if(nchar(a)>3){substr(a,4,4)}
#   else{substr(a,3,3)
#   }
# }
# 
# #This function pulls the relation type from the string
# parseRel<-function(a){
#   if(nchar(a)>3){substr(a,2,3)}
#   else{substr(a,2,2)
#   }
# }
# 
# #This function outputs a color string for the edge based on the derivation level
# #This scheme should be red/green colorblind accesssible
# edgeColor<- function(a){
#   case_when(
#     a == "Directly Trained" ~ "Blue",
#     a == "Mutually Entailed"~ "Brown",
#     a == "Combinatorially Entailed" ~ "Dark Green"
#   )
# }
# 
# #This takes the output from the relational permutation.R v0.1 script and 
# # parses start and end vertexes and relational types, and assigns edge color attributes.
# # It filters for duplicate instances of edges prior to assigning to a new data frame.
# # The output is a data frame formatted such that each row is an edge description and
# # can be coerced into the network data structure.
# 
# # relb%>%
# #   mutate(From = unlist(substr(Relation,1,1)), 
# #          To=unlist(lapply(Relation, parseTo)),
# #          Relation_Type = unlist(lapply(Relation, parseRel)),
# #          .before=Derivation_Level)%>%
# #   mutate(edge_color = unlist(lapply(Derivation_Level, edgeColor)))%>%
# #   filter(!Relation_Type=="ku")%>% #use this to supress "ku" relations.  Comment this line to show "ku" relations
# #   filter(!duplicated(Relation))->
# #   edgeList
# #####