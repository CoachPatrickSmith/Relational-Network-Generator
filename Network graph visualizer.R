library(network)
library(igraph)
library(dplyr)
library(intergraph)

#The below script assumes you have assigned the output of the relationTrain() function in "relational permutations.R" 
# to the variable "x" already. Once that is done, simply running the unmodified script below should produce the basic 
# graph network visualization with stimuli as vertex and relationships as edges color coded as trained (Blue), mutually entailed (Purple),
# and combinatorially entailed (Orange). Edge colors may be modified by changing the color names in line in "relational permutations.R"


#The commented out lines just below allow you to filter "ku" or "non-ku" relations from the input table prior to generating
#the graph visualization. See line specific comments for more details.
x%>%
  #filter(!RelationType=="ku")%>% #use this to supress "ku" relations. Comment this line to show "ku" relations
  #filter(RelationType=="ku")%>%#use this to supress all but "ku" relations. Comment this line to show "ku" relations
  #filter(!substr(DerivationLevel,1,4)=="Comb")%>% #use this to supress combinatorially derived relations. Comment this line to show combinatorially derived relations
  filter(!duplicated(Relation))->
  edgeList

#This coerces the data frame output above, in the form of an edge list, 
# into a network data type for use with graph visualization packages.

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

#This line below coerces the generic network data type into an Igraph object for plotting
# If you are familiar with other graph packages, you
rel_graph<- asIgraph(relTnet)


#This line generates the actual plot of the graph network. 
# Some versions of Igraph may not show the vertex or edge labels in the plot window.
#  See https://github.com/igraph/python-igraph/issues/185 for details.
#  If this is the case for you, try exporting as an .SVG file and open that to check
#   for the labels plotting successfully.
plot(rel_graph,
     #vertex.size=12,
     vertex.label=V(rel_graph)$vertex.names,
     vertex.label.color = "white",
     #vertex.label.size = 20,
     vertex.color = "black",
     edge.label=E(rel_graph)$Relation,
     edge.color=E(rel_graph)$EdgeColor#,
     #edge.arrow.size=1,
     #edge.weight=(1+E(rel_graph)$DerivationDegree)^-1,
     #edge.width = ((1+E(rel_graph)$DerivationDegree)^-1)*5
)

#### For making a plot image of the table
# Only works for tables less than 30 rows.  
#   I don't know why it clips tops and bottoms otherwise.
# library(gridExtra)
# x.table<-tableGrob(x)
# grid.arrange(x.table)
####
