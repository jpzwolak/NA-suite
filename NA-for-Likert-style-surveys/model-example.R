library(tidyverse)
library(dplyr)
library(igraph)

# Set the working directory
setwd(getwd())
source("network_from_likert_survey.R")

# Load in the model file.
# Note, this is human-generated data, and not a random sample test case.
data <- read.csv("model-data.csv")
question.names <- colnames(data[,-(1:1)])

# All functions can be ran through this following function:
quick.net <- create_survey_item_network(df = data,
                                        a.count = 2,
                                        d.count = 2,
                                        n.count = 1,
                                        q.count = 8,
                                        q.names = question.names,
                                        r.selections = c("StD","D","N","A","StA"),
                                        alpha = 0.05)
print(quick.net)
print(as_adjacency_matrix(quick.net, attr='weight'))
  
  
  
# THE LONG WAY...
# Create an incidence matrix of student respondents and response selections
inc.mat <- likert_as_incidence_matrix(data)
# print(inc.mat)

inc.mat <- check_incidence_matrix(inc.mat,
                                   a.count = 2,
                                   d.count = 2,
                                   n.count = 1,
                                   q.count = 8,
                                   q.names = question.names,
                                   r.selections = c("StD","D","N","A","StA"))



# Use the incidence matrix to create an adjacency matrix of response selections
resp.adj.mat <- likert_as_resp_adj_mat(inc.mat)
# print(resp.adj.mat)

# Use the response selection adjacency matrix to collapse it to just a matrix
# of survey items. To call this function, the number of "agree" types and
# "disagree" types needs to be known (as well as whether or not there is a
# "neither" option). Additionally, the number of survey items should be given.
item.mats <- collapse_response_matrix(resp.adj.mat,
                                      a.count = 2, # agree, strongly agree
                                      d.count = 2, # disagree, strongly disagree
                                      n.count = 1,
                                      q.count = 8,
                                      q.names = question.names)
# print(item.mats[[1]])
# print(items.mats[[2]])

# Use both the selection [[1]], and the temperature [[2]] to create the
# survey item backbone.
item.bb <- item_network_backbone(q.mat = item.mats[[1]],
                                 c.mat = item.mats[[2]],
                                 alpha = 0.05) # 0.05 for small network
# print(item.bb[[1]])
# print(item.bb[[2]])

# Now from the backbone network, build the full two-mode network of 
# similarity and temperature.
net <- generate_two_mode_weighted_network(item.bb[[1]], item.bb[[2]])
print(net)
print(as_adjacency_matrix(net, attr='weight'))