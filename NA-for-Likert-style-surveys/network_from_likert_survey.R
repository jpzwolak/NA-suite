library(tidyverse)
library(dplyr)
library(igraph)

# likert_as_incidence_matrix: function
# inputs: 
#   df: dataframe of survey data with first column of respondent ID and 
#       subsequent columns of responses to each survey item. Responses must be
#       exactly or a subset of the following, "Strongly disagree", "Disagree",
#       "Slightly disagree", "Neither agree nor disagree", "Slightly agree",
#       "Agree", "Strongly agree".
#   mode: default is 'all', which creates column for each option independently. 
#         Can also be 'combine', which treats all variants of 'agree' the same and
#         all variants of 'disagree' the same
# returns:
#   incidence matrix with responses as the rows and respondents as the columns.
likert_as_incidence_matrix <- function(df, mode = 'all') {
  if(mode == 'all'){
    el <- df %>%
      mutate(ID = as.integer(ID)) %>%
      pivot_longer(!c(ID), names_to = "item", values_to = "response") %>% 
      mutate(item.resp = 
               case_when(response == "Strongly disagree" ~ paste0(item, 'StD'),
                         response == "Disagree" ~ paste0(item, 'D'),
                         response == "Slightly disagree" ~ paste0(item, 'SlD'),
                         response == "Neither agree nor disagree" ~ paste0(item, 'N'),
                         response == "Slightly agree" ~ paste0(item, 'SlA'),
                         response == "Agree" ~ paste0(item, 'A'),
                         response == "Strongly agree" ~ paste0(item, 'StA')),
             item.order = 
               case_when(response == "Strongly disagree" ~ paste0(item, '1'),
                         response == "Disagree" ~ paste0(item, '2'),
                         response == "Slightly disagree" ~ paste0(item, '3'),
                         response == "Neither agree nor disagree" ~ paste0(item, '4'),
                         response == "Slightly agree" ~ paste0(item, '5'),
                         response == "Agree" ~ paste0(item, '6'),
                         response == "Strongly agree" ~ paste0(item, '7')))
  }else if(mode == 'combine'){
    el <- df %>%
      mutate(ID = as.integer(ID)) %>%
      pivot_longer(!c(ID), names_to = "item", values_to = "response") %>% 
      mutate(item.resp = 
               case_when(response == "Strongly disagree" ~ paste0(item, 'D'),
                         response == "Disagree" ~ paste0(item, 'D'),
                         response == "Slightly disagree" ~ paste0(item, 'D'),
                         response == "Neither agree nor disagree" ~ paste0(item, 'N'),
                         response == "Slightly agree" ~ paste0(item, 'A'),
                         response == "Agree" ~ paste0(item, 'A'),
                         response == "Strongly agree" ~ paste0(item, 'A')),
             item.order = 
               case_when(response == "Strongly disagree" ~ paste0(item, '1'),
                         response == "Disagree" ~ paste0(item, '2'),
                         response == "Slightly disagree" ~ paste0(item, '3'),
                         response == "Neither agree nor disagree" ~ paste0(item, '4'),
                         response == "Slightly agree" ~ paste0(item, '5'),
                         response == "Agree" ~ paste0(item, '6'),
                         response == "Strongly agree" ~ paste0(item, '7')))
  }else{
    print('Unidentified mode, must be \'all\' or \'combine\'.')
    return()
  }
  
  df.inc <- el %>%
    filter(!is.na(response)) %>%
    select(c(ID, item.resp, item.order)) %>%
    arrange(item.order, ID) %>%
    select(c(ID, item.resp)) %>%
    mutate(val = 1) %>%
    pivot_wider(names_from = item.resp, values_from = val)
  
  mat.inc <- as.matrix(df.inc %>% select(-c(ID)))
  rownames(mat.inc) <- df.inc$ID
  
  mat.inc.items <- t(mat.inc)
  mat.inc.items[is.na(mat.inc.items)] <- 0
  
  return(mat.inc.items)
}

# check_incidence_matrix: function
# inputs:
#   inc.mat: incidence matrix with responses as the rows
#           and respondents as the columns.
#   a.count: integer value (1-3) that indicates the number of agree option variants
#   d.count: integer value (1-3) that indicates the number of disagree option variants
#   n.count: 1 or 0, indicating the existance of a neutral option
#   q.count: integer value that indicates the number of survey questions
#   q.names: vector that contains the shorthand of the selections
#   r.selections: vector that contains the names of the selections
# returns:
#   incidence matrix with responses as the rows and respondents as the columns.
#   this matrix is fixed if there was unselected responses.
check_incidence_matrix <- function(inc.mat, a.count, d.count, n.count, q.count,
                                   q.names, r.selections){
  while(nrow(inc.mat) != (a.count + n.count + d.count)*q.count){
    q <- 1
    r <- 1
    for(i in 1:nrow(inc.mat)){
      if(rownames(inc.mat)[i] != paste0(q.names[q],r.selections[r])){
        rnames <- rownames(inc.mat)
        rnames <- append(rnames, paste0(q.names[q],r.selections[r]), after=i-1)
        inc.mat <- rbind(inc.mat[1:i,], rep(0, ncol(inc.mat)), inc.mat[-(1:i),])
        rownames(inc.mat) <- rnames
        break
      }
      if(r < a.count + n.count + d.count){
        r <- r+1
      }else{
        r <- 1
        q <- q+1
      }
    }
    #check last row
    if(rownames(inc.mat)[nrow(inc.mat)] !=
       paste0(q.names[q.count],r.selections[a.count + n.count + d.count])){
      rnames <- rownames(inc.mat)
      rnames <- append(rnames,
                       paste0(q.names[q.count],r.selections[a.count + n.count + d.count]),
                       after=length(rnames))
      inc.mat <- rbind(inc.mat, rep(0, ncol(inc.mat)))
      rownames(inc.mat) <- rnames
    }
  }
  return(inc.mat)
}



# likert_as_resp_adj_mat: function
# inputs: 
#   inc.mat: incidence matrix with responses as the rows
#           and respondents as the columns.
# returns:
#   adjacency matrix of full response network.
# notes:
#   can obtain incidence matrix from likert_as_incidence_matrix function.
likert_as_resp_adj_mat <- function(inc.mat){
  A.mat <- inc.mat %*% t(inc.mat)
  diag(A.mat) <- 0
  
  return(A.mat)
}


# collapse_response_matrix: function
# inputs: 
#   adj.mat: adjacency matrix of the full response matrix
#   a.count: integer value (1-3) that indicates the number of agree option variants
#   d.count: integer value (1-3) that indicates the number of disagree option variants
#   n.count: 1 or 0, indicating the existence of a neutral option
#   q.count: integer value that indicates the number of survey questions
#   q.names: vector that contains the names of the questions (optional)
# returns:
#   vector object of two matrices, one similarity adjacency matrix of the item network
#    the second as the color adjacency matrix of the item network.
collapse_response_matrix <- function(adj.mat, a.count, d.count, n.count,
                                     q.count, q.names = NA){
  
  if(!(a.count %in% 1:3) | !(d.count %in% 1:3) | !(n.count %in% 0:1)){
    print("Enter the correct range for a.count (1-3), d.count (1-3), and n.count (0-1).")
    return()
  }
  if(!is.na(q.names) & length(q.names) != q.count){
    print("Length of question name vector must be the same as the q.count")
    return()
  }
  
  Q.mat <- matrix(0, q.count, q.count)
  if(!is.na(q.names)){
    colnames(Q.mat) <- q.names
    rownames(Q.mat) <- q.names
  }
  C.mat <- Q.mat
  
  opt.num <- a.count + d.count + n.count
  a.bounds <- (d.count+n.count+1):opt.num
  d.bounds <- 1:d.count
  
  for(i in 1:q.count){
    cols <- ((i-1)*opt.num+1):(i*opt.num)
    for(j in 1:q.count){
      rows <- ((j-1)*opt.num+1):(j*opt.num)
      Temp <- adj.mat[rows,cols]
      
      Q.mat[j,i] <- sum(Temp[a.bounds,a.bounds], Temp[d.bounds,d.bounds]) - sum(Temp[a.bounds,d.bounds], Temp[d.bounds,a.bounds])
      C.mat[j,i] <- sum(Temp[a.bounds,a.bounds]) - sum(Temp[d.bounds,d.bounds])
    }
  }
  
  return(list(Q.mat, C.mat))
}


# item_network_backbone: function
# inputs: 
#   q.mat: similarity adjacency matrix of the item network
#   c.mat: color adjacency matrix of the item network
#   alpha: level of significance (e.g. 0.001, 0.01, 0.05)
# returns:
#   vector object of two backbone matrices,
#    one similarity adjacency matrix of the item network
#    the second as the color adjacency matrix of the item network.
# notes:
#   relies on the LANS algorithm, other backbone processes could be used
item_network_backbone <- function(q.mat, c.mat, alpha){
  P_lans <- abs(q.mat) / colSums(abs(q.mat))
  A_lans <- P_lans
  A_lans[1:nrow(A_lans), 1:ncol(A_lans)] <- 0
  
  C_lans <- c.mat
  C_lans[1:nrow(C_lans), 1:ncol(C_lans)] <- 0
  
  for(i in 1:nrow(P_lans)){
    cdf.i <- P_lans[i,]
    p_a <- quantile(cdf.i, 1 - alpha)
    for(j in 1:nrow(P_lans)){
      if(P_lans[i,j] >= p_a){
        A_lans[i,j] <- q.mat[i,j]
        A_lans[j,i] <- q.mat[i,j]
        if(A_lans[i,j] > 0){
          C_lans[i,j] <- c.mat[i,j]
        }else{
          C_lans[i,j] <- 0
        }
        if(A_lans[j,i] > 0){
         C_lans[j,i] <- c.mat[i,j]
        }else{
          C_lans[j,i] <- 0
        }
      }
    }
  }
  
  return(list(A_lans, C_lans))
}


# generate_two_mode_weighted_network: function
# inputs: 
#   q.mat: backbone of similarity adjacency matrix of the item network
#   c.mat: backbone of color adjacency matrix of the item network
#   mode: either "edgelist" or "graph"
# returns:
#   (a) "edgelist": dataframe containing edgelist with weight and color
#   (b) "graph" igraph object with edge weight as similarity and edge color as agree/disagree ratio
# notes:
#   relies on the igraph package for R
generate_two_mode_weighted_network <- function(q.mat, c.mat, mode = "graph"){
  g <- graph_from_adjacency_matrix(q.mat, mode = "undirected", weighted = TRUE)
  gc <- graph_from_adjacency_matrix(c.mat, mode = "undirected", weighted = TRUE)
  g <- set_edge_attr(g, "color", E(g), E(gc)$weight)
  
  df <- get.data.frame(g) %>%
    mutate(color = E(g)$color)
  
  if(mode == "graph"){
    return(g)
  }else if(mode == "edgelist"){
    return(df)
  }else{
    print("Unidentified mode, must be \'graph\' or \'edgelist\'.")
    return()
  }
}


# create_survey_item_network: function
# inputs: 
#   df: dataframe of survey data with first column of respondent ID and 
#       subsequent columns of responses to each survey item. Responses must be
#       exactly or a subset of the following, "Strongly disagree", "Disagree",
#       "Slightly disagree", "Neither agree nor disagree", "Slightly agree",
#       "Agree", "Strongly agree".
#   a.count: integer value (1-3) that indicates the number of agree option variants
#   d.count: integer value (1-3) that indicates the number of disagree option variants
#   n.count: 1 or 0, indicating the existance of a neutral option
#   q.count: integer value that indicates the number of survey questions
#   q.names: vector that contains the names of the questions
#   alpha: level of significance (e.g. 0.001, 0.01, 0.05)
#   output: either "edgelist" or "graph"
#
# returns:
#   (a) "edgelist": dataframe containing edgelist with weight and color
#   (b) "graph" igraph object with edge weight as similarity and edge color as agree/disagree ratio
create_survey_item_network <- function(df, a.count, d.count, n.count,
                                       q.count, q.names, r.selections,
                                       alpha = 0.05, output = "graph"){
  inc.mat <- likert_as_incidence_matrix(df)
  inc.mat <- check_incidence_matrix(inc.mat, a.count, d.count, n.count,
                                    q.count, q.names, r.selections)
  resp.adj.mat <- likert_as_resp_adj_mat(inc.mat)
  item.mats <- collapse_response_matrix(resp.adj.mat, a.count, d.count,
                                        n.count, q.count, q.names)
  item.bbs <- item_network_backbone(item.mats[[1]], item.mats[[2]], alpha)
  net <- generate_two_mode_weighted_network(item.bbs[[1]], item.bbs[[2]], output)
  
  return(net)
}
