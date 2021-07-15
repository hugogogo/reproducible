# this file is modified version of
# btree package available in
# https://github.com/ben519/btree

library(data.table)
library(ggplot2)

# construct a binary tree from a list of nodeIds and their parentIds
make_binary_tree <- function(nodeIds, parentIds){
  # Initialize the btree
  btree <- data.table(NodeId = nodeIds, ParentId = parentIds)
  
  # Add left child and right child
  btree <- btree[, list(NodeId = ParentId, LeftChildId = NodeId)][btree, on = "NodeId", mult = "first"]
  btree <- btree[, list(NodeId = ParentId, RightChildId = NodeId)][btree, on = "NodeId", mult = "last"]
  
  # Add depth field
  btree$NodePath <- btree_paths(btree)
  btree$Depth <- nchar(btree$NodePath)
  btree$Value <- rep(integer(0), nrow(btree))
  
  return(btree)
}

# construct a perfect binary tree of sepecified depth
make_perfect_binary_tree <- function(depth){
  # Make a perfect btree of total of 2^(depth + 1) - 1 nodes
  
  n_nodes <- 2^(depth + 1) - 1
  nodeIds <- seq_len(n_nodes)
  parentIds <- c(NA, head(rep(seq_len(n_nodes), each = 2), 
                          length(nodeIds) - 1))
  btree <- make_binary_tree(nodeIds = nodeIds,
                                 parentIds = parentIds)
  
  return(btree)
}

# before calling compute_nonleaf_node_value
# should assign values to all leaf nodes
# e.g.,
# leaf_nodes <- btree[btree$Depth == max(btree$Depth), ]$NodeId
# btree[btree$NodeId %in% leaf_nodes, ]$Value <- sample(c(0, 1), size = length(leaf_nodes), replace = TRUE)
compute_nonleaf_node_value <- function(btree){
  # assume that each leaf node has been assigned a value
  depth <- max(btree$Depth)
  if (depth == 0){
    warning("this tree has depth 0!")
    return(btree)
  }
  for (d in seq(depth - 1, 0)){
    node_on_this_level <- btree[btree$Depth == d, ]$NodeId
    for (id in node_on_this_level){
      # the value of each non-leaf node is the union of all nodes in its subtree. 
      # in bernoulli case, its just the max of all children values
      btree[NodeId == id, ]$Value <- max(get_subtree(btree, id)$Value)
    }
  }
  return(btree)
}

btree_paths <- function(btree, nodeid = FALSE){
  # Returns a vector of paths, "L" representing traversal to the left and "R" representing traversal to the right
  
  # Get the root node
  parents <- btree[is.na(ParentId)]
  parents[, Path := ""]
  
  parentsList <- list(parents)
  while(nrow(parents) > 0){
    # Get the children
    leftChildren <- btree[parents[, list(Path, LeftChildId)], on=c("NodeId"="LeftChildId"), nomatch=0]
    rightChildren <- btree[parents[, list(Path, RightChildId)], on=c("NodeId"="RightChildId"), nomatch=0]
    
    # Mark the paths
    leftChildren[, Path := paste0(Path, "L")]
    rightChildren[, Path := paste0(Path, "R")]
    
    # make the new parents
    parents <- rbind(leftChildren, rightChildren, use.names=TRUE)
    
    # Insert new parents into parentsList
    parentsList <- c(parentsList, list(parents))
  }
  
  # rbind parentsList into btree
  newbtree <- rbindlist(parentsList)
  
  # Sort the rows of newbtree to match the given btree
  newbtree <- newbtree[btree[, list(NodeId)], on="NodeId"]
  
  return(newbtree$Path)
}

get_subtree <- function(btree, nodeId){
  childNodes <- btree[NodeId == nodeId]
  childNodesList <- list(childNodes)
  
  while(nrow(childNodes) > 0){
    childNodes <- btree[childNodes[, list(NodeId)], on=c("ParentId" = "NodeId"), nomatch=0]
    childNodesList <- c(childNodesList, list(childNodes))
  }
  
  subtree <- rbindlist(childNodesList)
  subtree[NodeId == nodeId, ParentId := NA]
  
  # Prepend class with "btree"
  class(subtree) <- class(btree)
  
  return(subtree[])
}

plot_btree <- function(btree, nodeid = FALSE){
  # Plot a btree
  
  # Copy the given btree and add columns for Depth, NodePath
  btreeCopy <- btree[, list(NodeId, ParentId, LeftChildId, RightChildId)]
  btreeCopy[, NodePath := btree_paths(btreeCopy)]
  btreeCopy[, Depth := nchar(NodePath)]
  
  # Order the rows properly
  setorder(btreeCopy, "Depth", "NodePath")
  
  # Determine the width of each subtree @ each node
  subtree_width <- function(btree, nodeId){
    subtree <- get_subtree(btree, nodeId)
    width <- pmax(1, max(nchar(btree_paths(subtree))))
    return(width)
  }
  btreeCopy[, SubTreeWidth := subtree_width(btreeCopy, NodeId), by=NodeId]
  
  # Get the max depth
  btree.depth <- max(btreeCopy$Depth)
  
  # Insert X coordinates
  btreeCopy[Depth == 0, `:=`(Xmin = 0, Xmax = 1, X = 0.5)]
  for(depth in seq(1, btree.depth)){
    
    # widths: 1, 6  -> 1/7
    # X: 0.5, 1+3
    
    # Get the nodes at the current depth
    current_level_nodes <- btreeCopy[Depth == depth]
    
    # For each node, get its parent's Xmin, Xmax
    parent_level_nodes <- btreeCopy[Depth == depth - 1]
    current_level_nodes[parent_level_nodes, `:=`(ParentXmin = i.Xmin, ParentXmax = i.Xmax), on=c("ParentId"="NodeId")]
    
    # Determine Xmin, X, Xmax
    current_level_nodes[, BlockWidth := (ParentXmax - ParentXmin)/sum(SubTreeWidth), by=ParentId]
    current_level_nodes[, BlocksOver := cumsum(SubTreeWidth) - SubTreeWidth + SubTreeWidth/2, by=ParentId]
    current_level_nodes[, X := ParentXmin + BlockWidth * BlocksOver]
    current_level_nodes[, `:=`(
      Xmin = ParentXmin + (cumsum(SubTreeWidth) - SubTreeWidth) * BlockWidth,
      Xmax = ParentXmin + cumsum(SubTreeWidth) * BlockWidth
    ), by=ParentId]
    
    # Insert values into btreeCopy
    btreeCopy[current_level_nodes, `:=`(Xmin = i.Xmin, Xmax = i.Xmax, X = i.X), on="NodeId"]
  }
  
  # Insert Y coordinates
  btreeCopy[, Y := 1 - Depth/btree.depth]
  btreeCopy[Y < 1, Y := Y + (1/btree.depth)* 0.5 * (seq_len(.N) %% 3)/2, by=Depth]
  
  # Get the edges
  btree.edges <- btreeCopy[btreeCopy, on=c("NodeId"="ParentId"), nomatch=0]
  btree.edges <- btree.edges[, list(NodeId1=NodeId, NodeId2=i.NodeId, X1=X, Y1=Y, X2=i.X, Y2=i.Y)]
  
  # different values have different colors
  btreeCopy$Value <- factor(btree$Value)
  
  if(nodeid){
    ggplot(data=btreeCopy, aes(x=X, y=Y)) +
      geom_point(aes(colour = factor(Value)), size = 10) +
      geom_text(aes(label = NodeId)) +
      labs(colour="Probability") +
      geom_segment(data=btree.edges, aes(x=X1, y=Y1, xend=X2, yend=Y2), linetype="dotted", alpha=.25)+
      xlim(0, 1)+ylim(0, 1)+theme_void()
  }
  else{
    ggplot(data=btreeCopy, aes(x=X, y=Y)) +
      geom_point(aes(colour = factor(Value)), size = 10) +
      labs(colour="Probability") +
      geom_segment(data=btree.edges, aes(x=X1, y=Y1, xend=X2, yend=Y2), linetype="dotted", alpha=.25)+
      xlim(0, 1)+ylim(0, 1)+theme_void()
  }
}
