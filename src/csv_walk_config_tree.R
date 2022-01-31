# File:     walk_csv_config_tree.R

# see github.com/jayventi/csv_config_tree for full description
# walk_csv_config_tree() walks a csv configuration file formatted as a text tree, and 
# converts it to a R list data structure.
# raw_csv_config takes one parameter raw_csv_config a dataframe of a csv file, 
# outputs a list representation of the csv config file.

# this script is maintained in its own repository for most recent version copy 
# from github.com/jayventi/csv_config_tree  is a standalone script

# constance ## ################################################################
comt_chr = '#' # comment character
key_sufx = ':' #  key name suffix character

# cvs config helpers ## main funtion > walk_csv_config_tree ###################

# csv string handling and parsing functions
is_comment_str =  function(x, com_chr='#') {
  x_trim = str_trim(x, "left")
  result = (str_sub(x_trim,1,1) == com_chr)
  return(result)
}

is_na_or_whitespace = function(x) {
  result = TRUE
  if(! is.na(x)){
    result = grepl("^\\s*$", x)
  }
  return(result)
}

is_key = function(x) {return(grepl("\\:$", x))}

get_key_nam = function(x, com_chr='\\:') {
  x_trim = str_trim(x, "left")
  com_chr_inx = gregexpr( x_trim, pattern = com_chr)
  return(str_sub(x_trim,1, com_chr_inx[[1]]-1))
}

is_v_comment_or_blank =  function(v, control='BOTH',com_chr='#') {
  first_non_empty_comm = FALSE
  first_non_blank_sene = FALSE
  for( element in 1:length(v)){
    if (! first_non_empty_comm & ! first_non_blank_sene){
      if (!is_na_or_whitespace(v[element])) { 
        first_non_blank_sene = TRUE
        if(is_comment_str(v[element], com_chr)){first_non_empty_comm = TRUE}
      }
    }
  } 
  if (control=='COMMENT'){result=first_non_empty_comm
  } else if (control=='BLANK'){result != first_non_blank_sene
  } else {result = (first_non_empty_comm | ! first_non_blank_sene)}
  return(result)
}

get_key_index = function(v) {
  key_inx = 0 
  if (! is.na(v[[1]])){
    for(inx in 1:length(v) ){
      if (key_inx == 0){
        if (is_key(v[inx])){
          key_inx = inx
        }
      }
    }
  }
  return(key_inx) 
}

get_list_from_row = function(row, key_inx) {
  result = list()
  key_nam = ''
  v_out = vector()
  key_nam = get_key_nam(row[key_inx])
  for(inx in (key_inx+1):length(row) ){
    if (! is_na_or_whitespace(row[inx])){
      if (!is.na(suppressWarnings(as.numeric(row[inx])))){
        v_out[inx-key_inx] = as.integer(row[inx])
      } else {
        v_out[inx-key_inx] = (row[inx])
      } 
    }
  }
  result[[key_nam]] = v_out
  return(result)
}

delete_comment_or_blank = function(csv_like_list) {
  result = list()
  delete_lines = c()
  del_inx = 0
  for (line_n in 1:nrow(csv_like_list)){
    raw_line = raw_csv_config[line_n,]
    if (is_v_comment_or_blank(raw_line)){
      del_inx = del_inx + 1
      delete_lines[del_inx] = line_n
    }
  }
  result = csv_like_list[-delete_lines,]
  row.names(result) = NULL
  return(result)
}

#  helper functions for walking the configuration tree 

set_n_leaf = function(n, working_str, leaf, line_list) {
  eval_str = paste('n',working_str,'[["',leaf,'"]] = unlist(line_list[[1]])',sep = "")
  eval(parse(text=eval_str))
  return(n)
}

set_n_parent = function(n, working_str,parent){
  eval_str = paste('n',working_str,'[["parent"]] = parent',sep = "")
  eval(parse(text=eval_str))
  return(n)  
}

get_n_parent = function(n, working_str){
  eval_str = paste('parent = n',working_str,'[["parent"]]',sep = "")
  eval(parse(text=eval_str))
  return(parent)  
}

# main walking function, walks the CSV file and builds a configuration list with the same 
#  hierarchical arrangement as the CSV following the CSV layout rules 
walk_csv_config_tree = function(raw_csv_config) {
  n = list()
  m = n # for parent
  working = ''
  key_inx = 1
  core_csv_list = delete_comment_or_blank(raw_csv_config)
  for (line_n in 1:nrow(core_csv_list)){
    raw_line = core_csv_list[line_n,]
    last_key_inx = key_inx # track last, current
    key_inx = get_key_index(raw_line)
    line_list = get_list_from_row(raw_line, key_inx)
    key_nam = get_key_nam(raw_line[key_inx])
    if (key_inx < last_key_inx ){ # go towards root
      working = get_n_parent(m, working)
      if (length(line_list[[1]]) == 0) { # is a inner nod
        working = paste(working,'[["',key_nam,'"]]',sep = "") # setup new working
      }
    } else if (length(line_list[[1]]) == 0){ # go away from root
      parent = working
      working = paste(working,'[["',key_nam,'"]]',sep = "") # setup new working
      m = set_n_parent(n, working, parent) # set parent = working
    }
    if (length(line_list[[1]]) > 0) { # is it a leaf node then process it
      n = set_n_leaf(n, working, key_nam, line_list) }
  }
  return(n)
}

# little testbed
# config = walk_csv_config_tree(raw_csv_config)