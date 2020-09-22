#*****************************************************************************************#
# Clustering Algorithm for hotspots data                                                  #
#                                                                                         #
# Default:                                                                                #
#                                                                                         #
#   hotspots_clustering(con, method = "null", adj_distance = 3e3, active_time = 24,       #
#                       fire_mov_dir = "", memory_saving = FALSE, save_point = 1000)      #
#                                                                                         #
# Arguments:                                                                              #
#                                                                                         #
#   con:           a dbConnect() (SQLite) object.                                         #
#   table_name:    a table name of the hotspots data. It should contains 4 and only 4     #
#                  columns, which are id, lon, lat and time_id                            #                                     
#   method:        clustering method. Either "null", "mean_r", or 'max_r'.                #
#   adj_distance:  distance (0 to 1e5 m) for two vertices to be considered as adjacent.   #                                                                               
#   active_time:   units of time a clusters remain active.                                #
#   fire_mov_dir:  a database dir to store fire movement result. Database will be         #
#                  overwritten.                                                           #
#   memory_saving: (TRUE or FALSE) occasionally save data to database to release memory.  #
#   save_point:    only useful when memory_saving is TRUE. save data per 'save_point'     #
#                  number of steps.                                                       #
#                                                                                         #
# Value:                                                                                  #
#                                                                                         #
#   A clusteringResult class inherit from list with a customize print method. It contains # 
#   clstuering result and other attributes used in this algorithm.                        #
#*****************************************************************************************#

hotspots_clustering <- function(con,
                                table_name = "",
                                method = "null",
                                adj_distance = 3e3,
                                active_time = 24,
                                fire_mov_dir = "",
                                memory_saving = FALSE,
                                save_point = 200){
  
  # Overwrite fire movement database
  if (memory_saving) {
    if (file.exists(fire_mov_dir)) file.remove(fire_mov_dir)
    table_count = 0
  }
  
  if (table_name == "") stop("Argument table_name is missing")
  
  if (!DBI::dbExistsTable(con, table_name)){
    stop(paste0(table_name, "does not exist"))
  }
  
  if (any(DBI::dbListFields(con, table_name) != c("id", "lon", "lat", "time_id"))){
    stop("fields name do not match with 'id, lon, lat, time_id'")
  }
  
  if (!method %in% c("null", "mean_r", "max_r")){
    stop(paste0("invaild method ", method))
  }
  
  # Get the max timestamp
  max_time <- tbl(con, sql(paste0("SELECT MAX(time_id) FROM ", table_name))) %>%
    collect() %>%
    .[[1]]
  
  if (!((is.atomic(max_time)) & (length(max_time == 1)))) stop("max_time contains more than 1 value")
  
  # Get number of rows
  max_obs <- tbl(con, sql(paste0("SELECT COUNT(time_id) FROM ", table_name))) %>%
    collect() %>%
    .[[1]]
  
  if (!((is.atomic(max_obs)) & (length(max_obs == 1)))) stop("max_obs contains more than 1 value")
  
  # Set up progress bar
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) eta: :eta", 
                                   total = max_time)
  pb$tick(0)
  pb$tick(1)
  
  # Initialize hotspots memberships
  fire_id <- c()
  id <- c()
  
  # Algorithm start from the first timestamp
  # Select the first timestamp data
  current_time_data <- tbl(con, table_name) %>% 
    filter(time_id == 1) %>%
    collect()
  
  if (nrow(current_time_data) == 0) stop("There is no data in the table")
  
  # Compute the distance matrix
  dist_matrix <- current_time_data %>%
    select(lon, lat) %>%
    geodist::geodist()
  
  if (!is.matrix(dist_matrix)) stop("distance matrix is not a matrix")
  
  # Create adjacency matrix
  adj_matrix <- dist_matrix <= adj_distance
  
  # Create graph from adjacency matrix
  current_graph <- graph.adjacency(adj_matrix, mode = 'undirected')
  
  # Compute clusters
  current_clusters = clusters(current_graph)
  
  # Assign membership to each point
  current_time_data$fire_id <- current_clusters$membership
  
  # Assign the fire_id to a vector
  fire_id <- c(fire_id, current_time_data$fire_id)
  id <- c(id, current_time_data$id)
  fire_move_ls <- vector(mode = "list", length = max_time)
  
  # Compute the centroid of each group
  fire_mov <- current_time_data %>%
    group_by(fire_id) %>%
    summarise(r = calc_r(lon, lat),
              lon = mean(lon), 
              lat = mean(lat),
              mean_r = as.numeric(stringr::str_split(r, ",")[[1]][1]),
              max_r = as.numeric(stringr::str_split(r, ",")[[1]][2])) %>%
    ungroup() %>%
    mutate(active = 0) %>%
    mutate(time_id = 1) %>%
    arrange(fire_id) %>%
    select(-r)
  
  
  # Store fire movement in list
  fire_move_ls[[1]] <- fire_mov %>%
    select(-active)
  
  
  for (i in 2:max_time){
    
    pb$tick(1)
    
    # Let the `active` minus 1
    fire_mov$active <- fire_mov$active - 1
    
    # Get the current timestamp hotspots data
    current_time_data <- tbl(con, table_name) %>% 
      filter(time_id == i) %>%
      collect()
    
    if (nrow(current_time_data) == 0) next
    
    # Get the active fire center
    active_group <- filter(fire_mov, active > -active_time)
    
    # Get row number
    total_current_obs <- nrow(current_time_data)
    total_active_group <- nrow(active_group)
    total_current_points <- total_current_obs + total_active_group
    
    # Append the groups geometry to the hotspots geometry
    if (total_active_group > 0){
      geom_info <- current_time_data %>%
        select(lon, lat) %>%
        bind_rows(select(active_group, lon, lat))
    } else {
      geom_info <- current_time_data %>%
        select(lon, lat)
    }
    
    # Compute the dist matrix
    dist_matrix <- geodist::geodist(geom_info)
    
    if (!is.matrix(dist_matrix)) stop("distance matrix is not a matrix")
    
    # Create adjacency matrix
    adj_matrix <- dist_matrix <= adj_distance
    
    
    # Apply algorithm using nominated method
    if ((total_active_group > 0) & (method %in% c("mean_r", "max_r"))){
      addition_metric <- active_group[[method]]
      
      # Extract distance matrix between hotspots and active groups
      partialA <- dist_matrix[1:total_current_obs,
                              (total_current_obs + 1):total_current_points,
                              drop = FALSE]
      
      # Extract radius from active groups
      partialB <- matrix(rep(addition_metric, total_current_obs),
                         nrow = total_current_obs,
                         byrow = TRUE)
      
      if (all(dim(partialA) != dim(partialB))) stop("unmatched matrices")
      
      # Decide the adjacency partial matrix
      partialC <- (partialA <= partialB) + (partialA <= adj_distance)
      partialC <- partialC > 0
      
      # Reassign the adjacency matrix
      adj_matrix[1:total_current_obs,
                 (total_current_obs + 1):total_current_points] <- partialC
      
    }
    
    # Create graph from adjacency matrix
    current_graph <- graph.adjacency(adj_matrix, mode = 'undirected') 
    
    # Compute clusters
    current_clusters = clusters(current_graph)
    
    # Assign membership to each point
    current_time_data$fire_id <- current_clusters$membership[1:total_current_obs]
    
    # Adjust fire_id for hotspots
    if (total_active_group > 0){
      active_group$new_fire_id <- current_clusters$membership[(total_current_obs + 1):total_current_points]
      
      # Expand all combination between memberships of hotspots and active groups
      combination_tbl <- expand.grid(current_time_data$fire_id, active_group$new_fire_id)
      
      # A vector to represent matched membership
      combination_tbl <- combination_tbl$Var1 == combination_tbl$Var2
      
      # Turn this to a matrix
      mat_a <- matrix(combination_tbl, nrow = total_current_obs)
      
      # Extract distance matrix
      mat_b <- dist_matrix[1:total_current_obs,(total_current_obs + 1):total_current_points]
      if (is.vector(mat_b)) {mat_b <- matrix(mat_b, nrow = total_current_obs) }
      
      if (all(dim(mat_a) != dim(mat_b))) stop("unmatched matrices")
      
      # Combine membership matrix and distance matrix
      mat_c <- mat_a * mat_b
      
      if (sum(mat_c < 0) > 0) stop("mat_c contains negative value")
      if (anyNA(mat_c)) stop("mat_c contains NA")
      
      # Find positive min for each row
      nearest_active_group <- apply(mat_c,
                                    1,
                                    FUN = function(x){
                                      ifelse(sum(x) == 0,
                                             0,
                                             which(x == min(x[x > 0]))[1])
                                    })
      
      if (anyNA(nearest_active_group)) stop("nearest_active_group contians NA")
      if (!is.atomic(nearest_active_group)) stop("nearest_active_group is not a vector")
      
      # Assign nearest active group fire_id to hotspots
      if (sum(nearest_active_group > 0) > 0){
        
        hotspots_index <- which(nearest_active_group > 0)
        active_group_index <- nearest_active_group[hotspots_index]
        
        current_time_data$fire_id[hotspots_index] <- active_group$fire_id[active_group_index]
      }
      
    }
    
    
    # Adjust membership label
    index <- 1:total_current_obs
    
    if (total_active_group > 0){
      if (length(which(nearest_active_group > 0)) > 0){
        index <- -which(nearest_active_group > 0)
      }
    }
    
    temp_membership <- current_time_data$fire_id[index]
    
    # Let membership start from 1 and common difference equal to 1
    if (length(temp_membership) > 0) {
      temp_membership <- data.frame(temp_membership = temp_membership) %>%
        group_indices(temp_membership)
      
      # Add the total known group
      temp_membership <- temp_membership + max(fire_mov$fire_id)
      
      # Assign them back to hotspots
      current_time_data$fire_id[index] <- temp_membership
    }
    
    # Process fire movement
    current_fire_mov <- current_time_data %>%
      group_by(fire_id) %>%
      summarise(r = calc_r(lon, lat),
                lon = mean(lon), 
                lat = mean(lat),
                mean_r = as.numeric(stringr::str_split(r, ",")[[1]][1]),
                max_r = as.numeric(stringr::str_split(r, ",")[[1]][2])) %>%
      ungroup() %>%
      mutate(active = 0) %>%
      mutate(time_id = i) %>%
      arrange(fire_id) %>%
      select(-r)
    
    if (any(duplicated(current_fire_mov$fire_id))) stop("duplicated fire_id found")
    
    # Update information in fire_mov
    fire_mov <- fire_mov %>%
      filter(!(fire_id %in% current_fire_mov$fire_id))
    fire_mov <- bind_rows(fire_mov, current_fire_mov)
    fire_mov <- arrange(fire_mov, fire_id)
    fire_mov <- mutate(fire_mov, time_id = i)
    
    # Store fire movement in list
    fire_move_ls[[i]] <- current_fire_mov %>%
      select(-active)
    
    # Assign the fire_id to a vector
    fire_id <- c(fire_id, current_time_data$fire_id)
    id <- c(id, current_time_data$id)
    
    # Saving memory by occasionally writing to database
    if ((i %% save_point == 0) & (memory_saving)){
      
      my_fire_db <- DBI::dbConnect(RSQLite::SQLite(), fire_mov_dir)
      
      temp_fire_move <- fire_move_ls %>%
        reduce(rbind)
      
      table_count <- table_count + 1
      
      if (nrow(temp_fire_move) > 0) {
        DBI::dbWriteTable(my_fire_db, paste0("FIRE_MOV", table_count), temp_fire_move)
      }
      
      rm(temp_fire_move)
      
      for (j in 1:i){
        fire_move_ls[[j]] <- tibble()
      }
      
      RSQLite::dbDisconnect(my_fire_db)
      
    }
    
    
    
    
    ## end of one iteration of for loop
  }
  
  
  if (memory_saving){
    
    my_fire_db <- DBI::dbConnect(RSQLite::SQLite(), fire_mov_dir)
    
    temp_fire_move <- fire_move_ls %>%
      reduce(rbind)
    
    table_count <- table_count + 1
    
    if (nrow(temp_fire_move) > 0) {
      DBI::dbWriteTable(my_fire_db, paste0("FIRE_MOV", table_count), temp_fire_move)
    }
    
    rm(temp_fire_move)
    
    RSQLite::dbDisconnect(my_fire_db)
    
  } else {
    
    # Store fire movement in nominated database
    message(paste0('Saving fire movement to DIR: "', fire_mov_dir,'"'))
    
    fire_move_ls <- fire_move_ls %>%
      reduce(rbind)
    
    if (file.exists(fire_mov_dir)) file.remove(fire_mov_dir) 
    
    my_fire_db <- DBI::dbConnect(RSQLite::SQLite(), fire_mov_dir)
    
    DBI::dbWriteTable(my_fire_db, "FIRE_MOV", fire_move_ls)
    
    RSQLite::dbDisconnect(my_fire_db)
    
  }
  
  
  
  result <- list(fire_id = fire_id,
                 method = method,
                 adj_distance = adj_distance,
                 active_time = active_time,
                 timestamps = max_time,
                 fire_mov_dir = fire_mov_dir)
  
  class(result) <- c("clusteringResult", class(result))
  
  return(result)
}



# Result print method for better presentation
print.clusteringResult <- function(x){
  
  cat('*-------------------------------------------*\n')
  cat('')
  cat('Results of hotspots clustering:\n')
  cat('\n')
  cat('  Observations:       ', length(x$fire_id),      '\n')
  cat('  Clusters:           ', max(x$fire_id),         '\n')
  cat('  Timestamps:         ', x$timestamps,           '\n')
  cat('  Adjacency Distance: ', x$adj_distance,         '\n')
  cat('  Active Time:        ', x$active_time,          '\n')
  cat('  Method:             ', x$method,               '\n')
  cat('  Fire Movement DIR:  ', x$fire_mov_dir,         '\n')
  cat('\n')
  cat('*-------------------------------------------*\n')

}



# Function to calculate mean and max distance to the centroid
calc_r <- function(lon, lat){
  cen_lon <- mean(lon)
  cen_lat <- mean(lat)
  centroid <- data.frame(lon = cen_lon, lat = cen_lat)
  points <- data.frame(lon = lon, lat = lat)
  dist_vector <- geodist::geodist(points, centroid)
  
  paste(format(mean(dist_vector), digits = 3), 
        format(max(dist_vector), digits = 3),
        sep = ",") %>%
    return()
}











