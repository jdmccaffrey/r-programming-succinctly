# beecolony.R
# R 3.2.4

Bee = setClass(  # S4 class
  "Bee",

  slots = list(
    type = "integer", #1 = worker, 2 = scout
    path = "vector",
    error = "numeric"
  )

) # Bee

# -----

make_data = function(nc) {
  result <- matrix(0.0, nrow=nc, ncol=nc)
  for (i in 1:nc) {
    for (j in 1:nc) {
      if (i < j) {
        result[i,j] <- 1.0 * (j - i)
      }
      else if (i > j) {
        result[i,j] <- 1.5 * (i - j)
      }
      else {
        result[i,j] <- 0.0
      }
    }
  }
  return(result)
}

error = function(path, distances) {
  nc <- nrow(distances)
  mindist <- nc - 1
  actdist <- 0
  for (i in 1:(nc-1)) {
    d <- distances[path[i], path[i+1]]
    actdist <- actdist + d
  }
  return(actdist - mindist)
}

solve = function(nc, nb, distances, max_epochs) {
  # create nb random bees
  numWorker <- as.integer(nb * 0.80)
  numScout <- nb - numWorker
  hive <- list()

  for (i in 1:nb) {
    b <- new("Bee")

    # set type
    if (i <= numWorker) {
      b@type <- as.integer(1) # worker
    }
    else {
      b@type <- as.integer(2) # scout
    }
    # set a random path and its error
    b@path <- sample(1:nc)
    b@error <- error(b@path, distances)

    hive[[i]] <- b # place bee in hive
  }
 
  # find initial best (lowest) error
  best_error <- 1.0e40 # really big
  best_path <- c(1:nc) # placeholder path
  for (i in 1:nb) {
    if (hive[[i]]@error < best_error) {
      best_error <- hive[[i]]@error
      best_path <- hive[[i]]@path
    }
  }

  cat("Best initial path = \n")
  print(best_path)
  cat("Best initial error = ")
  cat(formatC(best_error, digits=1, format="f"))
  cat("\n\n")

  # main processing loop
  epoch <- 1
  while (epoch <= max_epochs) {
    
    # cat("epoch =", epoch, "\n")
    if (best_error <= 1.0e-5) { break }

    # process each bee
    for (i in 1:nb) { 
      if (hive[[i]]@type == 1) { # worker
        # get a neighbor path and its error
        neigh_path <- hive[[i]]@path
        ri <- sample(1:nc, 1)
        ai <- ri + 1
        if (ai > nc) { ai <- as.integer(1) }
        tmp <- neigh_path[[ri]]
        neigh_path[[ri]] <- neigh_path[[ai]]
        neigh_path[[ai]] <- tmp
        neigh_err <- error(neigh_path, distances)

        # is neighbor path better?
        p <- runif(1, min=0.0, max=1.0)
        if (neigh_err < hive[[i]]@error || p < 0.05) {
          hive[[i]]@path <- neigh_path
          hive[[i]]@error <- neigh_err

          # new best?
          if (hive[[i]]@error < best_error) {
            best_path <- hive[[i]]@path
            best_error <- hive[[i]]@error
            cat("epoch =", formatC(epoch, digits=4))
            cat(" new best path found ")
            cat("error = ")
            cat(formatC(best_error, digits=1, format="f"))
            cat("\n")
          }
        } # neighbor is better
        
      }
      else if (hive[[i]]@type == 2) { # scout
        # try random path
        hive[[i]]@path <- sample(1:nc)
        hive[[i]]@error <- error(hive[[i]]@path, distances)
        # new best?
        if (hive[[i]]@error < best_error) {
          best_path <- hive[[i]]@path
          best_error <- hive[[i]]@error
          cat("epoch =", formatC(epoch, digits=4))
          cat(" new best path found ")
          cat("error = ")
          cat(formatC(best_error, digits=1, format="f"))
          cat("\n")
        }

        # waggle dance to a worker
        wi <- sample(1:numWorker, 1)  # random worker
        if (hive[[i]]@error < hive[[wi]]@error) {
          hive[[wi]]@error <- hive[[i]]@error
          hive[[wi]]@path <- hive[[i]]@path 
        }
      } # scout
 
    } # each bee
    epoch <- epoch + 1
  } # while

  cat("\nProcessing complete \n")
  cat("Best error found = ")
  cat(formatC(best_error, digits=1, format="f"))
  cat("\n")

  cat("Best path found = \n")
  print(best_path)
} # solve

# -----

cat("\nBegin TSP using bee colony optimization demo \n\n")

set.seed(7) # 
numCities <- as.integer(20)
numBees <- as.integer(100)
max_epochs <- as.integer(5000)

cat("Setting numCities =", numCities, "\n")
cat("Setting numBees =", numBees, "max_epochs =", max_epochs, "\n\n")

distances <- make_data(numCities ) # city-city distances

optPath <- c(1:numCities)
cat("Optimal path = \n")
print(optPath)
cat("\n")

solve(numCities, numBees, distances, max_epochs)

cat("\nEnd demo \n")
