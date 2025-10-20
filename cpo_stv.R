# This is a script for processing votes via ranked choice voting
library(tidyverse)
library(magrittr)
library(readxl)
library(stringi)
library(gtools)
library(parallel)
library(vote)

default_seed <- 38725

# A list of tiebreak methods listed in order of preference.
tie_methods <- c("borda", "random", "stv", "cpo_stv")
# Random can only ever yield one result, and stv has its own tiebreakers built 
# in. Borda is currently configured correctly for tiebreaking and having ties
# broken. cpo_stv will correctly refer ties to a tiebreaker, but it is not
# configured correctly to serve as a tiebreaker.
# I'd like to implement the "ordered" tiebreaker from stv as an option.

# If passed TRUE, returns the function print. If FALSE, invisible.
verbose_setup <- function(verbose) {
  if (verbose == TRUE) {
    return(print)
  } else {
    return(invisible)
  }
}

# Checks if all items of a vector are identical
homogenous <- function(x) {
  identicalValue <- function(x,y) if (identical(x,y)) x else FALSE
  return(Reduce(identicalValue, x) != FALSE)
}

# Finds the longest common substrings at the beginning and end of column names,
# then removes them
removeQuestion <- function(df) {
  cols <- colnames(df)
  # find the length of the shortest column name
  sortedcols <- cols[order(nchar(cols), cols)]
  shortest <- nchar(head(sortedcols, n=1))
  
  # Find the longest substring that 
  # a) all column names have in common and
  # b) occurs at the beginning of all column names
  # this will be the question from the Google Form
  LCS1 <- ""
  i <- shortest
  repeat {
    test <- str_sub(cols, 1, i)
    if (homogenous(test)) {
      LCS1 <- test[1]
      break
    }
    i <- i - 1
    if (i < 1) {break}
  }
  
  # Find the longest substring that
  # a) all column names have in common and
  # b) occcurs at the end of all column names
  # this will be some suffix imposed by Google Forms
  LCS2 <- ""
  i <- 1
  repeat {
    test <- str_sub(cols, -i)
    if (homogenous(test)) {
      LCS2 <- test[1]
    } else {break}
    i <- i + 1
    if (i >= shortest) {break}
  }
  
  # now actually remove both LCS, then return the updated dataframe
  colnames(df) <- cols %>%
    str_replace(LCS1, "") %>%
    stri_replace_last_fixed(LCS2, "") %>%
    str_replace_all("\\.+", "\\.")
  return(df)
}

# Remove spaces between rankings on individual ballots
normalize_rankings <- function(df) {
  # May not function as expected if ties exist!
  # Unmarked candidates will be ranked last. If multiple candidates are
  # unranked, they will be tied for last.
  df %>%
    # First, handle missing values by imputing one below the lowest rank
    mutate(max_rank = do.call(pmax, c(., na.rm = TRUE))) %>%
    mutate(across(everything(), ~ if_else(is.na(.), max_rank + 1, .))) %>%
    select(-max_rank) %>%
    # Pivot all rankings into one column, then group by ballot ID
    mutate(ballot_id = row_number()) %>%
    pivot_longer(
      cols = -ballot_id, 
      names_to = "candidate", 
      values_to = "ballot_ranking"
    ) %>%
    group_by(ballot_id) %>%
    # Create a new column with the normalized rank
    mutate(ranking = min_rank(ballot_ranking)) %>%
    # Put the data back how we found it before returning it
    ungroup() %>%
    select(-ballot_ranking) %>%
    pivot_wider(
      names_from = candidate,
      values_from = ranking
    ) %>% 
    arrange(ballot_id) %>%
    select(-ballot_id) %>%
    return()
}

normalize_if <- function(df, normalize) {
  if (normalize == TRUE) {
    df %>%
      normalize_rankings() %>%
      return()
  } else {
    return(df)
  }
}

generate_outcome_matrix <- function(n) {
  output <- matrix(rep.int(0, n^2), nrow = n, ncol = n)
  for (i in 1:n) {
    output[i, i] <- 1
  }
  return(output)
}

# Appends a row or column of zeroes to the beginning or end of a matrix
add_empty <- function(mtx, row = TRUE, before = TRUE) {
  if (row == TRUE) {
    if (before == TRUE) {
      return(rbind(
        matrix(
          rep.int(0, ncol(mtx)),
          nrow = 1,
          ncol = ncol(mtx)
        ), 
        mtx
      ))
    } else { # before == FALSE
      return(rbind(
        mtx,
        matrix(
          rep.int(0, ncol(mtx)),
          nrow = 1,
          ncol = ncol(mtx)
        )
      ))
    }
  } else { # row == FALSE
    if (before == TRUE) {
      return(cbind(
        matrix(
          rep.int(0, nrow(mtx)),
          nrow = nrow(mtx),
          ncol = 1
        ), 
        mtx
      ))
    } else { # before == FALSE
      return(cbind(
        mtx,
        matrix(
          rep.int(0, nrow(mtx)),
          nrow = nrow(mtx),
          ncol = 1
        )
      ))
    }
  }
}

# Returns the row in a votes dataframe corresponding to a certain index
get_voter_ballot <- function(df, voter) {
  return(df[unlist(voter),])
}

# Given a candidate on a ballot, determine if that candidate is the voter's
# top preference
is_top_preference <- function(ballot, candidate) {
  return(ballot[candidate] == min(ballot))
}

# Get a list of voters whose top preference is a particular candidate
get_candidate_voters <- function(df, candidate) {
  voters <- list()
  for (ballot in 1:nrow(df)) {
    condition <- get_voter_ballot(df, ballot) %>%
      is_top_preference(candidate)
    if (condition) {
      voters %<>% append(ballot)
    }
  }
  return(voters)
}

# Return the number of voters for a particular candidate
get_candidate_score <- function(df, candidate) {
  return(get_candidate_voters(df, candidate) %>% length())
}

# Create an initial set of scores from a set of ballots
generate_scores <- function(df) {
  scores <- list()
  for (candidate in 1:ncol(df)) {
    scores[colnames(df)[candidate]] <- get_candidate_score(df, candidate)
  }
  return(scores)
}

# Determine if there are scores over the quota that are eligible for transfer
need_transfer <- function(scores, transfer_eligible, quota) {
  output <- NULL
  compare_thing <- NULL
  if (length(transfer_eligible) > 1) {
    compare_thing <- max(unlist(scores[transfer_eligible]))
  } else {
    compare_thing <- scores[transfer_eligible[1]]
  }
  output <- (compare_thing > quota)
  return(output)
}

# Transfer any surplus votes to the next selected candidate
transfer_surplus <- function(df, scores, transfer_eligible, quota, 
                             eliminated = list(), report = verbose_setup(FALSE),
                             deport = verbose_setup(FALSE)) {
  if (length(transfer_eligible) == 0) {
    return(scores)
  } else if (need_transfer(scores, transfer_eligible, quota)) {
    # Identify whose surplus we're transferring
    top_score <- max(unlist(scores[transfer_eligible]))
    top_candidate <- names(scores)[which(scores==top_score)[1]]
    
    # Remove previously eliminated candidates before checking who voted for
    # the currently being eliminated candidate
    df_temp1 <- df
    if (length(eliminated) > 0) {
      df_temp1 %<>% select(-any_of(unlist(eliminated)))
    }
    
    # Calculate the vote fraction
    surplus <- top_score - quota
    voters <- get_candidate_voters(df_temp1, top_candidate)
    num_voters <- length(voters)
    fraction <- surplus / num_voters
    
    # Remove the candidate whose surplus was transferred from the list
    # of candidates eligible for transfer in the next recursion
    # Note: I'm not sure if it's okay to do this, but I'm doing it.
    # At worst, this is doing it the standard STV way.
    eliminated %<>% append(top_candidate)
    top_candidate_index <- which(transfer_eligible==top_candidate)
    transfer_eligible <- transfer_eligible[-top_candidate_index]
    
    # Transfer votes
    # First, subtract the surplus from the candidate in question
    scores[[top_candidate]] <- quota
    # Now, create temporary ballots without any surplus candidates
    df_temp2 <- df %>% select(-any_of(unlist(eliminated)))
    # Then iterate through the voters with extra power
    for (voter in 1:length(voters)) {
      voter <- voters[voter]
      ballot <- get_voter_ballot(df_temp2, voter)
      # Identify their top remaining preference
      top_preference <- names(ballot)[which(ballot == min(ballot))]
      # And transfer their fractional vote
      if (length(top_preference) == 1) {
        scores[[top_preference]] %<>% add(fraction) 
      } else {
        warning(paste("Incomplete ballot or equal ranking detected.",
                      "Ballot will be considered exhausted for this comparison,",
                      "no further transfers possible."))
      }
    }
    # Recurse
    transfer_surplus(df, scores, transfer_eligible, 
                     quota, eliminated = eliminated)
  } else {
    return(scores)
  }
}

borda <- function(df, seats = 3, ties = tie_methods, normalize = TRUE, 
                  seed = default_seed, ...) {
  # For running code manually to debug:
  # df <- read.csv("Cycle 4.csv") %>% select(2:9) %>% removeQuestion()
  # seats <- 3
  # ties <- c("random")
  # normalize <- TRUE
  
  # Clean inputs #
  seats <- min(seats, length(colnames(df)) - 1)
  
  # Remove borda from tiebreak methods to be very sure we don't go recursive
  ties <- c(unlist(ties)) # Coerce to a vector in case it's a string
  ties <- ties[ties != "borda"]
  
  # Normalize data if applicable
  votes <- df %>% normalize_if(normalize)
  
  # Calculate Borda scores
  scores <- votes %>%
    summarize(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "candidate", values_to = "score") %>%
    arrange(score)
  
  # Check for a tie
  # Remember that Borda scores are like golf scores, lowest wins
  quota <- scores$score[seats]
  winners <- scores %>%
    filter(score <= quota)
  if (nrow(winners) == seats) {
    # No tie occurred, return winners
    return(list(winner = winners$candidate))
  } else {
    # A tie occurred, break the tie
    guaranteed <- winners %>% 
      filter(score < quota) %>%
      pull(candidate)
    tied <- winners %>% 
      filter(score == quota) %>% 
      pull(candidate)
    winner <- break_tie(
      df,
      seats = seats,
      guaranteed = guaranteed,
      tied = tied,
      ties = ties, 
      normalize = normalize, 
      seed = seed,
      ...
    )
    return(list(winner = winner))
  }
}

elect_random <- function(df, seats = 3, seed = default_seed) {
  set.seed(seed)
  winners <- df %>%
    select(all_of(sample(colnames(.), seats))) %>%
    colnames()
  return(list(winner = winners))
}

# If the result of an election is inconclusive, this is used to try the next
# option
break_tie <- function(df, seats, guaranteed = c(), tied, ties, ...) {
  # clean data
  votes <- df %>%
    select(all_of(tied)) %>%
    normalize_rankings()
  seats <- seats - length(guaranteed)
  
  # right now, anything not recognizable will be interpreted as "random"
  method <- ties[1]
  ties <- ties[-1]
  if (method == "stv") {
    # Run stv
    winners <- stv(votes, nseats = seats, quiet = TRUE, ...)
    winners$winner <- winners$elected # can remove if I change "winner" to
    # "elected" throughout
  } else if (method == "borda") {
    # Run Borda
    winners <- borda(votes, seats = seats, ties = ties, ...)
    # } else if (method == "ordered") {
    # Run ordered
    # Sort candidates by first votes. If there is a tie, sort by second votes.
    # Continue down the ballots until no ties remain.
    # Commented out until the function for an "ordered" vote is written.
  } else if (method == "cpo_stv") {
    # Run cpo_stv
    winners <- cpo_stv(votes, seats = seats, ties = ties, ...)
  } else {
    # Run random
    winners <- elect_random(votes, seats = seats, ...)
  }
  winners$winner <- c(unlist(guaranteed), unlist(winners$winner))
  return(winners)
}

cpo_stv <- function(df, seats = 3, normalize = TRUE, multi = FALSE, 
                    ties = tie_methods, seed = default_seed, verbose = FALSE,
                    debug_mode = FALSE, ...) {
  # For running code manually to debug:
  # df <- read.csv("Cycle 7.csv") %>% select(2:9) %>% removeQuestion()
  # seats <- 3
  # normalize <- T
  # multi <- F
  # ties <- c("borda")
  # verbose <- T
  # debug_mode <- T
  
  # Record start time
  start.time <- Sys.time()
  
  # Clean inputs #
  # Adjust seats if necessary
  seats <- min(seats, ncol(df) - 1)
  
  # If normalize was used, normalize rankings
  ballots <- df %>% normalize_if(normalize)
  
  # Remove cpo_stv from tiebreak methods to be very sure we don't go recursive
  ties <- c(unlist(ties)) # Coerce to a vector in case it's a string
  ties <- ties[ties != "cpo_stv"]
  
  # Set up the function for verbose logging
  report <- verbose_setup(verbose)
  deport <- verbose_setup(debug_mode)
  
  ############################################################################
  # Using the procedure from https://en.wikipedia.org/wiki/CPO-STV#Procedure #
  ############################################################################
  
  # Get the candidate list
  candidates <- colnames(ballots)
  numcandidates <- length(candidates)
  
  report(paste("There are", numcandidates, "candidates:", 
               paste(unlist(candidates))))
  
  # Determine the Droop quota
  quota <- nrow(ballots) / (seats + 1)
  
  # Determine if any candidate meets the quota in first votes.
  # Because CPO-STV will always elect any such candidate, any outcomes not
  # including such a candidate can be excluded from consideration.
  first_vote_scores <- generate_scores(ballots)
  first_vote_winners <- candidates[which(first_vote_scores > quota)]
  num_first_winners <- length(first_vote_winners)
  report(paste("Checked for first-preference winners, found", 
               first_vote_winners))
  
  # If this process eliminates all but one outcome, return that outcome as the
  # winner and skip all the tabulating.
  if (num_first_winners == seats) {
    return(list(winner = first_vote_winners,
                outcomes = first_vote_winners, 
                outcome_matrix = matrix(c(1)),
                matchups = NULL
    ))
  }
  
  # Find all possible outcomes for the election
  outcomes <- combinations(
    # We can omit first vote winners from the combinations calculation
    n = numcandidates - num_first_winners,
    r = seats - num_first_winners,
    v = candidates[!(candidates %in% first_vote_winners)]
  )
  numoutcomes <- nrow(outcomes)
  # Then we add the first vote winners back in as columns
  outcomes <- first_vote_winners %>% 
    sapply(function (winner) rep(winner, numoutcomes)) %>%
    unname() %>%
    cbind(outcomes)
  
  # Find all possible pairs of outcomes
  matchups <- combinations(numoutcomes, 2)
  nummatchups <- nrow(matchups)
  
  report(paste(
    "Comparing", nummatchups,
    "pairs of", numoutcomes,
    "possible outcomes of an election for", seats,
    "seats from", numcandidates, "total candidates..."
  ))
  
  # Define a function to determine the winner of a given matchup
  compare_matchup <- function(matchup) {
    # identify the two outcomes
    index_a <- matchups[matchup, 1]
    outcome_a <- outcomes[index_a,]
    
    index_b <- matchups[matchup, 2]
    outcome_b <- outcomes[index_b,]
    
    relevant_candidates <- unique(c(outcome_a, outcome_b))
    transfer_eligible <- intersect(outcome_a, outcome_b)
    
    # Eliminate candidates in neither outcome
    votes <- ballots %>% select(all_of(relevant_candidates))
    
    # Transfer surpluses of candidates in both outcomes
    # Using the Gregory method
    scores <- votes %>% generate_scores()
    votes <- transfer_surplus(votes, scores, transfer_eligible, quota, 
                              report = report, deport = deport)
    
    # Add up the totals
    score_a <- scores %>%
      keep(names(.) %in% outcome_a) %>%
      unlist() %>%
      sum()
    score_b <- scores %>%
      keep(names(.) %in% outcome_b) %>%
      unlist() %>%
      sum()
    
    result_a = NULL
    if (score_a > score_b) {
      result_a <- 1
    } else if (score_a < score_b) {
      result_a <- 0
    } else {
      result_a <- 0.5
    }
    
    # And return result_a
    return(list(
      index_a = index_a,
      index_b = index_b,
      score_a = score_a,
      score_b = score_b,
      result_a = result_a
    ))
  }
  
  # Check the number of cores so we don't go to the trouble of a cluster
  # if only one core is available for some reason
  num_cores <- detectCores()
  
  # Initialize a variable for matchup results
  comparisons_output <- NULL
  
  # Compare matchups
  if (!multi | num_cores <= 2) { # Single-threaded
    comparisons_output <- lapply(1:nummatchups, compare_matchup)
    
  } else { # Multi-threaded
    # Run matchup comparisons in parallel 
    # First, create an environment with the information the cluster will need
    cluster_env <- environment()
    global_objects <- c("get_voter_ballot", "is_top_preference", 
                        "get_candidate_voters", "get_candidate_score", 
                        "generate_scores", "need_transfer", "transfer_surplus")
    list2env(mget(global_objects, envir = .GlobalEnv), envir = cluster_env)
    cluster_objects <- ls(envir = cluster_env)
    
    # Create a cluster
    cluster <- makeCluster(num_cores - 1) # Reserve one core for the OS
    
    # Send the environment to the cluster
    clusterExport(cluster, varlist = cluster_objects, envir = cluster_env)
    
    # Load packages on the cluster
    clusterEvalQ(cluster, {
      library(tidyverse)
      library(magrittr)
      library(gtools)
    })
    
    # Run the cluster
    comparisons_output <- parLapply(cluster, 1:nummatchups, compare_matchup)
    
    # Stop the cluster so it doesn't get angry at us
    stopCluster(cluster)
  }
  
  report("Matchup comparisons complete, determining winning outcome...")
  
  matchup_results <- do.call(rbind, comparisons_output) %>%
    data.frame() %>%
    mutate(across(everything(), unlist)) %>%
    mutate(result_b = 1 - result_a) %>%
    arrange(index_a, index_b)
  
  matrix_a <- matchup_results %>%
    select(index_b, index_a, result_a) %>%
    pivot_wider(names_from = index_a, values_from = result_a) %>%
    select(-index_b) %>%
    mutate(across(everything(), ~ if_else(is.na(.), 0, .))) %>%
    as.matrix() %>%
    add_empty(row = TRUE, before = TRUE) %>%
    add_empty(row = FALSE, before = FALSE)
  
  matrix_b <- matchup_results %>%
    select(index_a, index_b, result_b) %>%
    pivot_wider(names_from = index_b, values_from = result_b) %>%
    select(-index_a) %>%
    mutate(across(everything(), ~ if_else(is.na(.), 0, .))) %>%
    as.matrix() %>%
    add_empty(row = TRUE, before = FALSE) %>%
    add_empty(row = FALSE, before = TRUE)
  
  outcome_matrix <- pmax(
    matrix_a,
    matrix_b,
    generate_outcome_matrix(numoutcomes)
  )
  
  # Determine a winner
  # winner <- "" # I think this line does nothing, so I'm commenting it out for now
  total_scores <- colSums(outcome_matrix)
  max_score <- max(total_scores)
  winning_outcome_indices <- which(total_scores == max_score)
  winner <- unlist(outcomes[winning_outcome_indices,])
  num_winning_outcomes <- length(winning_outcome_indices)
  if (num_winning_outcomes == 1) {
    report(paste("A single election outcome was found that satisfies",
                 "Condorcet's criterion, defeating the other", max_score - 1, 
                 "possible outcomes. The winners are:"))
    report(winner)
  } else {
    report(paste("Multiple winning election outcomes were found.",
                 "Breaking tie with", ties[1]))
    
    # Implement tiebreakers
    win_frequency <- winner %>%
      data.frame() %>%
      mutate(outcome_id = row_number()) %>%
      pivot_longer(
        cols = -outcome_id,
        names_to = "seat",
        values_to = "candidate"
      ) %>%
      group_by(candidate) %>%
      summarize(outcome_count = n_distinct(outcome_id)) %>%
      ungroup()
    
    guaranteed <- win_frequency %>%
      filter(outcome_count == num_winning_outcomes) %>%
      pull(candidate)
    tied <- win_frequency %>%
      filter(outcome_count < num_winning_outcomes) %>%
      pull(candidate)
    
    winner <- break_tie(
      df,
      seats = seats,
      guaranteed = guaranteed,
      tied = tied,
      ties = ties, 
      normalize = normalize, 
      seed = seed, 
      verbose = verbose,
      ...
    )
  }
  
  # Print time taken
  report(paste("Code execution completed in:", 
               difftime(Sys.time(), start.time, units = "secs") %>%
                 round(3),
               "seconds"
  ))
  
  # Return a winner
  if (verbose == TRUE) {
    return(list(winner = winner, 
                outcomes = outcomes,
                outcome_matrix = outcome_matrix, 
                matchups = matchup_results
    ))
  } else {
    return(list(winner = winner))
  }
}

#filenames <- c("Cycle 1.csv", "Cycle 2.csv", "Cycle 4.csv", "Cycle 5.csv",
#               "Cycle 6.csv", "Cycle 7.csv", "Cycle 8.csv")
#filepaths <- file.path("Sample Data", filenames)
#
#election_results <- sapply(1:length(filenames), function(file) {
#  result <- filepaths[file] %>% 
#    read.csv() %>% # Open the votes
#    select(where(is_integer)) %>% # remove timestamp
#    removeQuestion() %>% # reduce column names to unique IDs, i.e., book titles
#   cpo_stv(verbose = FALSE) # actually run the vote to get the results
#  print(paste("The result of the election from file", filenames[file], "is:"))
#  print(result$winner)
#  return(result)
#})


# Notes:

# I don't understand what which(x, arr.ind = TRUE) does, but maybe it can make
# names(scores)[which(scores==top_score)[1]] and similar lines more elegant.

# For the far future, it would be cool to develop a visualization for the
# console while CPO-STV tabulation is in progress that shows the outcome matrix
# visually as it's filled up. A results visualization could show the outcome
# matrix with wins, losses, and draws in different colors.

# There is a more efficient way than a cluster to parallelize this function
# when running on UNIX-like systems, so development should eventually involve
# finding a method to detect if we're on such a system and, if so, take
# advantage of that.

# Conceptually, it should be feasible to make this robust to equal rankings and
# partial rankings, but that would take a great deal of work. An easier thing
# would be to create something that filters out votes with missing values,
# since the Google Form handles preventing equal rankings on its own.