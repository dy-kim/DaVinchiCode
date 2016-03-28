initPlayers <- function(nicknames) {
  num.player <- length(nicknames)

  data_frame(nickname = nicknames,
             id = sample(x = 1:num.player,
                         size = num.player)) %>%
    arrange(id) %>%
    return()
}

requestGameState <- function(blocks, requester_id) {
  blocks$visible[blocks$owner == requester_id] <- TRUE
  return(blocks)
}

pickBlock <- function(blocks, color = COLOR_SET, player_id) {
  color <- match.arg(color)
  block.targets <-
    blocks %>%
    filter(owner == UNOWNED &
             color == color)

  if (nrow(block.targets) == 0L) {
    block.targets <-
      blocks %>%
      filter(owner == UNOWNED,
             color != color)
  }

  block.targets %>%
    sample_n(size = 1) %>%
    mutate(owner = player_id) %>%
    updateBlockState(blocks) %>%
    return()
}

initialPick <- function(blocks, player_id, col_seq = EVEN_COLOR) {
  stopifnot(length(col_seq) == INITIAL_BLOCKS)

  blocks %>%
    pickBlock(color = col_seq[1], player_id) %>%
    pickBlock(color = col_seq[2], player_id) %>%
    pickBlock(color = col_seq[3], player_id) %>%
    pickBlock(color = col_seq[4], player_id) %>%
    return()
}
