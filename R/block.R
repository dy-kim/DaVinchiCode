initBlocks <- function(max_n = 11) {
  rbind(genBlock(max_n, 'black'),
        genBlock(max_n, 'white')) %>%
    mutate(owner = UNOWNED,
           visible = FALSE) %>%
    return()
}

genBlock <- function(max_n, color = COLOR_SET) {
  color <- match.arg(color)
  data_frame(number = NUM_JOCKER:max_n,
             color = color) %>%
    return()
}

updateBlockState <- function(block_target, blocks) {
  # Key: number, color
  index.to.update <-
    which(blocks$number == block_target$number &
            blocks$color  == block_target$color)

  blocks$owner[index.to.update]   <- block_target$owner
  blocks$visible[index.to.update] <- block_target$visible

  blocks %>%
    arrangeBlock() %>%
    return()
}

arrangeBlock <- function(blocks) {
  blocks %>%
    group_by(owner) %>%
    arrange(number, color) %>%
    ungroup() %>%
    return()
}
