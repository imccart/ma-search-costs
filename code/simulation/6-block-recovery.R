# Conditional recovery on the same simulated panel in every block.
# Other parameters are fixed at truth.
# Each block has its own output folder and uses 31 random-effect nodes.
# Override settings with block_recovery_config before sourcing this file.
local({
  config <- modifyList(list(n_b = 80L, waves = 3L, starts = 3L, reps = 1L,
                            maxit = 500L, n_nu = 31L, seed = 1729L),
    if (exists('block_recovery_config', inherits = TRUE)) block_recovery_config else list())
  for (block_name in c('utility', 'search_cost', 'consideration')) {
    env <- new.env(parent = globalenv())
    env$recovery_config <- modifyList(config,
      list(block = block_name, label = paste0('block_', block_name)))
    cat('\nConditional recovery:', block_name, '\n')
    sys.source('code/simulation/4-parameter-recovery.R', envir = env)
  }
  runs <- data.table::rbindlist(lapply(c('utility', 'search_cost', 'consideration'), function(b) {
    data.table::fread(file.path('results/simulation', paste0('recovery_block_', b), 'runs.csv'))
  }))
  data.table::fwrite(runs, 'results/simulation/block_recovery_runs.csv')
  print(runs)
})
