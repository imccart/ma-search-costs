# export-paper-numbers.R — Export key numbers referenced in paper text
# Writes results/paper-numbers.csv for inline citation in paper.qmd

# Collect numbers referenced in paper
paper_numbers <- tribble(
  ~label, ~value, ~description
)

write_csv(paper_numbers, "results/paper-numbers.csv")
