import Lake
open Lake DSL

require batteries from git 
  "https://github.com/leanprover-community/batteries" @ "v4.8.0"

package aoc2018

@[default_target]
lean_exe day01 where
  root := `Day01

lean_exe day05 where
  root := `Day05
