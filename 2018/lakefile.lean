import Lake
open Lake DSL

package «aoc2018» {
  -- add package configuration options here
}

lean_lib «Gen» {
  -- add library configuration options here
}

@[default_target]
lean_exe «Day01» {
  root := `Day01
}
