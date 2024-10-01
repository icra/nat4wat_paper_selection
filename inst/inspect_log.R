read.table("log.txt", sep = ";") |> 
  tidytable::count(V1)
