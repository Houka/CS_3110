let roulette _ =
  let x = Random.int (36) in
  string_of_int(x) ^ if x=0 then "green"

  else (
    if x<=10 || (x>=19 && x<=28) then (
      if x mod 2 = 0 then "black"
      else "red"
    )
    else (
        if x mod 2 = 0 then "red"
        else "black"

    )
  )

