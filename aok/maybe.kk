// Flattens a double-`maybe`.
fun flatten(m : maybe<maybe<a>>) : maybe<a>
  match m
    Nothing -> Nothing
    Just(m') -> m'
