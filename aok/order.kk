// Returns the opposite order.
pub fun (~)(o : order) : order
  match o
    Lt -> Gt
    Eq -> Eq
    Gt -> Lt

// Chains two orders for lexicographical comparison.
pub fun or(lhs : order, rhs : order) : order
  match lhs
    Eq -> rhs
    _ -> lhs

// Chains a list of two orders together lexicographically.
pub fun lexicographical(os : list<order>) : order
  os.foldl(Eq, or)
