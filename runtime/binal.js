function Tuple (xs) {
  this.xs = xs;
}

function mkTuple (xs) {
  return new Tuple(xs);
}
