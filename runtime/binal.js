function Tuple (xs) {
  this.xs = xs;
}

function mkTuple (xs) {
  return new Tuple(xs);
}

function NonExhaustivePatterns(message) {
  this.message = message;
}

NonExhaustivePatterns.prototype = {
  prototype: Error.prototype,
  name: "NonExhaustivePatterns"
};
