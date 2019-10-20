// empty :: forall a. Effect (MutableArray a)
exports.empty = function() {
  return [];
};

// push :: forall a. EffectFn2 (MutableArray a) a Unit
exports.push = function(self, x) {
  self.push(x);
};

// length :: forall a. EffectFn1 (MutableArray a) Int
exports.length = function(self) {
  return self.length;
};
