// _get :: forall s a. Fn2 (Field s Immutable a) s a
exports._get = function(field, object) {
  return object[field];
};

// _read :: forall s a. EffectFn2 (Field s Mutable a) s a
exports._read = function(field, object) {
  return object[field];
};

// _write :: forall s a. EffectFn3 (Field s Mutable a) s a Unit
exports._write = function(field, object, value) {
  object[field] = value;
};
