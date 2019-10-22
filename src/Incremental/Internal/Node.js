exports._new = function(none, source, dependents, observers, value, height) {
  return {
    source: source,
    dependents: dependents,
    observers: observers,
    value: value,
    height: height,
    adjustedHeight: height,
    inRecomputeQueue: false,
    nextInRecomputeQueue: none,
    name: '',
    changedAt: 0,
  };
};

