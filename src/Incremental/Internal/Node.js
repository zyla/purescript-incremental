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

/*
for field in adjustedHeight changedAt dependents height name observers source value; do
cat <<EOF
exports.get_${field} = function(node) { return node.${field}; };
exports.set_${field} = function(node, value) { node.${field} = value; };
EOF
done
*/

exports.get_adjustedHeight = function(node) { return node.adjustedHeight; };
exports.set_adjustedHeight = function(node, value) { node.adjustedHeight = value; };
exports.get_changedAt = function(node) { return node.changedAt; };
exports.set_changedAt = function(node, value) { node.changedAt = value; };
exports.get_dependents = function(node) { return node.dependents; };
//exports.set_dependents = function(node, value) { node.dependents = value; };
exports.get_height = function(node) { return node.height; };
exports.set_height = function(node, value) { node.height = value; };
exports.get_name = function(node) { return node.name; };
exports.set_name = function(node, value) { node.name = value; };
exports.get_observers = function(node) { return node.observers; };
//exports.set_observers = function(node, value) { node.observers = value; };
exports.get_source = function(node) { return node.source; };
//exports.set_source = function(node, value) { node.source = value; };
exports.get_value = function(node) { return node.value; };
exports.set_value = function(node, value) { node.value = value; };
