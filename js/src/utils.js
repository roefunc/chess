"use strict"
/* @flow */

export let extendsObject = function(child, parent, methods) {
  child.prototype = Object.create(parent.prototype);
  for (let n in methods) {
    if (methods.hasOwnProperty(n)) {
      child.prototype[n] = methods[n];
    }
  }
}

export let checkThis = function(self, parent) {
  if (self === undefined) {
    return Object.create(parent.prototype);
  }
  return self;
}

export let override = function(that, method_name, callback) {
  let f = that[method_name];
  that[method_name] = callback(f);
}
