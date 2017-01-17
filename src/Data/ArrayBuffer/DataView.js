"use strict";

// module Data.ArrayBuffer.DataView


exports.whole = function(b) {
  return function() {
    return new DataView(b);
  }
}

exports.sliceImpl = function(just, nothing, s, l, b) {
  return function() {
    return s + l <= b.byteLength ? just(new DataView(b, s, l)) : nothing;
  }
}

exports.buffer = function(v) {
  return function() {
    return v.buffer;
  }
}

exports.byteOffset = function(v) {
  return function() {
    return v.byteOffset;
  }
}

exports.byteLength = function(v) {
  return function() {
    return v.byteLength;
  }
}

exports.getterImpl = function(just, nothing, s, l, e, v, o) {
  return function() {
    return (o + l) <= v.byteLength? just(v[s].call(v,o,e)) : nothing;
  };
}

exports.setter = function(s) {
  return function(e) {
    return function(v) {
      var f = v[s];
      return function(n) {
        return function(o) {
            return function() {
            f.call(v,o,n,e);
          };
        };
      };
    };
  };
}
