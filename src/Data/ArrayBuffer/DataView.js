"use strict";

// module Data.ArrayBuffer.DataView


exports.whole = function(b) {
  return new DataView(b);
}

exports.sliceImpl = function(just, nothing, s, l, b) {
  return s + l <= b.byteLength? just(new DataView(b, s, l)) : nothing;
}

exports.buffer = function(v) {
  return v.buffer;
}

exports.byteOffset = function(v) {
  return v.byteOffset;
}

exports.byteLength = function(v) {
  return v.byteLength;
}

exports.getterImpl = function(just, nothing, s, l, v, o) {
  return function() {
    return (o + l) <= v.byteLength? just(v[s].call(v,o)) : nothing;
  };
}

exports.setter = function(s) {
  return function(v) {
    var f = v[s];
    return function(n) {
      return function(o) {
        return function() {
          f.call(v,o,n);
        };
      };
    };
  };
}
