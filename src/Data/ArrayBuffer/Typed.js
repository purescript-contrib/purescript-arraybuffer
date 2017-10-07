"use strict";

// module Data.ArrayBuffer.Typed

exports.asInt8Array = function(v) {
  return new Int8Array(v.buffer, v.byteOffset, v.byteLength);
}

exports.asInt16Array = function(v) {
  return new Int16Array(v.buffer, v.byteOffset, v.byteLength >>> 1);
}

exports.asInt32Array = function(v) {
  return new Int32Array(v.buffer, v.byteOffset, v.byteLength >>> 2);
}

exports.asUint8Array = function(v) {
  return new Uint8Array(v.buffer, v.byteOffset, v.byteLength);
}

exports.asUint16Array = function(v) {
  return new Uint16Array(v.buffer, v.byteOffset, v.byteLength >>> 1);
}

exports.asUint32Array = function(v) {
  return new Uint32Array(v.buffer, v.byteOffset, v.byteLength >>> 2);
}

exports.asUint8ClampedArray = function(v) {
  return new Uint8ClampedArray(v.buffer, v.byteOffset, v.byteLength);
}

exports.asFloat32Array = function(v) {
  return new Float32Array(v.buffer, v.byteOffset, v.byteLength >>> 2);
}

exports.asFloat64Array = function(v) {
  return new Float64Array(v.buffer, v.byteOffset, v.byteLength >>> 3);
}

exports.dataView = function(a) {
  return a;
}

exports.setImpl = function(ra, off, a) {
  return function() {
    a.set(ra, off);
  };
}

exports.unsafeAtImpl = function(a, i) {
  return function() {
   return a[i];
  };
}

exports.hasIndexImpl = function(a, i) {
  return i in a;
}

exports.toArray = function(a) {
  var l = a.length;
  var ret = new Array(l);
  for (var i = 0; i < l; i++)
    ret[i] = a[i];
  return ret;
}

exports.toIntArray = function(a) {
  var l = a.length;
  var ret = new Array(l);
  for (var i = 0; i < l; i++)
    ret[i] = a[i] | 0;
  return ret;
}
