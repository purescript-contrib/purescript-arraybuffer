"use strict";

// module Data.ArrayBuffer.ArrayBuffer

exports.create = function(s) {
  return function () {
    return new ArrayBuffer(s);
  };
}

exports.byteLength = function(a) {
  return a.byteLength;
}

exports.sliceImpl = function(s, e, a) {
  return function () {
    return a.slice(s, e);
  };
}

exports.fromArray = function(s) {
  return (new Uint8Array(s)).buffer;
}

exports.fromIntArray = function(s) {
  return (new Uint8Array(s)).buffer;
}

exports.fromString = function(s) {
  var l = s.length;
  var ab = new ArrayBuffer(l * 2);
  var a = new Uint16Array(ab);
  for (var i = 0; i < l; i++)
    a[i] = s.charCodeAt(i);
  return a.buffer;
}
