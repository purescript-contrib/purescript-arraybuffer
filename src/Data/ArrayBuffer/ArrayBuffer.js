"use strict";

// module Data.ArrayBuffer.ArrayBuffer

exports.create = function(s) {
  return function () {
    return new ArrayBuffer(s);
  };
};

exports.byteLength = function(a) {
  return a.byteLength;
};

exports.sliceImpl = function(s, e, a) {
  return function () {
    return a.slice(s, e);
  };
};

exports.fromArray = function(s) {
  return (new Uint8Array(s)).buffer;
};

exports.fromIntArray = function(s) {
  return (new Uint8Array(s)).buffer;
};

exports.fromString = function(s) {
  var buf = new ArrayBuffer(s.length*2);
  var bufView = new Uint16Array(buf);
  for (var i=0, strLen=s.length; i<strLen; i++) {
    bufView[i] = s.charCodeAt(i);
  }
 return buf;
};

exports.decodeToStringImpl = function(just, nothing, buffer) {
  try {
    const uintBuffer = new Uint16Array(buffer);
    const reducer = function(accum, point) {
        // use concat instead of es6 syntax for compatibility
        return accum.concat([String.fromCharCode(point)]);
    };
    const points = uintBuffer.reduce(reducer, new Array());
    return just(points.join(""));
  }
  catch (e) {
    return nothing;
  }
};
