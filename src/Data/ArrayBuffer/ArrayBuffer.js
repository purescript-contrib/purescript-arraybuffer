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
