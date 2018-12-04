"use strict";

// module Data.ArrayBuffer.ArrayBuffer

exports.empty = function empty (s) {
    return new ArrayBuffer(s);
};

exports.byteLength = function(a) {
    return a.byteLength;
};

exports.sliceImpl = function(s, e, a) {
    return a.slice(s, e);
};
