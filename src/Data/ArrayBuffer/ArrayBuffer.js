"use strict";

// module Data.ArrayBuffer.ArrayBuffer

exports.emptyImpl = function empty (s) {
    return new ArrayBuffer(s);
};

exports.byteLength = function byteLength (a) {
    return a.byteLength;
};

exports.sliceImpl = function sliceImpl (a, s, e) {
    return a.slice(s, e);
};
