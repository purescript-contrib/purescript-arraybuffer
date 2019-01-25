"use strict";

// module Data.ArrayBuffer.ArrayBuffer

exports.emptyImpl = function empty (s) {
    return new ArrayBuffer(s);
};

exports.byteLength = function byteLength (a) {
    return a.byteLength;
};

exports.sliceImpl = function sliceImpl (a, ms, me) {
    return me === null ? (ms === null ? a.slice() : a.slice(ms)) : a.slice(ms,me);
};
