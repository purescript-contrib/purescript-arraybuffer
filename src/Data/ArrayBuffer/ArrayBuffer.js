"use strict";

// module Data.ArrayBuffer.ArrayBuffer

exports.empty = function empty (s) {
    return new ArrayBuffer(s);
};

exports.byteLength = function byteLength (a) {
    return a.byteLength;
};

exports.sliceImpl = function sliceImpl (a, ms, me) {
    if (me === null) {
        if (ms === null) {
            return a.slice();
        } else {
            return a.slice(ms);
        }
    } else {
        return a.slice(ms,me);
    }
};
