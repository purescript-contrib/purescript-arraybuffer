"use strict";

// module Data.ArrayBuffer.DataView


exports.whole = function whole (b) {
    return new DataView(b);
};

exports.remainderImpl = function remainderImpl (b,i) {
    return new DataView(b,i);
};

exports.partImpl = function partImpl (b,i,j) {
    return new DataView(b,i,j);
};

exports.buffer = function buffer (v) {
    return v.buffer;
};

exports.byteOffset = function byteOffset (v) {
    return v.byteOffset;
};

exports.byteLength = function byteLength (v) {
    return v.byteLength;
};

exports.getterImpl = function getterImpl (data, v, o) {
    return ((o + data.bytesPerValue) >>> 0) <= v.byteLength
        ? data.just (v[data.functionName].call(v,o,data.endian))
        : data.nothing;
};

exports.setterImpl = function setterImpl (s,e,v,n,o) {
    var f = v[s];
    f.call(v,o,n,e);
};
