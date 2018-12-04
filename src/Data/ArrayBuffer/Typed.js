"use strict";


// Lightweight polyfill for ie - see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#Methods_Polyfill
var typedArrayTypes = [Int8Array, Uint8Array, Uint8ClampedArray, Int16Array,
                       Uint16Array, ​​​Int32Array, Uint32Array, ​​​Float32Array, Float64Array];

for (var k in typedArrayTypes)
    for (var v in Array.prototype)
        if (Array.prototype.hasOwnProperty(v) &&
            !typedArrayTypes[k].prototype.hasOwnProperty(v))
            typedArrayTypes[k].prototype[v] = Array.prototype[v];


// module Data.ArrayBuffer.Typed

exports.buffer = function buffer (v) {
    return v.buffer;
};

exports.byteOffset = function byteOffset (v) {
    return v.byteOffset;
};

exports.byteLength = function byteLength (v) {
    return v.byteLength;
};

exports.lengthImpl = function lemgthImpl (v) {
    return v.length;
};


exports.setImpl = function(ra, off, a) {
  return function() {
    a.set(ra, off);
  };
}

exports.unsafeAtImpl = function(a, i) {
    return a[i];
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
