"use strict";


exports.polyFill = function () {
    var typedArrayTypes =
        [ Int8Array, Uint8Array, Uint8ClampedArray, Int16Array
        , Uint16Array, Int32Array, Uint32Array, Float32Array, Float64Array
        ];

    for (var k in typedArrayTypes) {
        for (var v in Array.prototype) {
            if (Array.prototype.hasOwnProperty(v) && !typedArrayTypes[k].prototype.hasOwnProperty(v))
                typedArrayTypes[k].prototype[v] = Array.prototype[v];
        }
    }
};


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


// Uint8Clamped

exports.newUint8ClampedArray = function newUint8ClampedArray (a) {
    return new Uint8ClampedArray(a);
};
exports.newUint8ClampedArray2 = function newUint8ClampedArray2 (a,b) {
    return new Uint8ClampedArray(a,b);
};
exports.newUint8ClampedArray3 = function newUint8ClampedArray3 (a,b,c) {
    return new Uint8ClampedArray(a,b,c);
};


exports.everyImpl = function everyImpl (a,p) {
    return a.every(p);
};
exports.someImpl = function someImpl (a,p) {
    return a.some(p);
};


exports.copyWithinImpl = function copyWithinImpl (a,t,s) {
    a.copyWithin(t,s);
};
exports.copyWithinImpl3 = function copyWithinImpl (a,t,s,e) {
    a.copyWithin(t,s,e);
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
