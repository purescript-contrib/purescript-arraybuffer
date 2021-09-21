"use strict";

// Lightweight polyfill for IE
// See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#Methods_Polyfill
//
// Can we trust that this FFI code gets called exactly once? I think so, but
// I can't find a reference for that.
{
    // var is not block scoped, but there is no way to scope this block inside
    // a function in such a way that the function will not be dead-code-eliminated.
    // https://github.com/purescript-contrib/purescript-arraybuffer/issues/34
    var _typedArrayTypes =
        [ Int8Array, Uint8Array, Uint8ClampedArray, Int16Array
        , Uint16Array, Int32Array, Uint32Array, Float32Array, Float64Array
        ];

    for (var k in _typedArrayTypes) {
        for (var v in Array.prototype) {
            if (Array.prototype.hasOwnProperty(v) && !_typedArrayTypes[k].prototype.hasOwnProperty(v))
                _typedArrayTypes[k].prototype[v] = Array.prototype[v];
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


// Typed Arrays


function newArray (f) {
  return function newArray_ (a,mb,mc) {
    if (mb === null)
      return new f(a);
    var l = a.byteLength;
    var eb = f.BYTES_PER_ELEMENT;
    var off = Math.min(l, mb>>>0);
    if (mc === null)
      return new f(a,off);
    var len = Math.min((l - off) / eb, mc);
    return new f(a,off,len);
  };
}

exports.newUint8ClampedArray = newArray(Uint8ClampedArray);
exports.newUint32Array = newArray(Uint32Array);
exports.newUint16Array = newArray(Uint16Array);
exports.newUint8Array = newArray(Uint8Array);
exports.newInt32Array = newArray(Int32Array);
exports.newInt16Array = newArray(Int16Array);
exports.newInt8Array = newArray(Int8Array);
exports.newFloat32Array = newArray(Float32Array);
exports.newFloat64Array = newArray(Float64Array);


// ------

exports.everyImpl = function everyImpl (a,p) {
    return a.every(p);
};
exports.someImpl = function someImpl (a,p) {
    return a.some(p);
};


exports.fillImpl = function fillImpl (x, s, e, a) {
    return a.fill(x,s,e);
};


exports.mapImpl = function mapImpl (a,f) {
    return a.map(f);
};

exports.forEachImpl = function forEachImpl (a,f) {
    a.forEach(f);
};

exports.filterImpl = function filterImpl (a,p) {
    return a.filter(p);
};

exports.includesImpl = function includesImpl (a,x,mo) {
    return mo === null ? a.includes(x) : a.includes(x,mo);
};

exports.reduceImpl = function reduceImpl (a,f,i) {
    return a.reduce(f,i);
};
exports.reduce1Impl = function reduce1Impl (a,f) {
    return a.reduce(f);
};
exports.reduceRightImpl = function reduceRightImpl (a,f,i) {
    return a.reduceRight(f,i);
};
exports.reduceRight1Impl = function reduceRight1Impl (a,f) {
    return a.reduceRight(f);
};

exports.findImpl = function findImpl (a,f) {
    return a.find(f);
};

exports.findIndexImpl = function findIndexImpl (a,f) {
    var r = a.findIndex(f);
    return r === -1 ? null : r;
};
exports.indexOfImpl = function indexOfImpl (a,x,mo) {
    var r = mo === null ? a.indexOf(x) : a.indexOf(x,mo);
    return r === -1 ? null : r;
};
exports.lastIndexOfImpl = function lastIndexOfImpl (a,x,mo) {
    var r = mo === null ? a.lastIndexOf(x) : a.lastIndexOf(x,mo);
    return r === -1 ? null : r;
};



exports.copyWithinImpl = function copyWithinImpl (a,t,s,me) {
    if (me === null) {
        a.copyWithin(t,s);
    } else {
        a.copyWithin(t,s,me);
    }
};


exports.reverseImpl = function reverseImpl (a) {
    a.reverse();
};


exports.setImpl = function setImpl (a, off, b) {
  a.set(b,off);
};


exports.sliceImpl = function sliceImpl (a, s, e) {
  return a.slice(s,e);
};

exports.sortImpl = function sortImpl (a) {
    a.sort();
};


exports.subArrayImpl = function subArrayImpl (a, s, e) {
    return a.subarray(s, e);
};


exports.toStringImpl = function toStringImpl (a) {
    return a.toString();
};

exports.joinImpl = function joinImpl (a,s) {
    return a.join(s);
};

exports.unsafeAtImpl = function(a, i) {
    return a[i];
}

exports.hasIndexImpl = function(a, i) {
    return i in a;
}

exports.toArrayImpl = function(a) {
    var l = a.length;
    var ret = new Array(l);
    for (var i = 0; i < l; i++)
        ret[i] = a[i];
    return ret;
}
