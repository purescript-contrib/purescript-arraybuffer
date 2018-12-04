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

exports.newUint8ClampedArray = function newUint8ClampedArray (a,mb,mc) {
    if (mc === null) {
        if (mb === null) {
            return new Uint8ClampedArray(a);
        } else {
            return new Uint8ClampedArray(a,mb);
        }
    } else {
        return new Uint8ClampedArray(a,mb,mc);
    }
};


exports.everyImpl = function everyImpl (a,p) {
    return a.every(p);
};
exports.someImpl = function someImpl (a,p) {
    return a.some(p);
};


exports.fillImpl = function fillImpl (a,x,ms,me) {
    if (me === null) {
        if (ms === null) {
            return a.fill(x);
        } else {
            return a.fill(x,ms);
        }
    } else {
        return a.fill(x,ms,me);
    }
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
    if (mo === null) {
        return a.includes(x);
    } else {
        return a.includes(x,mo);
    }
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
    if (off === null) {
        a.set(b);
    } else {
        a.set(b,off);
    }
};


exports.sliceImpl = function sliceImpl (a,ms,me) {
    if (me === null) {
        if (ms === null) {
            return a.slice();
        } else {
            return a.slice(ms);
        }
    } else {
        return a.slice(s,e);
    }
};


exports.sortImpl = function sortImpl (a) {
    a.sort();
};


exports.subArrayImpl = function subArrayImpl (a,s,me) {
    if (me === null) {
        return a.subarray(s);
    } else {
        return a.subarray(s,me);
    }
};


exports.toString = function toString (a) {
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
