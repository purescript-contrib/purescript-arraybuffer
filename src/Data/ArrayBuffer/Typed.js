// module Data.ArrayBuffer.Typed

export function buffer(v) {
    return v.buffer;
}

export function byteOffset(v) {
    return v.byteOffset;
}

export function byteLength(v) {
    return v.byteLength;
}

export function lengthImpl(v) {
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

export const newUint8ClampedArray = newArray(Uint8ClampedArray);
export const newUint32Array = newArray(Uint32Array);
export const newUint16Array = newArray(Uint16Array);
export const newUint8Array = newArray(Uint8Array);
export const newInt32Array = newArray(Int32Array);
export const newInt16Array = newArray(Int16Array);
export const newInt8Array = newArray(Int8Array);
export const newFloat32Array = newArray(Float32Array);
export const newFloat64Array = newArray(Float64Array);

// ------

export function everyImpl(a, p) {
    return a.every(p);
}

export function someImpl(a, p) {
    return a.some(p);
}

export function fillImpl(x, s, e, a) {
    return a.fill(x,s,e);
}

export function mapImpl(a, f) {
    return a.map(f);
}

export function forEachImpl(a, f) {
    a.forEach(f);
}

export function filterImpl(a, p) {
    return a.filter(p);
}

export function includesImpl(a, x, mo) {
    return mo === null ? a.includes(x) : a.includes(x,mo);
}

export function reduceImpl(a, f, i) {
    return a.reduce(f,i);
}

export function reduce1Impl(a, f) {
    return a.reduce(f);
}

export function reduceRightImpl(a, f, i) {
    return a.reduceRight(f,i);
}

export function reduceRight1Impl(a, f) {
    return a.reduceRight(f);
}

export function findImpl(a, f) {
    return a.find(f);
}

export function findIndexImpl(a, f) {
    var r = a.findIndex(f);
    return r === -1 ? null : r;
}

export function indexOfImpl(a, x, mo) {
    var r = mo === null ? a.indexOf(x) : a.indexOf(x,mo);
    return r === -1 ? null : r;
}

export function lastIndexOfImpl(a, x, mo) {
    var r = mo === null ? a.lastIndexOf(x) : a.lastIndexOf(x,mo);
    return r === -1 ? null : r;
}

export function copyWithinImpl(a, t, s, me) {
    if (me === null) {
        a.copyWithin(t,s);
    } else {
        a.copyWithin(t,s,me);
    }
}

export function reverseImpl(a) {
    a.reverse();
}

export function setImpl(a, off, b) {
  a.set(b,off);
}

export function sliceImpl(a, s, e) {
  return a.slice(s,e);
}

export function sortImpl(a) {
    a.sort();
}

export function subArrayImpl(a, s, e) {
    return a.subarray(s, e);
}

export function toStringImpl(a) {
    return a.toString();
}

export function joinImpl(a, s) {
    return a.join(s);
}

export function unsafeAtImpl(a, i) {
    return a[i];
}

export function hasIndexImpl(a, i) {
    return i in a;
}

export function toArrayImpl(a) {
    var l = a.length;
    var ret = new Array(l);
    for (var i = 0; i < l; i++)
        ret[i] = a[i];
    return ret;
}
