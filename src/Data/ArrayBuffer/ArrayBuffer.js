// module Data.ArrayBuffer.ArrayBuffer

exports.emptyImpl = function empty (s) {
    return new ArrayBuffer(s);
};

export function byteLength(a) {
    return a.byteLength;
}

export function sliceImpl(a, s, e) {
    return a.slice(s, e);
}
