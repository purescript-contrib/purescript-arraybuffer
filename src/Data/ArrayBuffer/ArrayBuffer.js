// module Data.ArrayBuffer.ArrayBuffer

export function emptyImpl(s) {
    return new ArrayBuffer(s);
};

export function byteLength(a) {
    return a.byteLength;
}

export function sliceImpl(a, s, e) {
    return a.slice(s, e);
}
