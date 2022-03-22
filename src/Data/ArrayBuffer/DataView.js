// module Data.ArrayBuffer.DataView


export function whole(b) {
    return new DataView(b);
}

export function remainderImpl(b, i) {
    return new DataView(b,i);
}

export function partImpl(b, i, j) {
    return new DataView(b,i,j);
}

export function buffer(v) {
    return v.buffer;
}

export function byteOffset(v) {
    return v.byteOffset;
}

export function byteLength(v) {
    return v.byteLength;
}

export function getterImpl(data, v, o) {
    return ((o + data.bytesPerValue) >>> 0) <= v.byteLength
        ? v[data.functionName].call(v,o,data.littleEndian)
        : null;
}

export function setterImpl(data, v, o, n) {
    if (((o + data.bytesPerValue) >>> 0) <= v.byteLength) {
        v[data.functionName].call(v,o,n,data.littleEndian);
        return true;
    } else {
        return false;
    }
}
