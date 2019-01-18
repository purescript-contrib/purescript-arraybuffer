"use strict";

// module Data.ArrayBuffer.Typed.Gen

exports.toFloat32 = function toFloat32(s) {
  var r = new Float32Array(1);
  r[0] = s;
  return r[0];
}
