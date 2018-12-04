"use strict";

// module Show

exports.showViaInspect = function showViaInspect (a) {
  return require('util').inspect(a);
}
