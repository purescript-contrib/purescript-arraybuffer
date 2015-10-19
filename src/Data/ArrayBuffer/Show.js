"use strict";

// module Show

exports.showImpl = function(a) {
  return require('util').inspect(a);
}
