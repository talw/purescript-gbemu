"use strict";

// module Utils

//exports.toHexStr=function(n) { return n.toString(16).toUpperCase(); }

exports.toHexStr=function(padding) {
  return function(d) {
      var hex = d.toString(16).toUpperCase();

      while (hex.length < padding) {
        hex = "0" + hex;
      }

      return hex;
  }
}

exports.fromHexStr=function(str) {
  return parseInt(str, 16);
}
