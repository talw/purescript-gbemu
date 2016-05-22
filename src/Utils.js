"use strict";

// module Utils

//exports.toHexStr=function(n) { return n.toString(16).toUpperCase(); }

var counter = 0;

exports.incDbgCnt=function(x) {
  counter = counter + x;
  return counter;
}

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
