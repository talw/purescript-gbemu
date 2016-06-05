"use strict";

// module Loader

exports.fromArrayBuffer=function(arrBuf) {

  var uint8Array = new Uint8Array(arrBuf);
  var array = [];

  for (var i = 0; i < uint8Array.byteLength; i++) {
      array[i] = uint8Array[i];
  }

  return array;
}
