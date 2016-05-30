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

//exports.str2ab=function(str) {
  //var buf = new ArrayBuffer(str.length); // 2 bytes for each char
  //var bufView = new Uint8Array(buf);
  //for (var i=0, strLen=str.length; i < strLen; i++) {
    //bufView[i] = str.charCodeAt(i);
  //}
  //return exports.fromArrayBuffer buf;
//}


//var XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;

//var byteArray;

//exports.byteArray;
//exports.loadRomJs = function() {
  //var oReq = XMLHttpRequest();
  //oReq.open("GET", "/rom/rom.gb", true);
  //oReq.responseType = "arraybuffer";

  ////NOTE: I prefer to keep things synchronous, for now.

  ////oReq.onload = function (oEvent) {
    ////var arrayBuffer = oReq.response; // Note: not oReq.responseText
    ////if (arrayBuffer) {
      ////byteArray = new Uint8Array(arrayBuffer);
    ////}
  ////};

  //oReq.send(null);
  
  //var arrayBuffer = oReq.response; // Note: not oReq.responseText
  //if (arrayBuffer) {
    //byteArray = new Uint8Array(arrayBuffer);
  //}
  //else {
    //throw new Error('rom acquisition failed');
  //}

  //return byteArray;
//}
