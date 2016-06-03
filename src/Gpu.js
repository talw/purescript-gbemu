"use strict";

// module Gpu

var ctx = document.getElementById("screen").getContext('2d');
var imgData = ctx.createImageData(160,144);
var imgDataArr = imgData.data;

exports.setCanvasPixelColor=function(cix) {
  return function(palette) {
    return function(x) {
      return function(y) {
        return function() {
          var colorStartIx = cix*4;

          var canvasCurrentIx = y*160*4 + x;
          imgDataArr[canvasCurrentIx] = palette[colorStartIx]; //a
          imgDataArr[canvasCurrentIx+1] = palette[colorStartIx+1]; //r
          imgDataArr[canvasCurrentIx+2] = palette[colorStartIx+2]; //g
          imgDataArr[canvasCurrentIx+3] = palette[colorStartIx+3]; //b
        }
      }
    }
  }
}

//exports.setScreenArr=function(arr) {
  //return function(ctx) {
    //return function() {
      //var imgData = ctx.createImageData(160,144);
      //var id = imgData.data;
      //for(var i=0; i<160*144*4; i++)
      //{
        //id[i] = arr[i];
      //}
      //ctx.putImageData(imgData,0,0);
    //}
  //}
//}

exports.setScreen=function(ctx) {
  return function() {
    ctx.putImageData(imgData,0,0);
  }
}

//exports.setLine=function(arr) {
  //return function(y) {
    //return function(ctx) {
      //return function() {
        //var imgData = ctx.createImageData(160,1);
        //var id = imgData.data;
        //for(var i=0; i<160*4; i++)
        //{
          //id[i] = arr[i];
        //}
        //ctx.putImageData(imgData,0,y);
      //}
    //}
  //}
//}
