"use strict";

// module Gpu

exports.setScreen=function(arr) {
  return function(ctx) {
    return function() {
      var imgData = ctx.createImageData(160,144);
      var id = imgData.data;
      for(var i=0; i<160*144*4; i++)
      {
        id[i] = arr[i];
      }
      ctx.putImageData(imgData,0,0);
    }
  }
}

exports.setLine=function(arr) {
  return function(y) {
    return function(ctx) {
      return function() {
        var imgData = ctx.createImageData(160,1);
        var id = imgData.data;
        for(var i=0; i<160*4; i++)
        {
          id[i] = arr[i];
        }
        ctx.putImageData(imgData,0,y);
      }
    }
  }
}
