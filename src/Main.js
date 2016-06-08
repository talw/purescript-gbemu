"use strict";

// module Main

exports.changeLabel=function(str) {
  return function() {
    document.getElementById("testLabel").innerHTML=str;
  }
}
