"use strict";

// module MemSection

exports.getByIx=function(section) {
  return function(addr) {
    return function() {
      return section[addr];
    }
  }
}

exports.replace=function(val) {
  return function(addr) {
    return function(section) {
      return function() {
        section[addr] = val;
        return section;
      }
    }
  }
}

exports.getNew=function(size) {
  return function(initVal) {
    return new Array(size).fill(initVal);
  }
}

exports.fromIntArray=function(arr) {
  return arr;
}
