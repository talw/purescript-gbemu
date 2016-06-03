"use strict";

// module MainMem

exports.setGpu=function(val) {
  return function(mm) {
    return function() {
      mm.gpu = val;
      return mm;
    }
  }
}

exports.setImeCntDwn=function(mm) {
  return function() {
    mm.imeEnableCnt = 3;
    return mm;
  }
}

exports.setIme=function(val) {
  return function(mm) {
    return function() {
      mm.ime = val;
      return mm;
    }
  }
}

exports.setIntE= function(val) {
  return function(mm) {
    return function() {
      mm.intE = val;
      return mm;
    }
  }
}

exports.setIntF=function(val) {
  return function(mm) {
    return function() {
      mm.intF = val;
      return mm;
    }
  }
}

exports.setVblIntrr=function(val) {
  return function(mm) {
    return function() {
      mm.gpu.vblIntrr = val;
      return mm;
    }
  }
}
