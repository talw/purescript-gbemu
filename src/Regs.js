"use strict";

// module Regs

exports.innerSetPC=function(val) {
  return function(rs) {
    return function() {
      rs.pc = val;
      return rs;
    }
  }
}

exports.innerSetSP=function(val) {
  return function(rs) {
    return function() {
      rs.sp = val;
      return rs;
    }
  }
}

exports.innerSetA=function(val) {
  return function(rs) {
    return function() {
      rs.a = val;
      return rs;
    }
  }
}

exports.innerSetB=function(val) {
  return function(rs) {
    return function() {
      rs.b = val;
      return rs;
    }
  }
}

exports.innerSetC=function(val) {
  return function(rs) {
    return function() {
      rs.c = val;
      return rs;
    }
  }
}

exports.innerSetD=function(val) {
  return function(rs) {
    return function() {
      rs.d = val;
      return rs;
    }
  }
}

exports.innerSetE=function(val) {
  return function(rs) {
    return function() {
      rs.e = val;
      return rs;
    }
  }
}

exports.innerSetH=function(val) {
  return function(rs) {
    return function() {
      rs.h = val;
      return rs;
    }
  }
}

exports.innerSetL=function(val) {
  return function(rs) {
    return function() {
      rs.l = val;
      return rs;
    }
  }
}

exports.innerSetF=function(val) {
  return function(rs) {
    return function() {
      rs.f = val;
      return rs;
    }
  }
}

exports.innerSetBrTkn=function(val) {
  return function(rs) {
    return function() {
      rs.brTkn = val;
      return rs;
    }
  }
}
