"use strict";

// module Regs

exports.innerSetPC=function(rs) {
  return function(val) {
    return function() {
      rs.pc = val;
      return rs;
    }
  }
}

exports.innerSetSP=function(rs) {
  return function(val) {
    return function() {
      rs.sp = val;
      return rs;
    }
  }
}

exports.innerSetA=function(rs) {
  return function(val) {
    return function() {
      rs.a = val;
      return rs;
    }
  }
}

exports.innerSetB=function(rs) {
  return function(val) {
    return function() {
      rs.b = val;
      return rs;
    }
  }
}

exports.innerSetC=function(rs) {
  return function(val) {
    return function() {
      rs.c = val;
      return rs;
    }
  }
}

exports.innerSetD=function(rs) {
  return function(val) {
    return function() {
      rs.d = val;
      return rs;
    }
  }
}

exports.innerSetE=function(rs) {
  return function(val) {
    return function() {
      rs.e = val;
      return rs;
    }
  }
}

exports.innerSetH=function(rs) {
  return function(val) {
    return function() {
      rs.h = val;
      return rs;
    }
  }
}

exports.innerSetL=function(rs) {
  return function(val) {
    return function() {
      rs.l = val;
      return rs;
    }
  }
}

exports.innerSetF=function(rs) {
  return function(val) {
    return function() {
      rs.f = val;
      return rs;
    }
  }
}

exports.innerSetBrTkn=function(rs) {
  return function(val) {
    return function() {
      rs.brTkn = val;
      return rs;
    }
  }
}
