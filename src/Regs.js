"use strict";

// module Regs

exports.getNewRegs=function() {
  return new DataView(new ArrayBuffer(14));
}

exports.setA=function(val) {
  return function(rs) {
    return function() {
      rs.setUint8(0,val);
      return rs;
    }
  }
}

exports.a=function(rs) {
  return rs.getUint8(0);
}

exports.setF=function(val) {
  return function(rs) {
    return function() {
      rs.setUint8(1,val);
      return rs;
    }
  }
}

exports.f=function(rs) {
  return rs.getUint8(1);
}

exports.setB=function(val) {
  return function(rs) {
    return function() {
      rs.setUint8(2,val);
      return rs;
    }
  }
}

exports.b=function(rs) {
  return rs.getUint8(2);
}

exports.setC=function(val) {
  return function(rs) {
    return function() {
      rs.setUint8(3,val);
      return rs;
    }
  }
}

exports.c=function(rs) {
  return rs.getUint8(3);
}

exports.setD=function(val) {
  return function(rs) {
    return function() {
      rs.setUint8(4,val);
      return rs;
    }
  }
}

exports.d=function(rs) {
  return rs.getUint8(4);
}

exports.setE=function(val) {
  return function(rs) {
    return function() {
      rs.setUint8(5,val);
      return rs;
    }
  }
}

exports.e=function(rs) {
  return rs.getUint8(5);
}

exports.setH=function(val) {
  return function(rs) {
    return function() {
      rs.setUint8(6,val);
      return rs;
    }
  }
}

exports.h=function(rs) {
  return rs.getUint8(6);
}

exports.setL=function(val) {
  return function(rs) {
    return function() {
      rs.setUint8(7,val);
      return rs;
    }
  }
}

exports.l=function(rs) {
  return rs.getUint8(7);
}

exports.setSP=function(val) {
  return function(rs) {
    return function() {
      rs.setUint16(8,val);
      return rs;
    }
  }
}

exports.sp=function(rs) {
  return rs.getUint16(8);
}

exports.setPC=function(val) {
  return function(rs) {
    return function() {
      rs.setUint16(10,val);
      return rs;
    }
  }
}

exports.pc=function(rs) {
  return rs.getUint16(10);
}

exports.setBrTkn=function(val) {
  return function(rs) {
    return function() {
      var actualVal;
      if (val)
        rs.setUint8(12,1);
      else
        rs.setUint8(12,0);

      return rs;
    }
  }
}

exports.brTkn=function(rs) {
  if (rs.getUint8(12) == 0)
    return false;
  else
    return true;
}

exports.setAF=function(val) {
  return function(rs) {
    return function() {
      rs.setUint16(0,val);
      return rs;
    }
  }
}

exports.af=function(rs) {
  return rs.getUint16(0);
}

exports.setBC=function(val) {
  return function(rs) {
    return function() {
      rs.setUint16(2,val);
      return rs;
    }
  }
}

exports.bc=function(rs) {
  return rs.getUint16(2);
}

exports.setDE=function(val) {
  return function(rs) {
    return function() {
      rs.setUint16(4,val);
      return rs;
    }
  }
}

exports.de=function(rs) {
  return rs.getUint16(4);
}

exports.setHL=function(val) {
  return function(rs) {
    return function() {
      rs.setUint16(6,val);
      return rs;
    }
  }
}

exports.hl=function(rs) {
  return rs.getUint16(6);
}

