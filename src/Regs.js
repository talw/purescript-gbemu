"use strict";

// module Regs

//Uint8Array

exports.getNewRegs=function() {
  //return new DataView(new ArrayBuffer(14));
  return new Uint8Array(14);
}

exports.setA=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint8(0,val);
      rs[0]=val;
      return rs;
    }
  }
}

exports.a=function(rs) {
  //return rs.getUint8(0);
  return rs[0];
}

exports.setF=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint8(1,val);
      rs[1]=val;
      return rs;
    }
  }
}

exports.f=function(rs) {
  //return rs.getUint8(1);
  return rs[1];
}

exports.setB=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint8(2,val);
      rs[2]=val;
      return rs;
    }
  }
}

exports.b=function(rs) {
  //return rs.getUint8(2);
  return rs[2];
}

exports.setC=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint8(3,val);
      rs[3]=val;
      return rs;
    }
  }
}

exports.c=function(rs) {
  //return rs.getUint8(3);
  return rs[3];
}

exports.setD=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint8(4,val);
      rs[4]=val;
      return rs;
    }
  }
}

exports.d=function(rs) {
  //return rs.getUint8(4);
  return rs[4];
}

exports.setE=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint8(5,val);
      rs[5]=val;
      return rs;
    }
  }
}

exports.e=function(rs) {
  //return rs.getUint8(5);
  return rs[5];
}

exports.setH=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint8(6,val);
      rs[6]=val;
      return rs;
    }
  }
}

exports.h=function(rs) {
  //return rs.getUint8(6);
  return rs[6];
}

exports.setL=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint8(7,val);
      rs[7]=val;
      return rs;
    }
  }
}

exports.l=function(rs) {
  //return rs.getUint8(7);
  return rs[7];
}

exports.setSP=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint16(8,val);
      rs[8] = val >>> 8;
      rs[9] = val;
      return rs;
    }
  }
}

exports.sp=function(rs) {
  //return rs.getUint16(8);
  return (rs[8] << 8) + rs[9];
}

exports.setPC=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint16(10,val);
      rs[10] = val >>> 8;
      rs[11] = val;
      return rs;
    }
  }
}

exports.pc=function(rs) {
  //return rs.getUint16(10);
  return (rs[10] << 8) + rs[11];
}

exports.setBrTkn=function(val) {
  return function(rs) {
    return function() {
      var actualVal;
      if (val)
      {
        //rs.setUint8(12,1);
        rs[12]=1;
      }
      else
      {
        //rs.setUint8(12,0);
        rs[12]=0;
      }
      return rs;
    }
  }
}

exports.brTkn=function(rs) {
  //if (rs.getUint8(12) == 0)
  if (rs[12] == 0)
    return false;
  else
    return true;
}

exports.setAF=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint16(0,val);
      rs[0] = val >>> 8;
      rs[1] = val;
      return rs;
    }
  }
}

exports.af=function(rs) {
  //return rs.getUint16(0);
  return (rs[0] << 8) + rs[1];
}

exports.setBC=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint16(2,val);
      rs[2] = val >>> 8;
      rs[3] = val;
      return rs;
    }
  }
}

exports.bc=function(rs) {
  //return rs.getUint16(2);
  return (rs[2] << 8) + rs[3];
}

exports.setDE=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint16(4,val);
      rs[4] = val >>> 8;
      rs[5] = val;
      return rs;
    }
  }
}

exports.de=function(rs) {
  //return rs.getUint16(4);
  return (rs[4] << 8) + rs[5];
}

exports.setHL=function(val) {
  return function(rs) {
    return function() {
      //rs.setUint16(6,val);
      rs[6] = val >>> 8;
      rs[7] = val;
      return rs;
    }
  }
}

exports.hl=function(rs) {
  //return rs.getUint16(6);
  return (rs[6] << 8) + rs[7];
}














//DataView

//exports.getNewRegs=function() {
  //return new DataView(new ArrayBuffer(14));
  ////return new Uint8Array(14);
//}

//exports.setA=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint8(0,val);
      ////rs[0]=val;
      //return rs;
    //}
  //}
//}

//exports.a=function(rs) {
  //return rs.getUint8(0);
  ////return rs[0];
//}

//exports.setF=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint8(1,val);
      ////rs[1]=val;
      //return rs;
    //}
  //}
//}

//exports.f=function(rs) {
  //return rs.getUint8(1);
  ////return rs[1];
//}

//exports.setB=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint8(2,val);
      ////rs[2]=val;
      //return rs;
    //}
  //}
//}

//exports.b=function(rs) {
  //return rs.getUint8(2);
  ////return rs[2];
//}

//exports.setC=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint8(3,val);
      ////rs[3]=val;
      //return rs;
    //}
  //}
//}

//exports.c=function(rs) {
  //return rs.getUint8(3);
  ////return rs[3];
//}

//exports.setD=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint8(4,val);
      ////rs[4]=val;
      //return rs;
    //}
  //}
//}

//exports.d=function(rs) {
  //return rs.getUint8(4);
  ////return rs[4];
//}

//exports.setE=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint8(5,val);
      ////rs[5]=val;
      //return rs;
    //}
  //}
//}

//exports.e=function(rs) {
  //return rs.getUint8(5);
  ////return rs[5];
//}

//exports.setH=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint8(6,val);
      ////rs[6]=val;
      //return rs;
    //}
  //}
//}

//exports.h=function(rs) {
  //return rs.getUint8(6);
  ////return rs[6];
//}

//exports.setL=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint8(7,val);
      ////rs[7]=val;
      //return rs;
    //}
  //}
//}

//exports.l=function(rs) {
  //return rs.getUint8(7);
  ////return rs[7];
//}

//exports.setSP=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint16(8,val);
      ////rs[8] = val >>> 8;
      ////rs[9] = val;
      //return rs;
    //}
  //}
//}

//exports.sp=function(rs) {
  //return rs.getUint16(8);
  ////return (rs[8] << 8) + rs[9];
//}

//exports.setPC=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint16(10,val);
      ////rs[10] = val >>> 8;
      ////rs[11] = val;
      //return rs;
    //}
  //}
//}

//exports.pc=function(rs) {
  //return rs.getUint16(10);
  ////return (rs[10] << 8) + rs[11];
//}

//exports.setBrTkn=function(val) {
  //return function(rs) {
    //return function() {
      //var actualVal;
      //if (val)
      //{
        //rs.setUint8(12,1);
        ////rs[0]=1;
      //}
      //else
      //{
        //rs.setUint8(12,0);
        ////rs[0]=0;
      //}
      //return rs;
    //}
  //}
//}

//exports.brTkn=function(rs) {
  //if (rs.getUint8(12) == 0)
  ////if (rs[12] == 0)
    //return false;
  //else
    //return true;
//}

//exports.setAF=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint16(0,val);
      ////rs[0] = val >>> 8;
      ////rs[1] = val;
      //return rs;
    //}
  //}
//}

//exports.af=function(rs) {
  //return rs.getUint16(0);
  ////return (rs[0] << 8) + rs[1];
//}

//exports.setBC=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint16(2,val);
      ////rs[2] = val >>> 8;
      ////rs[3] = val;
      //return rs;
    //}
  //}
//}

//exports.bc=function(rs) {
  //return rs.getUint16(2);
  ////return (rs[2] << 8) + rs[3];
//}

//exports.setDE=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint16(4,val);
      ////rs[4] = val >>> 8;
      ////rs[5] = val;
      //return rs;
    //}
  //}
//}

//exports.de=function(rs) {
  //return rs.getUint16(4);
  ////return (rs[4] << 8) + rs[5];
//}

//exports.setHL=function(val) {
  //return function(rs) {
    //return function() {
      //rs.setUint16(6,val);
      ////rs[6] = val >>> 8;
      ////rs[7] = val;
      //return rs;
    //}
  //}
//}

//exports.hl=function(rs) {
  //return rs.getUint16(6);
  ////return (rs[6] << 8) + rs[7];
//}

