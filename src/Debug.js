"use strict";

// module Debug

var maxAverageBins=10;
var totals = new Array(maxAverageBins).fill(0);
var counts = new Array(maxAverageBins).fill(0);

var startTime;

exports.startTimer=function() {
  startTime = new Date().getTime();
}

exports.endTimer=function() {
  var endTime = new Date().getTime();
  var time = endTime - startTime;
  return time;
}

exports.recordTime=function(timerIx) {
  return function(target) {
    return function(time) {
      return function() {
        totals[timerIx] += time;
        counts[timerIx] += 1;

        var total = totals[timerIx];
        var count = counts[timerIx];

        if(count == target)
        {
          console.log("avg timer " + timerIx + ": " + total/target);
          totals[timerIx] = 0;
          counts[timerIx] = 0;
        }
      }
    }
  }
}
