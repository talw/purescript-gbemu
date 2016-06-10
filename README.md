
# purescript-gbemu

## Introduction

A Game Boy emulator, implemented in PureScript.

This is a pet project I've decided to undertake in order to learn [PureScript](http://www.purescript.org/), a Haskell-inspired language that is transpiled to JavaScript, and the one I'm placing my bets on to become the solution to [The JavaScript Problem](https://wiki.haskell.org/The_JavaScript_Problem).

## Current state

Currently, I've only tested this on  I5 3570K, Arch Linux, Chromium&Chrome, with Tetris (world) rom. 

Screenshot of current status:

![Screenshot](https://github.com/talw/purescript-gbemu/blob/master/current-state.png)

Things left to do:
- Must have (ordered by priority) 
  - Acceptable performance
    - On my processor, 3570K @ 3.4GHZ, it takes 20 seconds, vs 8 seconds on my Game Boy Advance, to reach the main title screen of Tetris.
    - This is after aggressively modifying my code to perform better, e.g. changing all my memory reads and writes to be on mutable javascript arrays (uggh...), and DEabstracting some code per what I've got from cpu profiles.
    - **This is the main issue that is holding back this project.** 
  - Foreground objects rendering - very simple to implement as the infrastructure already exists 
  - Key Input - very simple to implement as the infrastructure already exists 
  - Sound - no idea what this entails, will figure out when it's relevant. 
- Nice to have
  - Separate MemAccess effect to MemModify (writes) and MemAccess (reads), so that it's more fine grained. But even nicer would be to get rid of this effect and work with immutable data structures like I had before! Only it was a few orders of magnitude slower.
