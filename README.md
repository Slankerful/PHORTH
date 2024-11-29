# PHORTH
A repository for my version of Forth for the Raspberry Pi with support for GPIO.

A Brief History
---------------
I came across Forth many moons ago and decided to create a version of it for the BBC Micro.
This was a lot of fun and games on 6502 chip in Basic Assembler.
I then ported it to the Acorn Archimedes, a 32-bit ARM RISC machine.
In this, its latest incarnation, I hsave ported it to the Raspberry Pi, adding such
things as: support for GPIO, some rewriting for efficiency and a lot of changes to bring
it (as close as I could) to comply with the latest FORTH standard. It includes such words
as DEFER and DOES> and, I believe, implements all the core FORTH words. It is, however,
very much a work in progress.

General Notes
-------------
In writing the latest version I have shamelessly borrowed some ideas from JonesForth-arm
(https://github.com/M2IHP13-admin/JonesForth-arm) in particular some of the diagrams used
to explain how Forth works.

The repository also contains asm.fs, an ARM assembler for Forth which can be INCLUDE'd in.
I found this on the internet many moons ago and can no longer find it so, unfortunately,
I cannot give credit where it is due. If anyone knows who wrote it, please let me know.


