Atlatl
======

Atlatl, The Loud Application Time Limiter

This is a program that runs in your system tray, tracking the time you spend using programs you have chosen. It gives out a warning sound when you've spent too much time on those programs, and eventually kills them if you do not turn them off yourself.

AFAIK this should work on linux and mac as well, but only Windows has been tested so far.


TODO
====

* Measure spent time more accurately. At the moment the program assumes that fetching the information on running processes, playing the sounds, and killing processes do not take a significant amount of time, which might not be true.
* Split code into multiple files.
* Reset point for accumulated time, and time carry over program termination. At the moment the program assumes you shutdown your computer for the night, and only for the night. Restarting the program resets the accumulated time.
* Configuration GUI.
* Different sized icons?
* automate turning the SVG into a PNG
* Kill countermeasures? Currently this program's only defense against termination is that it doesn't provide an exit button.
