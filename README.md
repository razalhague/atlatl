Atlatl
======

Atlatl, The Loud Application Time Limiter

This is a program that helps you keep your use of applications below some threshold of time and/or only during some times of day. Emits warning sounds when a configurable amount of time is left for use on some application that is running. Once time runs out, atlatl kills the application in question.

At the moment, configuration happens through a JSON file.

AFAIK this should work on linux and mac as well, but only Windows has been tested so far.


TODO
====

* Split code into multiple files.
* Carry accumulated time over program termination. At the moment the program assumes you don't shutdown your computer. Restarting the program resets the accumulated time.
* Configuration GUI.
* Different sized icons?
* automate turning the SVG into a PNG
* Kill countermeasures? Currently this program's only defense against termination is that it doesn't provide an exit button.
