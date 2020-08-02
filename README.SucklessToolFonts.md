# Suckless Tool Fonts

It's never 100% clear how to specify a font to suckless tools. Using the scripts here, the incantation
that correctly names terminus (in dwm and st) for openbsd is:

```
make USERCFLAGS=-DUSERFONT="\"\\\"Terminus:style=Regular:size=12:antialias=true:autohint=true\\\"\""```
```

For completeness, I found this to work for st but *not* dwm:
```
make USERCFLAGS=-DUSERFONT="\"\\\"-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-1\\\"\""
```

Finally, here's the command that I use on linux and freebsd:
```
make USERCFLAGS=-DUSERFONT="\"\\\"xos4 Terminus:pixelsize=16:antialias=true:autohint=true\\\"\""
```
