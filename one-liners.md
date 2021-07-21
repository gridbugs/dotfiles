## One Liners

Make a gif out of all the png files in a directory:

```
ffmpeg -pattern_type glob -i '*.png' out.gif
```

Make a gif out of all the pngs files in a directory, scaling them up 16 times with nearest-neighbour
interpolation with a framerate of 10 frames per second:
```
ffmpeg -pattern_type glob -i '*.png' -vf 'scale=iw*16:ih*16:flags=neighbor' -r 10 out.gif
```
