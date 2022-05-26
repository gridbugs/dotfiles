# Docker Notes

## Terminology

A _Dockerfile_ is a text file with instructions for how to build an _image_.
An _image_ is an immutable snapshot of a filesystem.
A _container_ is a running system instantiated from an _image_.

## Minimal Dockerfile with ubuntu image

```Dockerfile
FROM ubuntu:18.04
```

## Build image, naming it "foo"
```
docker build . -t foo
```

## Run container from the "foo" image with interactive prompt
The `-i` specifies that the container should be interactive, and the `-t` allocates a pseudo-TTY for the container.
```
docker run -it foo
```