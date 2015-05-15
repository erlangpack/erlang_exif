# Erlang EXIF Library

## TODO

* Support images with JFIF meta data (just ignoring for now)
* Also need to recognize and skip over other segments (SOF, DHT, COM, SOS)

```
$ hexdump -C img_015.JPG | head
00000000  ff d8 ff e0 00 10 4a 46  49 46 00 01 01 01 00 48  |......JFIF.....H|
00000010  00 48 00 00 ff e1 03 08  45 78 69 66 00 00 4d 4d  |.H......Exif..MM|
```

* File magic numbers: `ff d8 ff e0 s1 s2 4a 46 49 46 00` ...
    - Where `s1 s2` == length of segment (not including `ff e0`)
    - Where `4a 46 49 46 00` is literally null-terminated "JFIF" string

* Add a CommonTest suite and one or more test images
