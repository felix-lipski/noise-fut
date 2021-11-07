#!/usr/bin/env python

from PIL import Image
import numpy
from mandelbrot import mandelbrot

m = mandelbrot()

filename = "mandelbrot.png"
width = 800
height = 600
limit = 255
minx = -2.23
miny = -1.15
maxx = 0.83
maxy = 1.15

fut_image = m.main(width, height, limit, minx, miny, maxx, maxy).get()
img = Image.fromarray(fut_image)
img.save(filename)
