#!/usr/bin/env python

from PIL import Image
import numpy
from noise import noise

m = noise()

filename = "noise.png"
width = 1024
height = 1024

fut_image = m.main(width, height).get()
img = Image.fromarray(fut_image)
img.save(filename)
