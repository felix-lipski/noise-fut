#!/usr/bin/env python

from PIL import Image
import numpy
from noise import noise

m = noise()

filename = "noise.png"
width = 800
height = 600

fut_image = m.main(width, height).get()
img = Image.fromarray(fut_image)
img.save(filename)
