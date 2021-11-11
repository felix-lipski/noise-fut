#!/usr/bin/env python

from PIL import Image
import numpy
from noise import noise

m = noise()

filename = "noise.png"
width = 512
# width = 1024
# width = 2048

fut_image = m.main(width).get()
img = Image.fromarray(fut_image)
img.save(filename)
