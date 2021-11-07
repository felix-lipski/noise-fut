scf4: mandelbrot.png
	sxiv mandelbrot.png

mandelbrot.png: mandelbrot.py mandelbrot-visualise.py
	python mandelbrot-visualise.py

mandelbrot.py: mandelbrot.fut
	futhark pyopencl --library mandelbrot.fut

clean:
	rm mandelbrot.png
