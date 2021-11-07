scf4: noise.png
	sxiv noise.png

noise.png: noise.py noise-visualise.py
	python noise-visualise.py

noise.py: noise.fut
	futhark pyopencl --library noise.fut

clean:
	rm noise.png
