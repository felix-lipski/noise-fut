scf4: noise.png
	sxiv noise.png

noise.png: noise.py noise-visualise.py
	python noise-visualise.py

noise.py: noise.fut
	futhark pyopencl --library noise.fut

scf5: 
	alacritty --hold --command futhark repl

clean:
	rm noise.png
