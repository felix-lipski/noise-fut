scf4: futhask
	alacritty --hold --command neovim src2/NoiseFut/Raw.hs

# scf4: noise.png
# 	sxiv noise.png

noise.png: noise.py noise-visualise.py
	python noise-visualise.py

noise.py: noise.fut
	futhark pyopencl --library noise.fut

noise.h: noise.fut
	futhark opencl --library noise.fut

futhask: noise.h
	futhask opencl noise.h src2 NoiseFut

scf5: 
	alacritty --hold --command futhark repl

clean:
	rm noise.png
