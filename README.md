# Neural-network---Flappy-bird---Freebasic
Flappy bird frames with neural network for freebasic

Version 3.0 now have:
- Signoid function to keep values withing -1 to +1 at biases
- Biases now have a added offset value, beter learing
- Weights now have more freedom at values beyond the -1 and +1
- quick save an load function.
- plots baises en weights in graph

Need to do:
- Train / modify baises offset values (Baises_add) to improve learning.



Tested setup:
Windows 10, 64bit
Intel Core i7-7700HQ
NVIDIA GeForce GTX 1050 [MSI] / Intel HD Graphics 630
Freebasic Version 1.09.0 64bit
Compled with, fbc -w all "%f" -gen gcc -Wc -O3 -s console -target win64
