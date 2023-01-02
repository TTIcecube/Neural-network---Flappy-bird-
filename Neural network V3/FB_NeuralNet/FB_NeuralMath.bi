'-------------------Random type for speed--------------------
type rnd8_16bit
	Union
		as ulong in32_rnd ' = input 32bit
		type
			as ubyte out8_a
			as ubyte out8_b
			as ushort out16_c
		end type
	end Union
	declare constructor
	declare function give16_rndXrnd() as double '-0.99999 to 0.99999
	declare function give16_rnd() as double     '0 to 0.99999
end type

constructor rnd8_16bit
	in32_rnd = rnd * 4294967296
end constructor

function rnd8_16bit.give16_rndXrnd() as double
	return ((out8_a-127.5) * (out8_b-127.5)) / 16256.2599999999
end function 

function rnd8_16bit.give16_rnd() as double
	return out16_c / 65535.9999999999
end function 


'----------------------Math fuctions---------------------------

function Activation( in as double) as double
	dim as double e2w = exp(2 * in)
	return (e2w - 1) / (e2w + 1)
end function
