'https://github.com/TTIcecube/Neural-network---Flappy-bird-
#include "FB_NeuralMath.bi"
#include "FB_ProccesTimers.bi"

#include "string.bi"
#include "file.bi"

dim shared as ulong ID_counter = 1			'START ID counter
dim shared as ulong graph_xoffset = 1300					'draw location of neural netwerk
dim shared as ulong graph_yoffset = 250
dim shared as ulong graph_size = 50						'draw size of neural netwerk

'const gen_threats = 1 						'not use now
const gen_size = 1000						'1000	nummer of players(bird) = nummer of neural networks(gen)
const gen_mutate_perc_top = 0.05			'0.05	top 0.1= 10% selection to mutate from
const gen_mutate_perc_weight = 0.05 			'0.1	Max how many weight are been modified on mutate 
const gen_mutate_streght_weight = 5			'  5	Max how mach to modify the weights
const gen_mutate_prec_dead_weight = 0.1		'0.1	Max 0.1 = 10%, 0 weights value connection
const gen_mutate_prec_fitnessToKeep = 0.1	'0.1	%fittnes to keep for top list copies 
const gen_mutate_cross_perc = 0.05 			'0.02	chance to cross mutate
const gen_mutate_crossX_perc = 0.05 		'0.05	chance to cross X mutate, second cross mutate avg of 2 
const as boolean do_cross_mutated = true
const as boolean cross_mutated_get_new_ID = false
dim shared as byte select_fit_type = 2 		'0 = best fitness / 1 = last fitness / 2 = avg best en last (best)

'const gen_update_fit_selection_frames = 100	'inteval how many times update the fittset brains

const node_in = 5							'inputs
const node_hidden = 7						'mid layers note count
const layer_hidden = 2 						'nummer of hidden layers (0 = 1)
const node_out = 2							'jump / left or right
const bias = node_in + (node_hidden * (layer_hidden+1)) + node_out - 1 ' do not change
const bias_out = node_in + (node_hidden * (layer_hidden+1)) ' do not change

'pre calculation neural netwerk (do not change)
const weights_in_hidden    = node_in * node_hidden 		'connection from in to first hidden layer
const weights_hidden_layer = (node_hidden * node_hidden) * layer_hidden 	'hidden layer to layer connecions
const weights_hidden_out   = node_hidden * node_out
const weights = weights_in_hidden + weights_hidden_layer + weights_hidden_out -1
if weights >= 255 then print "To many neural network weights": end -1

const as ushort gen_fit_selection_size = (gen_size + 1) * gen_mutate_perc_top
const as ushort gen_fit_mutate_size = (weights + 1) * gen_mutate_perc_weight

declare function round(n as single) as string
declare Function Regulate(Byval MyFps As long,Byref fps As long, reset_ as boolean = false) As long
declare function saveToDisk(byval dataout as ubyte ptr, lengteout as short, fileName as string) as ushort
declare function loadFromDisk(byval datain as ubyte ptr, lengtein as short, fileName as string) as short


type Neural_network Field = 1
	Public:
	as single biases(bias)
	as single biases_add(bias)
	as single weight(weights)
	as long ID
	as double mutated = 0
	as ulong fitness = 0
	as ulong best_fitness = 0
	declare constructor
	as single perc_weight = rnd * gen_mutate_perc_weight '0.2 			'how many weight are been modified on mutate 
	as single streght_weight = rnd * gen_mutate_streght_weight '50		'how mach to to modify the weights
	as single dead_weight = rnd * gen_mutate_prec_dead_weight '0.0		'dead weight connection
	as ushort cels_alive = (1-gen_mutate_prec_dead_weight) * weights +1
end type

type gen
	Public:
	as Neural_network brain(gen_size)
	as Neural_network fit_selection(gen_fit_selection_size) 'gen pointer array value
	as moving_avg gen_ma(gen_fit_selection_size) 'gen pointer array value
	as ulong DeadListOrder(gen_size)
	as ulong DeadListOrderIndex = 0
	as ushort fit_selection_counter = 0
	declare sub gen_draw_brain(a as ushort)
	declare sub gen_calc_brain(a as ushort)
	declare sub select_LongestAlive()
	declare sub select_fittest_sortFast()
	declare sub Update_Best_Fit()
	declare function is_ID_in_selection(s as ushort) as ushort
	declare sub mutate(byval n as ushort)
	declare function CrossMutate(a as Neural_network, b as Neural_network) as Neural_network
	declare function CrossMutateX(a as Neural_network, b as Neural_network) as Neural_network
	'declare function saveBrain(byref dataout as any ptr, lengteout as short) as ushort
	'declare function CrossMutateX(a as Neural_network, b as Neural_network) as Neural_network
end type


'-------------------------------------------------Neural netwerk ---------------------------------------------------------
constructor Neural_network
	for c as ushort = 0 to weights
		dim as rnd8_16bit pole
		if pole.give16_rnd > gen_mutate_prec_dead_weight then 'dead_weight
			'weight(c) = pole.give16_rndXrnd * 1
			weight(c) = Activation(pole.give16_rndXrnd * streght_weight) 'gen_mutate_streght_weight 
			if weight(c) > 1 then weight(c) = 1
			if weight(c) < -1 then weight(c) = -1
		else
			weight(c) = 0
		end if
	next
	for c as ushort = 0 to bias
		dim as rnd8_16bit pole
		biases_add(c) = pole.give16_rndXrnd+ pole.give16_rnd()-.5
	next c
	fitness = 0
	'mutated = 0
	best_fitness = 0
	ID = ID_counter
	ID_counter += 1
end constructor

'function gen.saveBrain(byref dataout as any ptr, lengteout as short) as ushort
'	for i as ushort = 0 to lengteout-1
'		print dataout+1
'	next
'	return 0
'end function

sub gen.Update_Best_Fit()
	for n as ushort = 0 to gen_size
		if brain(n).fitness > brain(n).best_fitness then brain(n).best_fitness = brain(n).fitness
	next n
end sub

function gen.CrossMutate(a as Neural_network, b as Neural_network) as Neural_network
	dim as ushort start_noteA = rnd * weights
	dim as ushort start_noteB = rnd * weights
	dim as ushort lenght_note = 1 + (rnd * (weights -1))
	dim as ubyte node_nrA ,node_nrB
	dim tmp as Neural_network = a
	
	for i as ubyte = 0 to lenght_note
		node_nrA = (start_noteA + i) mod (weights + 1)
		node_nrB = (start_noteB + i) mod (weights + 1)
		tmp.weight(node_nrA) = b.weight(node_nrB)
		tmp.biases_add(node_nrA) = b.biases_add(node_nrB)
	next i
	
	tmp.mutated += 1 
	if cross_mutated_get_new_ID = true then
		tmp.ID = ID_counter
		ID_counter += 1
	end if
	tmp.best_fitness = 0
	dim as rnd8_16bit pole
	tmp.fitness =0' *= gen_mutate_prec_fitnessToKeep
	tmp.perc_weight = pole.give16_rnd() * gen_mutate_perc_weight '0.2 			'how many weight are been modified on mutate 
	tmp.streght_weight = (pole.out8_a/255) * gen_mutate_streght_weight '50		'how mach to to modify the weights
	tmp.dead_weight = (pole.out8_b/255) * gen_mutate_prec_dead_weight '0.0		'dead weight connection
	return tmp
end function

function gen.CrossMutateX(a as Neural_network, b as Neural_network) as Neural_network
	dim tmp as Neural_network = a
	
	for i as ubyte = 0 to weights
		tmp.weight(i) = (a.weight(i) + b.weight(i)) / 2
	next i
	
	tmp.mutated += 1000
	if cross_mutated_get_new_ID = true then
		tmp.ID = ID_counter
		ID_counter += 1
	end if
	tmp.best_fitness = 0
	dim as rnd8_16bit pole
	tmp.fitness =0 ' *= gen_mutate_prec_fitnessToKeep
	tmp.perc_weight = pole.give16_rnd() * gen_mutate_perc_weight '0.2 			'how many weight are been modified on mutate 
	tmp.streght_weight = (pole.out8_a/255) * gen_mutate_streght_weight '50		'how mach to to modify the weights
	tmp.dead_weight = (pole.out8_b/255) * gen_mutate_prec_dead_weight '0.0		'dead weight connection
	return tmp
end function

sub gen.mutate(byval n as ushort)
	static as ushort loop_counter :loop_counter += 1
	if fit_selection_counter = 0 then exit sub
	dim as ushort loop_select = loop_counter mod (fit_selection_counter + 1)
	
	brain(n) = fit_selection(loop_select) ' copy from fit list is done
	'brain(n).fitness *= gen_mutate_prec_fitnessToKeep
	brain(n).mutated += 0.000001
	brain(n).fitness = 0
	brain(n).best_fitness = 0
	
	dim as ushort b 
	for a as ushort = 0 to int((weights + 1) * brain(n).perc_weight * rnd) '(gen_fit_mutate_size-1) '
		dim as rnd8_16bit pole
		b = weights * pole.give16_rnd	'select a note
		'if brain(n).weight(b) <> 0  then 'andalso (brain(n).weight(b) + pole.give16_rndXrnd) <> 0
			brain(n).weight(b) += pole.give16_rndXrnd * brain(n).streght_weight  'gen_mutate_streght_weight
			'if brain(n).weight(b) > 1 then brain(n).weight(b) = 1
			'if brain(n).weight(b) < -1 then brain(n).weight(b) = -1
		'end if
	next a
	
	'brain(n).cels_alive = 0
	'for a as ushort = 0 to weights
'		if brain(n).weight(a) <> 0 then brain(n).cels_alive += 1
	'next
end sub

function gen.is_ID_in_selection(s as ushort) as ushort
	dim as ushort count = 0 
	if fit_selection_counter > 0 then
		for i as short = 0 to fit_selection_counter -1
			if fit_selection(i).ID = brain(s).ID then count +=1
		next i
	end if
	return count
end function

sub gen.select_fittest_sortFast()
	static as ulong LookupTable(gen_size, 1) '0= index nr, 1=fitness
	dim as short a, c, b = 0
	fit_selection_counter = 0
	for a = 0 to gen_size
		LookupTable(a, 0) = a
		select case select_fit_type
		case 0
			LookupTable(a, 1) = brain(a).Best_fitness
		case 1
			LookupTable(a, 1) = brain(a).fitness
		case 2
			LookupTable(a, 1) = ( brain(a).Best_fitness + brain(a).fitness ) /2
		end select
	next
	
	for a = gen_size to 0 step -1 'sort only about 1/2 the array, if found top fit selection full, it will leave
		if a > 0 then
			for b = gen_size to (1 + gen_size - a) step -1
				if LookupTable(b, 1) > LookupTable(b-1, 1) then
					swap LookupTable(b, 1), LookupTable(b-1, 1)
					swap LookupTable(b, 0), LookupTable(b-1, 0)
				end if
			next b
		else
			b = gen_size
		end if
		
		c = LookupTable(b, 0)
		if is_ID_in_selection(c) = 0 then
			'print brain(c).fitness, ' procces here
			fit_selection(fit_selection_counter) = brain(c)
			if fit_selection_counter = gen_fit_selection_size then exit for 'found top list
			fit_selection_counter += 1
		end if
	next a
	'sleep
	DeadListOrderIndex = 0
end sub

sub gen.select_LongestAlive() 'normaal not used
	fit_selection_counter = 0
	
	this.fit_selection_counter = 0
	for a as short = gen_size to 0 step -1
		if is_ID_in_selection(DeadListOrder(a)) = 0 then
			fit_selection(fit_selection_counter) = brain(DeadListOrder(a))
			if fit_selection_counter = gen_fit_selection_size then exit for
			fit_selection_counter += 1
		end if
	next a
	DeadListOrderIndex = 0
	'if gen_fit_selection_size <> ( fit_selection_counter ) then print "         Error: "; fit_selection_counter, gen_fit_selection_size: sleep: end
end sub

sub gen.gen_calc_brain(a as ushort)
	Clear brain(a).biases(node_in), 0, (bias-node_in+1) * SizeOf(single) 'clear als outputs
	dim as single output_offset = node_in
	dim as single input_offset = 0
	dim as ushort count_weight = 0
	
	for c as ubyte = 0 to node_hidden - 1
		for b as ubyte = 0 to node_in - 1
			brain(a).biases(output_offset+c) += brain(a).biases(input_offset+b) * brain(a).weight(count_weight)
			count_weight += 1
		next
		brain(a).biases(output_offset+c) += brain(a).biases_add(output_offset+c) '----- add offset to bias
		brain(a).biases(output_offset+c) = Activation(brain(a).biases(output_offset+c))
		'if brain(a).biases(output_offset+c) > 1 then brain(a).biases(output_offset+c) = 1
		'if brain(a).biases(output_offset+c) < -1 then brain(a).biases(output_offset+c) = -1
		'brain(a).biases(output_offset+c) /= node_in
	next
	
	if layer_hidden > 0 then
		for d as ubyte = 0 to layer_hidden -1
			output_offset = node_in + (node_hidden * (d+1)) 
			input_offset = node_in + (node_hidden * d) 
			'print "mid input_offset"; input_offset
			'print "mid output_offset"; output_offset
			for c as ubyte = 0 to node_hidden - 1
				for b as ubyte = 0 to node_hidden - 1
					brain(a).biases(output_offset+c) += brain(a).biases(input_offset+b) * brain(a).weight(count_weight)
					count_weight += 1
				next
			brain(a).biases(output_offset+c) += brain(a).biases_add(output_offset+c) '----- add offset to bias
			brain(a).biases(output_offset+c) = Activation(brain(a).biases(output_offset+c))
			'if brain(a).biases(output_offset+c) > 1 then brain(a).biases(output_offset+c) = 1
			'if brain(a).biases(output_offset+c) < -1 then brain(a).biases(output_offset+c) = -1
			'brain(a).biases(output_offset+c) /= node_hidden
			next
		next
	end if
	
	
	output_offset = node_in + (node_hidden * (layer_hidden+1)) 
	input_offset = node_in + (node_hidden * (layer_hidden))
	for c as ubyte = 0 to node_out - 1
		for b as ubyte = 0 to node_hidden - 1
			brain(a).biases(output_offset+c) += brain(a).biases(input_offset+b) * brain(a).weight(count_weight)
			count_weight += 1
		next
		brain(a).biases(output_offset+c) += brain(a).biases_add(output_offset+c) '----- add offset to bias
		brain(a).biases(output_offset+c) = Activation(brain(a).biases(output_offset+c))
		'if brain(a).biases(output_offset+c) > 1 then brain(a).biases(output_offset+c) = 1
		'if brain(a).biases(output_offset+c) < -1 then brain(a).biases(output_offset+c) = -1
		'brain(a).biases(output_offset+c) /= node_hidden
	next
end sub



sub gen.gen_draw_brain( a as ushort)
	dim as short in_offset = ((node_in - 1) / 2 * graph_size)
	dim as short hidden_offset = ((node_hidden - 1) / 2 * graph_size)
	dim as short out_offset = ((node_out - 1) / 2 * graph_size)
	dim as short x, y, x2, y2, oldx, oldy
	dim as ushort count_bias = 0
	dim as ushort count_weight = 0
	
	'for i as short = 0 to weights
	'	oldx = x: oldy = y
	'	x = 0 + (i * 10)
	'	y = gen_graph_scr_y -20 - (brain(a).weight(i) * 20)
		'line (oldx, oldy)-(x, y), &HFFFFFF
	'next i
	
	draw string (graph_xoffset+hidden_offset, graph_yoffset-in_offset-graph_size), "Nr. " + str(a) + " / ID:" + str(int(brain(a).id)), rgb(255,255,255)
	
	'part weights
	for c as ubyte = 0 to node_hidden - 1
		for b as ubyte = 0 to node_in - 1
			x = graph_xoffset
			y = graph_yoffset + b*graph_size - in_offset
			x2 = graph_xoffset + graph_size*2
			y2 = graph_yoffset + c*graph_size - hidden_offset
			if brain(a).weight(count_weight) > 0 then
				line(x, y)-(x2, y2), rgb(0, abs(brain(a).biases(count_bias)*brain(a).weight(count_weight))*255, 0)
			elseif brain(a).weight(count_weight) = 0 then
				'line(x, y)-(x2, y2), &h000000
			else
				line(x, y)-(x2, y2), rgb( abs(brain(a).biases(count_bias)*brain(a).weight(count_weight))*255, 0, 0)
			end if
			'draw string ((x+x2)/2 - 16,(y+y2)/2-8+b*8),round(brain(a).weight(count_weight)), &hFFFF00
			count_weight += 1
		next
		count_bias += 1
	next
	
	if layer_hidden > 0 then
		for e as ubyte = 0 to layer_hidden-1
			for d as ubyte = 0 to node_hidden - 1
				 for f as ubyte = 0 to node_hidden - 1
					x = graph_xoffset + (4 + (e-1)*2) * graph_size
					y = graph_yoffset + d*graph_size - hidden_offset
					x2 = graph_xoffset + (4 + (e)*2) * graph_size
					y2 = graph_yoffset + f*graph_size - hidden_offset
					if brain(a).weight(count_weight) > 0 then
						line(x, y)-(x2, y2), rgb(0, abs(brain(a).biases(count_bias)*brain(a).weight(count_weight))*255, 0)
					elseif brain(a).weight(count_weight) = 0 then
						'line(x, y)-(x2, y2), &h000000
					else
						line(x, y)-(x2, y2), rgb( abs(brain(a).biases(count_bias)*brain(a).weight(count_weight))*255, 0, 0)
					end if
					'draw string ((x+x2)/2 - 16,(y+y2)/2-8+f*8),round(brain(a).weight(count_weight)), &hFFFF00
					count_weight += 1
				next
			next
			count_bias += 1
		next
	end if
	
	for d as ubyte = 0 to node_hidden - 1
		 for f as ubyte = 0 to node_out - 1
			x = graph_xoffset + (4 + (layer_hidden-1)*2) * graph_size
			y = graph_yoffset + d*graph_size - hidden_offset
			x2 = graph_xoffset + (4 + layer_hidden*2) * graph_size
			y2 = graph_yoffset + f*graph_size - out_offset
			if brain(a).weight(count_weight) > 0 then
				line(x, y)-(x2, y2), rgb(0, abs(brain(a).biases(count_bias)*brain(a).weight(count_weight))*255, 0)
			elseif brain(a).weight(count_weight) = 0 then
				'line(x, y)-(x2, y2), &h000000
			else
				line(x, y)-(x2, y2), rgb( abs(brain(a).biases(count_bias)*brain(a).weight(count_weight))*255, 0, 0)
			end if
			'draw string ((x+x2)/2 - 16,(y+y2)/2-8+f*8),round(brain(a).weight(count_weight)), &hFFFF00
			count_weight += 1
		next
		count_bias += 1
	next
	
	' part bias
	count_bias = 0
	for b as ubyte = 0 to node_in - 1
		x = graph_xoffset
		y = graph_yoffset + b*graph_size - in_offset
		circle (x, y), graph_size/4 , &h0000FF,,,,f
		draw string (x-12,y-4), str(count_bias) + "  " + round(brain(a).biases(count_bias)), &hFFFFFF
		count_bias += 1
	next
	for c as ubyte = 0 to node_hidden - 1
		x = graph_xoffset + graph_size*2
		y = graph_yoffset + c*graph_size - hidden_offset
		circle (x, y), graph_size/4 , &h00FF00
		draw string (x-12,y-4), str(count_bias) + "  " + round(brain(a).biases(count_bias)), &hFFFFFF
		count_bias += 1
	next
	if layer_hidden > 0 then
		for e as ubyte = 0 to layer_hidden -1
			for d as ubyte = 0 to node_hidden - 1
				x = graph_xoffset + (4 + e*2) * graph_size
				y = graph_yoffset + d*graph_size - hidden_offset
				circle (x, y), graph_size/4 , &h00FF00
				draw string (x-12,y-4),str(count_bias) + "  " + round(brain(a).biases(count_bias)), &hFFFFFF
				count_bias += 1
			next
		next
	end if
	for f as ubyte = 0 to node_out - 1
		x = graph_xoffset + (4 + layer_hidden*2) * graph_size
		y = graph_yoffset + f*graph_size - out_offset
		if brain(a).biases(count_bias) > 0 then
			circle (x, y), graph_size/4 , &h00FF00,,,,f
		else
			circle (x, y), graph_size/4 , &hFF0000,,,,f
		end if
		draw string (x-12,y-4), str(count_bias) + "  " + round(brain(a).biases(count_bias)), &hFFFFFFF
		count_bias += 1
	next
end sub

function round(n as single) as string
	return Format(n, "###.##")
end function


'---------------- save and load function ----------------

function saveToDisk(byval dataout as ubyte ptr, lengteout as short, fileName as string) as ushort
	dim as integer fileNum = freefile()
	if open(fileName, for binary, access write, as fileNum) = 0 then
		put #fileNum, , clng(lengteout) 'force long (4 bytes)
		put #fileNum, , dataout[0] ,lengteout
		close #fileNum
	else
		print "error open on save": return -1
	end if
	return 0
end function


function loadFromDisk(byval datain as ubyte ptr, lengtein as short, fileName as string) as short
	dim as long fileSize = FileLen(fileName)
	if FileSize < 4 then print "error lengte header": return -3
	dim as integer fileNum = freefile()
	dim as long getlengte
	if open(fileName, for binary, access read, as fileNum) = 0 then
		get #fileNum, , getlengte
		if getlengte <> lengtein then close #fileNum: print "error lengte": return -2
		get #fileNum, , datain[0], lengtein
		close #fileNum
	else
		print "error open on load": return -1
	end if
	return 0
end function


'----------- list compare functions ----------------------

Type listType
	as short lengte = -1
	as ulong ID(gen_fit_selection_size)
	'as long fitness(gen_fit_selection_size)
end type

type compare
	as moving_avg plot(gen_fit_selection_size)
	as listType top
	as listType old
	as listType active 'active list 
	as listType droped
	as listType added

	declare function Update() as ushort
	declare function IsInList(nummerID as long, list as listType) as long
	declare sub Procces_AddAndDrop
end type

sub compare.Procces_AddAndDrop()
	dim as long found = any
	'dim as short index = 0
	if active.lengte = -1 then active = top 'first run copy top fit list
	
	active.lengte = top.lengte
	
	for a as ushort = 0 to active.lengte
		found = IsInList(active.ID(a), droped)
		if found <> -1 then
			active.ID(a) = added.ID(found)
			'plot(a).ResetBuffer()
		end if
	next
end sub

function compare.IsInList(nummerID as long, list as listType) as long
	if list.lengte < 0 then return -1
	for a as ushort = 0 to list.lengte
		if nummerID = list.ID(a) then
			return a
		end if
	next a
	return -1
end function
function compare.Update() as ushort
	dim as long found = any 
	droped.lengte = -1
	added.lengte = -1
	
	'look for removed id's
	if old.lengte > 0 then
		for a as ushort = 0 to old.lengte
			found = IsInList(old.ID(a), top)
			if found = -1 then
				droped.lengte += 1
				droped.ID(droped.lengte) = old.ID(a)
			end if
		next a
	end if
	
	'look for new id's
	if top.lengte > 0 then
		for a as ushort = 0 to top.lengte
			found = IsInList(top.ID(a), old)
			if found = -1 then
				added.lengte += 1
				added.ID(added.lengte) = top.ID(a)
			end if
		next a
	end if
	
	return false
end function
