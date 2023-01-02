#include "fbgfx.bi"
#include "string.bi"
#include "file.bi"
#if __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#endif
#If Defined(__FB_WIN32__) 'windows timers resolution update
Declare Function HighPrecisionClock Lib "winmm" Alias "timeBeginPeriod"(As Ulong=1) As Long
HighPrecisionClock
#EndIf

dim shared as boolean graph_vsync_on = true
dim shared as single avg_pipe_global
dim shared as ulong ID_counter = 1			'START ID counter
const graph_scr_x = 1920					'screenres
const graph_scr_y = 1080 
const graph_pix_per_vsync = 6				'wrong name for speed of the pipes per frame
const graph_xoffset = 1300					'draw location of neural netwerk
const graph_yoffset = 250
const graph_size = 50						'draw size of neural netwerk
const graph_set_fps = 60					'
const graph_update_fps = 60					'

const player_count = 750					'nummer of players
const player_size = 20						'display size of player (no collion check for size only center point)
const player_jump_interval = 10				'how many frames pass to flap(jump) again

const pipe_with = 25						'with of the moving pipes
const pipe_interval = 80					'next pipe interval frame - pipe_rnd_interval
const pipe_rnd_interval = 30				'rnd decreded space betwin pipes
const pipe_max = 14							'max nummer of pipes in screen
const pipe_holesize = 0.20					'1 = full screen hole / 0 = no hole

'const gen_threats = 1 						'not use now
const gen_size = player_count				'nummer of players(bird) = nummer of neural networks(gen)
const gen_mutate_perc_top = 0.05			'top 0.1= 10% selection to mutate from
const gen_mutate_perc_weight = 0.1 		'Max how many weight are been modified on mutate 
const gen_mutate_streght_weight = 15			'Max how mach to to modify the weights
const gen_mutate_prec_dead_weight = 0.1		'Max 0.1 = 10%, 0 weights value connection
const gen_mutate_prec_fitnessToKeep = 0.1	'% fittnes to keep for top list copies 
const gen_mutate_cross_perc = 0.05 			'chance to cross mutate
const gen_mutate_crossX_perc = 0.05 		'chance to cross X mutate, second cross mutate avg of 2 
const as boolean do_cross_mutated = true
const as boolean cross_mutated_get_new_ID = false
dim shared as byte select_fit_type = 1 		'0 = best fitness / 1 = last fitness / 2 = avg best en last

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

type moving_avg
	as double array(255)
	as ubyte arrayIndex
	as ubyte bufferLengte
	as double ReturnSom
	declare sub ResetBuffer()
	declare function MA(in as double, lengte as ubyte) as long
end type
dim Main_fitness as moving_avg
'dim cels as moving_avg

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

dim list as compare

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


Enum MyState
	timer_startup = 0
	timer_stats
	timer_neural
	timer_collision
	timer_render
	timer_renderPlot
	timer_renderback
	timer_renderPipes
	timer_renderBrain
	timer_renderList
	timer_order_ids
	timer_mutate
	timer_screensync
	timer_selectfit
End Enum
type cpu_time
	as double current_timer = timer
	as moving_avg timers(19)
	as MyState current_index = 0
	declare sub Start(s as MyState)
end type
sub cpu_time.Start(s as MyState)
	dim as double this_time = timer
	dim as double passed_time = this_time - current_timer
	if s <> current_index then 
		timers(current_index).ma(passed_time, 254)
		current_index = s
		current_timer = timer 'this_time
	end if
end sub
dim cputime as cpu_time
cputime.Start(timer_startup)

function nummerToRGB(i as longint) as long
	i = i * 647328+125
	return rgb(i mod 266, (i\256) mod 256, (i\65536) mod 256)
end function 

type Neural_network Field = 1
	Public:
	as single bias(bias)
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
	declare function saveBrain(byref dataout as any ptr, lengteout as short) as ushort
	'declare function CrossMutateX(a as Neural_network, b as Neural_network) as Neural_network
end type
dim shared ai as gen

type bird
	Public:
	as single x, y
	as single old_x, old_y
	as single vel_y 'vel_x
	as single next_top_pipe_height
	as single next_bottom_pipe_height
	as single next_pipe_distance
	as single next2_top_pipe_height
	as single next2_bottom_pipe_height
	as long jump_timeout = 0
	as byte state = 1 '0 = ready, -1 = dead, 1 = alive
	declare sub jump()
	declare sub go_left()
	declare sub go_right()
	declare sub process()
	declare sub respawn()
	declare constructor
end type
type pipe_hole
	Public:
	as short x
	as short old_x = graph_scr_y
	as single top
	as single old_top
	as longint active_time = 0
	as longint pipe_id
end type
type flappy_game
	Public:
	dim as pipe_hole pipes(pipe_max)
	as longint count_pipe = 0
	as double start_time = 0
	declare sub draw_pipes()
	declare sub process_timers()
	declare sub add_pipe()
	declare sub remove_pipe()
	declare function check_player_collision(p as bird) as boolean
end type
dim game as flappy_game

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

randomize timer, 2
screenres graph_scr_x, graph_scr_y, 32, , 0
Dim stats_image As Any Ptr = ImageCreate( graph_scr_x, graph_scr_y, RGB(0, 0, 0) )
dim shared as short pipe_count = 0
dim shared as longint graph_frame_counter = 0 
dim as short stats_counter = 0
dim as long fps
dim as string key
dim as bird player(player_count)
dim as ushort OneAliveIndex = 0
dim as double run_time = timer
dim as ushort run_time_count = 0
dim as long run_counter = 0
dim as byte disable_select_fit = 0
dim as byte disable_timer_top_disp = 0
dim as long found = any 
dim as ulong highest_fitness_alive, old_highest_fitness_alive
dim as boolean load_on_select = false

for tmp as ushort = 0 to 249 ' pre-render some pipes movement
	graph_frame_counter += 1
	game.process_timers()
next

'print sizeof(Neural_network)
'sleep

do
	locate 1,1 
	cputime.Start(timer_stats)
	dim as short stats_buffer(player_count)
	old_highest_fitness_alive = highest_fitness_alive
	highest_fitness_alive = 0
	stats_counter = 0
	for i as ushort = 0 to gen_size ' count the players alive
		if player(i).state > 0 then
			if ai.brain(i).fitness > highest_fitness_alive then highest_fitness_alive = ai.brain(i).fitness: OneAliveIndex = i
			stats_counter += 1
		end if
	next i
	
	graph_frame_counter += 1 'to next frame
	if stats_counter = 0 then run_counter += 1
	avg_pipe_global = ((game.pipes(0).top + pipe_holesize) + game.pipes(0).top) / 2
	game.process_timers()
	
	cputime.Start(timer_selectfit)
	if stats_counter = 0 then '*respawn all becouse of all dead fit-list first then random fitlist pick with mutation
		game.remove_pipe()
		game.remove_pipe()
		game.remove_pipe()
		'game.remove_pipe()
		
		ai.Update_Best_Fit()
		'ai.select_LongestAlive()
		if disable_select_fit = 0 then ai.select_fittest_sortFast()
	end if
	
	
	cputime.Start(timer_mutate)
	if stats_counter = 0 then '*respawn all becouse of all dead fit-list first then random fitlist pick with mutation
		list.old = list.top
		for i as short = 0 to gen_size 'one less for randow neiuwe
			player(i).respawn
			'game.check_player_collision(player(i))
			if i <= ai.fit_selection_counter then 'first fit to 0 - selection
				ai.brain(i) = ai.fit_selection(i)
				ai.brain(i).fitness *= gen_mutate_prec_fitnessToKeep
				'ai.brain(i).fitness = 0
				list.top.ID(i) = ai.fit_selection(i).ID 'copy id list to list type
			else
				if do_cross_mutated = true then
					dim as rnd8_16bit pole
					if pole.give16_rnd() < gen_mutate_cross_perc then 'cross mutate here
						ai.brain(i) = ai.CrossMutate( ai.fit_selection((pole.out8_a/255.000001) * ai.fit_selection_counter), ai.fit_selection((pole.out8_b/255.000001) * ai.fit_selection_counter) )
					'	ai.brain(i) = ai.CrossMutate( ai.fit_selection(rnd * ai.fit_selection_counter), ai.fit_selection(rnd * ai.fit_selection_counter) )
					else 'normal mutate here
						dim as rnd8_16bit pole
						if pole.give16_rnd() < gen_mutate_crossX_perc then
							ai.brain(i) = ai.CrossMutateX( ai.fit_selection((pole.out8_a/255.000001) * ai.fit_selection_counter), ai.fit_selection((pole.out8_b/255.000001) * ai.fit_selection_counter) )
						else
							if i > (gen_size - gen_fit_mutate_size) then
								ai.brain(i).constructor
							else
								ai.mutate(i)
							end if
						end if
					end if
				else
					ai.mutate(i)
				end if
			end if
		next i
		
		if load_on_select = true then 
			var returnvalue = loadFromDisk(cast(Ubyte ptr, @ai.brain(gen_size)), sizeof(Neural_network), "test.raw")
			if returnvalue <> 0 then sleep
			ai.brain(gen_size).ID = 1
			ai.brain(gen_size).mutated = 0
			'ai.fit_selection(0).fitness = 900000
			'print "Loaded", returnvalue
			'sleep
			load_on_select = false
		end if
		
		list.top.lengte = ai.fit_selection_counter
		list.Update()
		list.Procces_AddAndDrop()
	end if
	
	
	'------------------------------------- test must go in part 2 not all cicles
	cputime.Start(timer_order_ids)
	if multikey(SC_CONTROL) then ' or List.droped.lengte = 0 then
		color RGB(255,255,255)
		print " Top New id's, green has added"
		for i as ushort = 0 to list.top.lengte
			if List.IsInList(List.top.ID(i), List.added) <> -1 then color rgb(0,255,0) else color RGB(255,255,255)
			if List.IsInList(List.top.ID(i), List.droped) <> -1 then color rgb(255,0,0) 'dubbel check please remove later
			print list.top.ID(i),
		next i
		print
		color RGB(255,255,255)
		print " Oude id's, red has been removed"
		for i as ushort = 0 to list.old.lengte 
			if List.IsInList(List.old.ID(i), List.droped) <> -1 then color rgb(255,0,0) else color RGB(255,255,255)
			if List.IsInList(List.old.ID(i), List.added) <> -1 then color rgb(0,255,0) 'dubbel check please remove later
			print list.old.ID(i),
		next i
		print
		print
		'sleep
	
		if list.active.lengte >= 0 then
			print
			color RGB(255,0,255)
			print " active list"
			for i as ushort = 0 to list.active.lengte 
				if list.IsInList(List.active.ID(i), list.added) <> -1 then color rgb(255,0,255) else color RGB(255,255,255)
				print list.active.ID(i),
			next i
			print
			print
		end if
	end if
	
	cputime.Start(timer_renderPlot)
	if stats_counter = 0 then '*part 2 after respan
		
		
		line stats_image, (graph_scr_x - 1, 0)-(graph_scr_x - 1, graph_scr_y - 1), rgb(0, 0, 0)
		if timer - run_time > 60 then 
			run_time_count += 1
			line stats_image, (graph_scr_x - 1, 0)-(graph_scr_x - 1, graph_scr_y - 1), rgb(63, 63, 0)
			draw string stats_image, (graph_scr_x - 2 -len(str(run_time_count )) * 8, 0), str(run_time_count), rgb(63, 63, 0)
			run_time += 60
		end if
		
		for i as ushort = 0 to list.active.lengte
			found = list.IsInList(List.active.ID(i), list.top)
			if found <> -1 then
				Main_fitness.MA(list.plot(i).MA(ai.fit_selection(found).fitness, 50), ai.fit_selection_counter)
				circle stats_image, (graph_scr_x - 1, graph_scr_y - 1 - (list.plot(i).ReturnSom/90)), 1 , nummerToRGB(list.active.ID(i)),,,,f
			end if
		next i
		circle stats_image, (graph_scr_x - 1, graph_scr_y - 1 - (Main_fitness.ReturnSom/90)), 2, rgb(255, 255, 255),,,,f
		
		Put stats_image, (-1, 0), stats_image, PSet
		if disable_select_fit = 1 then print "---------------------disable_select_fit"
	end if
	
	
	cputime.Start(timer_collision)
	if stats_counter <> 0 then
		for i as short = 0 to gen_size
			if player(i).state > 0 then
				if game.check_player_collision(player(i)) then
					player(i).state = -1 'dieded
					ai.DeadListOrder(ai.DeadListOrderIndex) = i
					ai.DeadListOrderIndex += 1
				end if
			end if
		next i
	end if
	
	cputime.Start(timer_neural)
	for i as short = 0 to gen_size
		player(i).process()
		if player(i).state > 0 then
			ai.brain(i).fitness += 1
			ai.brain(i).bias(0)= (player(i).next_top_pipe_height + -player(i).next_bottom_pipe_height) / 2
			'ai.brain(i).bias(1)= 0'player(i).next_bottom_pipe_height
			ai.brain(i).bias(1)= -(player(i).next_pipe_distance- player(i).x) / (pipe_interval * graph_pix_per_vsync)
			ai.brain(i).bias(2)= csng((player(i).x-(graph_scr_x/2)) / (graph_scr_x / 2 ))
			ai.brain(i).bias(3)= player(i).y 'csng((player(i).y-(graph_scr_y/2)) / (graph_scr_y / 2))
			ai.brain(i).bias(4)= player(i).vel_y*25
			'ai.brain(i).bias(4)= 0'player(i).next_pipe_distance / (graph_scr_x)
			'ai.brain(i).bias(6)= 0'player(i).next2_top_pipe_height
			'ai.brain(i).bias(7)= 0'player(i).next2_bottom_pipe_height
			ai.gen_calc_brain(i)
			
			'if (player(i).next_pipe_distance- player(i).x) < 10 then player(i).go_left
			if ai.brain(i).bias(bias_out) > 0 then player(i).jump
			player(i).x -= ai.brain(i).bias(bias_out+1) * 10 - 2.5
			'if ai.brain(i).bias(bias_out+1) < 0 then player(i).go_right': ai.brain(i).Fitness += 1 'extra fitness for moving forward player
			'if ai.brain(i).bias(bias_out+1) < 1 then player(i).go_left': ai.brain(i).Fitness += 1 'extra fitness for moving forward player
			'if ai.brain(i).bias(bias_out+2) < 0 then player(i).go_left': ai.brain(i).Fitness += 1
		end if
	next i
	
	cputime.Start(timer_screensync)
	key = inkey ' clear keyboard buffer
	if lcase(key) = "s" then saveToDisk(cast(Ubyte ptr, @ai.fit_selection(0)), sizeof(Neural_network), "test.raw")
	if lcase(key) = "l" then load_on_select = true
	if lcase(key) = "h" then sleep
	if lcase(key) = "m" then disable_select_fit xor= 1
	if lcase(key) = "t" then disable_timer_top_disp xor= 1
	if key = "0" then select_fit_type = 0
	if key = "1" then select_fit_type = 1
	if key = "2" then select_fit_type = 2
	if multikey(SC_ESCAPE) then exit do
	if key = " " then
		regulate(graph_set_fps, fps, true) 'reset
		if graph_vsync_on then graph_vsync_on = false else graph_vsync_on = true
	end if
	if graph_vsync_on then sleep regulate(graph_set_fps, fps) else var tmp = regulate(graph_set_fps, fps)
	
	screenlock
	cputime.Start(timer_renderback)
	
	if graph_vsync_on or stats_counter = 0 then Put (0, 0), stats_image, PSet
	
	if (graph_frame_counter mod graph_update_fps) = 0 or graph_vsync_on then
		color rgb(255,255,255)
		if disable_select_fit = 1 then print "---------------------disable_select_fit"
		print "Framerate = " & fps & " ( " & format(fps / graph_set_fps, "#") & "x ) Run = " + str(run_counter) + "      "
		color nummerToRGB(ai.Brain(OneAliveIndex).id)
		print "Active player color x" + str(int(player(OneAliveIndex).x)) + " - y" + str(int(player(OneAliveIndex).y*graph_scr_y))
		print "select_fit_type = "; select_fit_type
	end if
	
	cputime.Start(timer_renderList)
	if disable_timer_top_disp = 0 then
		if ai.fit_selection_counter > 0 and (stats_counter = 0 or graph_vsync_on = true) then
			if graph_vsync_on then print "active players: " & stats_counter
			color rgb(255,255,255)
			locate 16,1
			print "List", "Ai.id to copy",, "top fitness", "best fitness","Versie=Cross.mutation","%Mutate","%Strengt","%deadweight", "Alive cels"
			for i as ushort = 0 to ai.fit_selection_counter - 1
				color nummerToRGB(ai.fit_selection(i).id)
				print i, ai.fit_selection(i).id,,
				print     ai.fit_selection(i).fitness,
				print     ai.fit_selection(i).best_fitness,
				print using "######.######"; ai.fit_selection(i).mutated;
				color rgb(0, ai.fit_selection(i).perc_weight*255*20, 0)
				print ,,ai.fit_selection(i).perc_weight,
				color rgb(0, ai.fit_selection(i).streght_weight*127, 0)
				print ai.fit_selection(i).streght_weight,
				color rgb(0, ai.fit_selection(i).dead_weight*255*2, 0)
				print ai.fit_selection(i).dead_weight,
				color rgb(127,127,127)
				print ai.fit_selection(i).cels_alive, ""
			next i
			color RGB(255,255,255)
			print
			print using "timer startup    ###.########"; cputime.timers(timer_startup).ReturnSom
			print using "timer stats      ###.########"; cputime.timers(timer_stats).ReturnSom
			print using "timer neural     ###.########"; cputime.timers(timer_neural).ReturnSom
			print using "timer collision  ###.########"; cputime.timers(timer_collision).ReturnSom
			print using "timer order ids  ###.########"; cputime.timers(timer_order_ids).ReturnSom
			print using "timer render     ###.########"; cputime.timers(timer_render).ReturnSom
			print using "timer renderPlot ###.########"; cputime.timers(timer_renderPlot).ReturnSom
			print using "timer renderBack ###.########"; cputime.timers(timer_renderback).ReturnSom
			print using "timer renderList ###.########"; cputime.timers(timer_renderlist).ReturnSom
			print using "timer renderPipes###.########"; cputime.timers(timer_renderpipes).ReturnSom
			print using "timer renderBrain###.########"; cputime.timers(timer_renderbrain).ReturnSom
			print using "timer selectFit  ###.########"; cputime.timers(timer_selectfit).ReturnSom
			print using "timer mutate     ###.########"; cputime.timers(timer_mutate).ReturnSom
			print using "timer screensync ###.########"; cputime.timers(timer_screensync).ReturnSom
		end if
	end if
	
	cputime.Start(timer_renderPipes)
	game.draw_pipes() 'and calculate move pipes
	cputime.Start(timer_renderBrain)
	if graph_vsync_on then
		ai.gen_draw_brain(OneAliveIndex)
		draw string (graph_scr_x / 2 - 50, graph_scr_y / 2     ),"<Press spacebar> to switch normal or full speed", &hFFFFFF
		draw string (graph_scr_x / 2 - 50, graph_scr_y / 2 + 20),"<Press 0 > for select best fit all time", &h0FFF00
		draw string (graph_scr_x / 2 - 50, graph_scr_y / 2 + 30),"<Press 1 > for select fit last run only", &h0FFF00
		draw string (graph_scr_x / 2 - 50, graph_scr_y / 2 + 40),"<Press 2 > for select avg. of last and best", &h0FFF00
		draw string (graph_scr_x / 2 - 50, graph_scr_y / 2 + 60),"<Press T > hide top list", &h0FFF00
		draw string (graph_scr_x / 2 - 50, graph_scr_y / 2 + 70),"<Press M > disable mutate", &h0FFF00
		draw string (graph_scr_x / 2 - 50, graph_scr_y / 2 + 80),"<Press S > save pos 0 to disk ", &h0FFF00
		draw string (graph_scr_x / 2 - 50, graph_scr_y / 2 + 90),"<Press L > load pos 0 from disk", &h0FFF00
	end if
	
	cputime.Start(timer_render)
	for n as short = player_count to 0 step -1
		if player(n).state >= 0 then
			if graph_vsync_on then 
				circle (player(n).x, player(n).y*graph_scr_y), player_size, nummerToRGB(ai.brain(n).ID), , , , F
				circle (player(n).x, player(n).y*graph_scr_y), player_size-2, &h000000,
				draw string (player(n).x-8, player(n).y*graph_scr_y-4),str(int(ai.brain(n).mutated)), rgb(0,0,0)
				circle (player(n).x, player(n).y*graph_scr_y), player_size, &hFFFFFF,
			end if
		end if
	next
	if graph_vsync_on then ' draw cross on fitest
		line (player(OneAliveIndex).x-player_size*2, player(OneAliveIndex).y*graph_scr_y)-step(player_size*4, 0), RGB(255,255,255)
		line (player(OneAliveIndex).x, player(OneAliveIndex).y*graph_scr_y-player_size*2)-step(0, player_size*4), RGB(255,255,255)
	end if
	'locate 1,50: print graph_frame_counter
	screenunlock
loop

ImageDestroy stats_image
sleep
end


sub bird.go_left()
	'if state = 0 then state = 1
	x -=5
	if x < 0 then x = 0
end sub

sub bird.go_right()
	'if state = 0 then state = 1
	x +=10
	if x > graph_scr_x-1 then x = graph_scr_x - 1
end sub

sub bird.jump()
	'if state = 0 then state = 1
	if (graph_frame_counter - jump_timeout) > player_jump_interval then
		vel_y /= 2.5
		vel_y -= 0.015
		jump_timeout = graph_frame_counter
	end if
end sub

sub bird.process()
	if state = 1 then
		'Gravity
		old_x = x
		old_y = y
		vel_y += 0.00075
		y += vel_y
	end if
end sub

sub bird.respawn()
	x = 200+rnd*50
	'y = rnd*.8 + 0.1
	y = avg_pipe_global+ ((rnd-.5) / 20)
	state = 1
	vel_y = 0
end sub

constructor bird
	x = 200+rnd*50
	y = rnd*.6 + 0.2
	state = 1
end constructor

sub flappy_game.process_timers()
	'print (graph_frame_counter - start_time)
	if (graph_frame_counter - start_time) > pipe_interval then start_time += (graph_frame_counter - start_time)-rnd*  pipe_rnd_interval: add_pipe()
end sub

sub flappy_game.remove_pipe()
	if pipe_count > 0 then
		'beep
		for i as ubyte = 0 to pipe_count-1
			pipes(i) = pipes(i + 1)
		next i
		'pipes(0).active_time = 0
		'pipes(pipe_count-1).active_time = 0
		pipe_count -=1
	end if
end sub

sub flappy_game.add_pipe()
	if pipe_count < pipe_max then
		dim as single pipe_rnd 
		pipe_count += 1
		for i as ubyte = 0 to pipe_max
			if pipes(i).active_time = 0 then
				count_pipe +=1
				pipes(i).pipe_id = count_pipe
				pipes(i).active_time = graph_frame_counter
				pipe_rnd = rnd * (1 - pipe_holesize )
				pipes(i).top = pipe_rnd
				pipes(i).x = graph_scr_x - (graph_frame_counter - pipes(i).active_time) * graph_pix_per_vsync
				pipes(i).old_x = pipes(i).x
				exit sub
			end if
		next i
	else
		'beep
	end if
end sub

sub flappy_game.draw_pipes()
	dim as longint t = graph_frame_counter
	if pipes(0).old_x < -pipe_with then remove_pipe()
	
	for i as ubyte = 0 to pipe_count-1
		if pipes(i).active_time <> 0 then
			pipes(i).old_x = pipes(i).x
			pipes(i).x = graph_scr_x - (t - pipes(i).active_time) * graph_pix_per_vsync
			if graph_vsync_on then
				line (pipes(i).x, 0)-(pipes(i).x + pipe_with, graph_scr_y*pipes(i).top), &hAAAAAA, BF
				line (pipes(i).x, graph_scr_y * (pipes(i).top + pipe_holesize))-(pipes(i).x + pipe_with, graph_scr_y-1), &h888888, BF
				line (pipes(i).x, 0)-(pipes(i).x + pipe_with, graph_scr_y*pipes(i).top), &h000000, B 'F
				line (pipes(i).x, graph_scr_y * (pipes(i).top + pipe_holesize))-(pipes(i).x + pipe_with, graph_scr_y-1), &h000000, B 'F
				'draw string (pipes(i).old_x+ pipe_with/2-4, 8), str(pipes(i).pipe_id)&"/"&i, &h000000
			end if
		end if
	next i
end sub

function flappy_game.check_player_collision(p as bird) as boolean
	dim as short x1 = any
	dim as short x2 = any
	dim as short y1 = any
	dim as short y2 = any
	dim as short py = any
	dim as short last_x2 = 0
	
	if p.y > 1 or p.y < 0 then return true
	if p.x <= 0 then return true
	if pipe_count <= 0 or pipe_count > pipe_max then beep: screen 0: print pipe_count:end
	
	for i as ubyte = 0 to pipe_count-1
		x1 = pipes(i).x
		x2 = (pipes(i).x + pipe_with)
		
		if x1 < p.x andalso x2 > p.x then
			y1 = graph_scr_y * pipes(i).top
			y2 = graph_scr_y * (pipes(i).top + pipe_holesize)
			py = p.y * graph_scr_y
			if y1 < py andalso y2 > py then
				'line (x1, y1)-(x2, y2), &h00FF00, bf
			else
				return true
			end if
		end if
		
		if last_x2 < x2 andalso x2 > p.x then
			'look for next colision height
			last_x2 = x2
			
			y1 = graph_scr_y * pipes(i).top
			y2 = graph_scr_y * (pipes(i).top + pipe_holesize)
			'line (0, y1+1)-(x2, y1+1), &hff0000
			'line (0, y2-1)-(x2, y2-1), &hff0000
			
			p.next_top_pipe_height = p.y - pipes(i).top
			p.next_bottom_pipe_height = -(p.y - (pipes(i).top + pipe_holesize))
			p.next_pipe_distance = x1
			p.next2_top_pipe_height = p.y - pipes(i+1).top
			p.next2_bottom_pipe_height = -(p.y - (pipes(i+1).top + pipe_holesize))
			'draw string (x2, y1-8),"1 = " & p.next_top_pipe_height
			'draw string (x2, y2),"2 = " & p.next_bottom_pipe_height
			
			exit for ' need pipe do not need checks now
		end if
	next i
end function


Function Regulate(Byval MyFps As long,Byref fps As long, reset_ as boolean = false) As long
	Static As Double timervalue, _LastSleepTime, t3, frames
	if reset_  then timervalue = 0: _LastSleepTime = 0: t3 = 0: frames = 0: fps = 0
	frames += 1
	If (Timer-t3) >= 1 Then t3 = Timer: fps = frames: frames = 0
	dim as double sleeptime=_LastSleepTime + ((1 / myfps) - Timer + timervalue) * 1000
	If sleeptime<1 Then sleeptime = 1
	_LastSleepTime = sleeptime
	timervalue = Timer
	Return sleeptime
End Function


'-------------------------------------------------Neural netwerk ---------------------------------------------------------
function gen.saveBrain(byref dataout as any ptr, lengteout as short) as ushort
	for i as ushort = 0 to lengteout-1
		print dataout+1
	next
	return 0
end function

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
	for a as ushort = 0 to (weights + 1) * brain(n).perc_weight '(gen_fit_mutate_size-1) '
		dim as rnd8_16bit pole
		b = weights * pole.give16_rnd	'select a note
		'if brain(n).weight(b) <> 0  then 'andalso (brain(n).weight(b) + pole.give16_rndXrnd) <> 0
			brain(n).weight(b) += pole.give16_rndXrnd * brain(n).streght_weight  'gen_mutate_streght_weight
			if brain(n).weight(b) > 1 then brain(n).weight(b) = 1
			if brain(n).weight(b) < -1 then brain(n).weight(b) = -1
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
	static as ulong LookupTable(gen_size, 1) '0= index, 1=fitness
	dim as ushort a, c, b = 0
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
	
	for a = gen_size to 0 step -1 'sort only 1/2 the array
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
			if fit_selection_counter = gen_fit_selection_size then exit for
			fit_selection_counter += 1
		end if
	next a
	'sleep
	DeadListOrderIndex = 0
end sub

sub gen.select_LongestAlive() 'normaal not used
	fit_selection_counter = 0
	
	this.fit_selection_counter = 0
	for a as ushort = gen_size to 0 step -1
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
	Clear brain(a).bias(node_in), 0, (bias-node_in+1) * SizeOf(single) 'clear als outputs
	dim as single output_offset = node_in
	dim as single input_offset = 0
	dim as ushort count_weight = 0
	
	for c as ubyte = 0 to node_hidden - 1
		for b as ubyte = 0 to node_in - 1
			brain(a).bias(output_offset+c) += brain(a).bias(input_offset+b) * brain(a).weight(count_weight)
			count_weight += 1
		next
		if brain(a).bias(output_offset+c) > 1 then brain(a).bias(output_offset+c) = 1
		if brain(a).bias(output_offset+c) < -1 then brain(a).bias(output_offset+c) = -1
		'brain(a).bias(output_offset+c) /= node_in
	next
	
	if layer_hidden > 0 then
		for d as ubyte = 0 to layer_hidden -1
			output_offset = node_in + (node_hidden * (d+1)) 
			input_offset = node_in + (node_hidden * d) 
			'print "mid input_offset"; input_offset
			'print "mid output_offset"; output_offset
			for c as ubyte = 0 to node_hidden - 1
				for b as ubyte = 0 to node_hidden - 1
					brain(a).bias(output_offset+c) += brain(a).bias(input_offset+b) * brain(a).weight(count_weight)
					count_weight += 1
				next
			if brain(a).bias(output_offset+c) > 1 then brain(a).bias(output_offset+c) = 1
			if brain(a).bias(output_offset+c) < -1 then brain(a).bias(output_offset+c) = -1
				'brain(a).bias(output_offset+c) /= node_hidden
			next
		next
	end if
	
	
	output_offset = node_in + (node_hidden * (layer_hidden+1)) 
	input_offset = node_in + (node_hidden * (layer_hidden))
	for c as ubyte = 0 to node_out - 1
		for b as ubyte = 0 to node_hidden - 1
			brain(a).bias(output_offset+c) += brain(a).bias(input_offset+b) * brain(a).weight(count_weight)
			count_weight += 1
		next
		if brain(a).bias(output_offset+c) > 1 then brain(a).bias(output_offset+c) = 1
		if brain(a).bias(output_offset+c) < -1 then brain(a).bias(output_offset+c) = -1
		'brain(a).bias(output_offset+c) /= node_hidden
	next
end sub

constructor Neural_network
	for c as ushort = 0 to weights
		dim as rnd8_16bit pole
		if pole.give16_rnd > gen_mutate_prec_dead_weight then 'dead_weight
			'weight(c) = pole.give16_rndXrnd * 1
			weight(c) = pole.give16_rndXrnd * streght_weight 'gen_mutate_streght_weight 
			if weight(c) > 1 then weight(c) = 1
			if weight(c) < -1 then weight(c) = -1
		else
			weight(c) = 0
		end if
	next
	fitness = 0
	'mutated = 0
	best_fitness = 0
	ID = ID_counter
	ID_counter += 1
end constructor

sub gen.gen_draw_brain( a as ushort)
	dim as short in_offset = ((node_in - 1) / 2 * graph_size)
	dim as short hidden_offset = ((node_hidden - 1) / 2 * graph_size)
	dim as short out_offset = ((node_out - 1) / 2 * graph_size)
	dim as short x, y, x2, y2, oldx, oldy
	dim as ushort count_bias = 0
	dim as ushort count_weight = 0
	
	for i as short = 0 to weights
		oldx = x: oldy = y
		x = 0 + (i * 10)
		y = graph_scr_y -20 - (brain(a).weight(i) * 20)
		line (oldx, oldy)-(x, y), &HFFFFFF
	next i
	
	draw string (graph_xoffset+hidden_offset, graph_yoffset-in_offset-graph_size), "Nr. " + str(a) + " / ID:" + str(int(brain(a).id)), rgb(255,255,255)
	
	'part weights
	for c as ubyte = 0 to node_hidden - 1
		for b as ubyte = 0 to node_in - 1
			x = graph_xoffset
			y = graph_yoffset + b*graph_size - in_offset
			x2 = graph_xoffset + graph_size*2
			y2 = graph_yoffset + c*graph_size - hidden_offset
			if brain(a).weight(count_weight) > 0 then
				line(x, y)-(x2, y2), rgb(0, abs(brain(a).bias(count_bias)*brain(a).weight(count_weight))*255, 0)
			elseif brain(a).weight(count_weight) = 0 then
				'line(x, y)-(x2, y2), &h000000
			else
				line(x, y)-(x2, y2), rgb( abs(brain(a).bias(count_bias)*brain(a).weight(count_weight))*255, 0, 0)
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
						line(x, y)-(x2, y2), rgb(0, abs(brain(a).bias(count_bias)*brain(a).weight(count_weight))*255, 0)
					elseif brain(a).weight(count_weight) = 0 then
						'line(x, y)-(x2, y2), &h000000
					else
						line(x, y)-(x2, y2), rgb( abs(brain(a).bias(count_bias)*brain(a).weight(count_weight))*255, 0, 0)
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
				line(x, y)-(x2, y2), rgb(0, abs(brain(a).bias(count_bias)*brain(a).weight(count_weight))*255, 0)
			elseif brain(a).weight(count_weight) = 0 then
				'line(x, y)-(x2, y2), &h000000
			else
				line(x, y)-(x2, y2), rgb( abs(brain(a).bias(count_bias)*brain(a).weight(count_weight))*255, 0, 0)
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
		draw string (x-12,y-4), str(count_bias) + "  " + round(brain(a).bias(count_bias)), &hFFFFFF
		count_bias += 1
	next
	for c as ubyte = 0 to node_hidden - 1
		x = graph_xoffset + graph_size*2
		y = graph_yoffset + c*graph_size - hidden_offset
		circle (x, y), graph_size/4 , &h00FF00
		draw string (x-12,y-4), str(count_bias) + "  " + round(brain(a).bias(count_bias)), &hFFFFFF
		count_bias += 1
	next
	if layer_hidden > 0 then
		for e as ubyte = 0 to layer_hidden -1
			for d as ubyte = 0 to node_hidden - 1
				x = graph_xoffset + (4 + e*2) * graph_size
				y = graph_yoffset + d*graph_size - hidden_offset
				circle (x, y), graph_size/4 , &h00FF00
				draw string (x-12,y-4),str(count_bias) + "  " + round(brain(a).bias(count_bias)), &hFFFFFF
				count_bias += 1
			next
		next
	end if
	for f as ubyte = 0 to node_out - 1
		x = graph_xoffset + (4 + layer_hidden*2) * graph_size
		y = graph_yoffset + f*graph_size - out_offset
		if brain(a).bias(count_bias) > 0 then
			circle (x, y), graph_size/4 , &h00FF00,,,,f
		else
			circle (x, y), graph_size/4 , &hFF0000,,,,f
		end if
		draw string (x-12,y-4), str(count_bias) + "  " + round(brain(a).bias(count_bias)), &hFFFFFFF
		count_bias += 1
	next
end sub

function round(n as single) as string
	return Format(n, "###.##")
end function

constructor rnd8_16bit
	in32_rnd = rnd * 4294967296
end constructor

function rnd8_16bit.give16_rndXrnd() as double
	return ((out8_a-127.5) * (out8_b-127.5)) / 16256.2599999999
end function 

function rnd8_16bit.give16_rnd() as double
	return out16_c / 65535.9999999999
end function 

sub moving_avg.ResetBuffer()
	for i as ubyte = 0 to Bufferlengte
		array(i) = 0
	next i
	Bufferlengte = 0
	arrayIndex = 0
	ReturnSom = 0
end sub

function moving_avg.MA(in as double, lengte as ubyte) as long
	dim as double som
	array(arrayIndex) = in
	arrayIndex += 1 
	if arrayIndex > lengte then arrayIndex = 0
	if BufferLengte < lengte then BufferLengte += 1
	for i as ubyte = 0 to BufferLengte
		som += array(i)
	next i
	ReturnSom = som / BufferLengte
	return ReturnSom
end function

function saveToDisk(byval dataout as ubyte ptr, lengteout as short, fileName as string) as ushort
	dim as integer fileNum = freefile()
	if open(fileName, for binary, access write, as fileNum) = 0 then
		put #fileNum, , clng(lengteout) 'force long (4 bytes)
		put #fileNum, , dataout[0] ,lengteout
		close #fileNum
	else
		print "error open": return -1
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
		print "error open": return -1
	end if
	return 0
end function

