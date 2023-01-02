'https://github.com/TTIcecube/Neural-network---Flappy-bird-

'#macro DOARGS
'   #cmdline "-s gui"
'#endmacro

#include "FB_NeuralNet\FB_NeuralNet.bi"
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
	Using FB 'Scan code constants are stored in the FB namespace in lang FB
#endif



dim cputime as cpu_time
cputime.Start(timer_startup)
dim shared as boolean graph_vsync_on = true
dim shared as single avg_pipe_global
dim Main_fitness as moving_avg
dim list as compare

const graph_scr_x = 1920					'screenres
const graph_scr_y = 1080 

const graph_pix_per_vsync = 6				'wrong name for speed of the pipes per frame
const graph_set_fps = 60					'
const graph_update_fps = 1					'

const player_count = gen_size				'nummer of players
const player_size = 20						'display size of player (no collion check for size only center point)
const player_jump_interval = 10				'how many frames pass to flap(jump) again

const pipe_with = 25						'with of the moving pipes
const pipe_interval = 50					'next pipe interval frame - pipe_rnd_interval
const pipe_rnd_interval = 20				'rnd decreded space betwin pipes
const pipe_max = 14							'max nummer of pipes in screen
const pipe_holesize = 0.20					'1 = full screen hole / 0 = no hole


function nummerToRGB(i as longint) as long
	i = i * 647328+125
	return rgb(i mod 266, (i\256) mod 256, (i\65536) mod 256)
end function 

type bird
	Public:
	as single x, y
	as single old_x, old_y
	as single vel_y 'vel_y
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


randomize timer, 2
screenres graph_scr_x, graph_scr_y, 32, , 0
dim shared ai as gen
Dim stats_image As Any Ptr = ImageCreate( graph_scr_x, graph_scr_y, RGB(0, 0, 0) )
dim shared as short pipe_count = 0
dim shared as longint graph_frame_counter = 0 
dim as short stats_counter = 0
dim as long fps
dim as string key
dim as bird player(player_count)
dim as ushort OneAliveIndex = 0
dim as double run_time = timer
dim as double timeactive
dim as ushort run_time_count = 0
dim as ushort minuut_time_count = 0
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
	if stats_counter = 0 then '*remove all first pipes at spawn
		game.remove_pipe()
		game.remove_pipe()
		game.remove_pipe()
		game.remove_pipe()
		game.remove_pipe()
		
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
		
		if load_on_select = true then 'load in asked
			var returnvalue = loadFromDisk(cast(Ubyte ptr, @ai.brain(gen_size)), sizeof(Neural_network), "Brain.raw")
			if returnvalue <> 0 then sleep:print "error load size?": end -1
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
		if timer - run_time > 60*minuut_time_count then 
			line stats_image, (graph_scr_x - 1, 0)-(graph_scr_x - 1, graph_scr_y - 1), rgb(63, 63, 0)
			draw string stats_image, (graph_scr_x - 2 -len(str(run_time_count )) * 8, 0), str(run_time_count), rgb(63, 63, 0)
			run_time_count += 1
			minuut_time_count += 1
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
		if player(i).state > 0 then 'set the input to neural network
			ai.brain(i).fitness += 1
			ai.brain(i).biases(0)= (player(i).next_top_pipe_height + -player(i).next_bottom_pipe_height) / 2
			'ai.brain(i).bias(1)= 0'player(i).next_bottom_pipe_height
			ai.brain(i).biases(1)= -.5+(player(i).next_pipe_distance- player(i).x) / ((pipe_interval + pipe_with + pipe_rnd_interval)*10)
			ai.brain(i).biases(2)= ((player(i).x-(graph_scr_x/2)) / (graph_scr_x / 2 ))
			ai.brain(i).biases(3)= player(i).y 'csng((player(i).y-(graph_scr_y/2)) / (graph_scr_y / 2))
			ai.brain(i).biases(4)= - player(i).vel_y*25
			'ai.brain(i).biases(4)= 0'player(i).next_pipe_distance / (graph_scr_x)
			'ai.brain(i).biases(6)= 0'player(i).next2_top_pipe_height
			'ai.brain(i).biases(7)= 0'player(i).next2_bottom_pipe_height
			ai.gen_calc_brain(i)
			
			'if (player(i).next_pipe_distance- player(i).x) < 10 then player(i).go_left
			if ai.brain(i).biases(bias_out) > 0 then player(i).jump
			player(i).x -= ai.brain(i).biases(bias_out+1) * 10 - 2.5
			'if ai.brain(i).bias(bias_out+1) < 0 then player(i).go_right': ai.brain(i).Fitness += 1 'extra fitness for moving forward player
			'if ai.brain(i).bias(bias_out+1) < 1 then player(i).go_left': ai.brain(i).Fitness += 1 'extra fitness for moving forward player
			'if ai.brain(i).bias(bias_out+2) < 0 then player(i).go_left': ai.brain(i).Fitness += 1
		end if
	next i
	
	cputime.Start(timer_screensync)
	key = inkey ' read keyboard buffer
	if lcase(key) = "s" then saveToDisk(cast(Ubyte ptr, @ai.fit_selection(0)), sizeof(Neural_network), "Brain.raw")
	if lcase(key) = "l" then load_on_select = true
	if lcase(key) = "h" then sleep
	if lcase(key) = "m" then disable_select_fit xor= 1
	if lcase(key) = "t" then disable_timer_top_disp xor= 1
	if key = "0" then select_fit_type = 0
	if key = "1" then select_fit_type = 1
	if key = "2" then select_fit_type = 2
	IF key = Chr$(255,133) then screenres graph_scr_x, graph_scr_y, 32, ,1 'F11 key
	if key = Chr$(27) or key = Chr$(255,107) then exit do 'escape key & close windows command
	if key = " " then regulate(graph_set_fps, fps, true): if graph_vsync_on then graph_vsync_on = false else graph_vsync_on = true
	
	if graph_vsync_on then sleep regulate(graph_set_fps, fps) else regulate(graph_set_fps, fps)
	
	screenlock
	cputime.Start(timer_renderback)
	
	if graph_vsync_on or stats_counter = 0 then Put (0, 0), stats_image, PSet
	
	if (graph_frame_counter mod graph_update_fps) = 0 or graph_vsync_on then
		color rgb(255,255,255)
		if disable_select_fit = 1 then print "---------------------disable_select_fit"
		print "Framerate = " & fps & " ( " & format(fps / graph_set_fps, "#") & "x ) Run = " + str(run_counter) + "      "
		timeactive = (timer - run_time)
		print int(timeactive/(60*60)) & ":" & format((int(timeactive/60) mod (3600)), "00") & ":" & format(int(timeactive mod 60), "00"),
		print graph_frame_counter
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
			print "List", "uniek.id",, "last fitness", "best fitness","Versie=Cross.mutation","%Mutate","%Strengt","%deadweight", "Alive cels"
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
			cputime.UpdateUsedTimePrecent()
			color rgb(255,255,255): print using "timer startup    ###.######"; cputime.timers(timer_startup).ReturnSom;			:?, :print using "###.##%"; cputime.precent(timer_startup)
			color rgb(128,128,138): print using "timer stats      ###.######"; cputime.timers(timer_stats).ReturnSom;			:?, :print using "###.##%"; cputime.precent(timer_stats)
			color rgb(255,255,255): print using "timer neural     ###.######"; cputime.timers(timer_neural).ReturnSom;			:?, :print using "###.##%"; cputime.precent(timer_neural)
			color rgb(128,128,138): print using "timer collision  ###.######"; cputime.timers(timer_collision).ReturnSom;		:?, :print using "###.##%"; cputime.precent(timer_collision)
			color rgb(255,255,255): print using "timer order ids  ###.######"; cputime.timers(timer_order_ids).ReturnSom;		:?, :print using "###.##%"; cputime.precent(timer_order_ids)
			color rgb(128,128,138): print using "timer render     ###.######"; cputime.timers(timer_render).ReturnSom;			:?, :print using "###.##%"; cputime.precent(timer_render)
			color rgb(255,255,255): print using "timer renderPlot ###.######"; cputime.timers(timer_renderPlot).ReturnSom;		:?, :print using "###.##%"; cputime.precent(timer_renderPlot)
			color rgb(128,128,138): print using "timer renderBack ###.######"; cputime.timers(timer_renderback).ReturnSom;		:?, :print using "###.##%"; cputime.precent(timer_renderback)
			color rgb(255,255,255): print using "timer renderList ###.######"; cputime.timers(timer_renderlist).ReturnSom;		:?, :print using "###.##%"; cputime.precent(timer_renderlist)
			color rgb(128,128,138): print using "timer renderPipes###.######"; cputime.timers(timer_renderpipes).ReturnSom;		:?, :print using "###.##%"; cputime.precent(timer_renderpipes)
			color rgb(255,255,255): print using "timer renderBrain###.######"; cputime.timers(timer_renderbrain).ReturnSom;		:?, :print using "###.##%"; cputime.precent(timer_renderbrain)
			color rgb(128,128,138): print using "timer selectFit  ###.######"; cputime.timers(timer_selectfit).ReturnSom;		:?, :print using "###.##%"; cputime.precent(timer_selectfit)
			color rgb(255,255,255): print using "timer mutate     ###.######"; cputime.timers(timer_mutate).ReturnSom;			:?, :print using "###.##%"; cputime.precent(timer_mutate)
			color rgb(128,128,138): print using "timer screensync ###.######"; cputime.timers(timer_screensync).ReturnSom;		:?, :print using "###.##%"; cputime.precent(timer_screensync)
			color rgb(255,255,255)
			
			dim as ulong cc, aa
			for aa as short = ai.fit_selection_counter /4-1 to 0 step -1
				cc = 255- ((255 / ai.fit_selection_counter) * aa)
				dim as ulong xx, yy, oldxx = 0, oldyy = (graph_scr_y -200) - (ai.brain(aa).biases(0) * 40)
				for ii as short = 1 to bias
					xx = 0 + (ii * 26)
					draw string (xx,graph_scr_y -250), str(ii) 
					yy = (graph_scr_y -200) - (ai.brain(aa).biases(ii) * 40)
					line (oldxx, oldyy)-(xx, yy), nummerToRGB(ai.brain(aa).id)
					oldxx = xx: oldyy = yy
				next ii
			next aa
			
			
			'draw weights of first 6 best on screen
			'im as ulong cc, aa
			for aa as short = gen_size to 0 step -1
				if ai.brain(OneAliveIndex).id = ai.brain(aa).id then
					cc = 255- ((255 / ai.fit_selection_counter) * aa)
					dim as ulong xx, yy, oldxx = 0, oldyy=(graph_scr_y -100) - (ai.brain(aa).weight(0) * 40)
					for ii as short = 1 to weights
						
						xx = 0 + (ii * 13)
						yy = (graph_scr_y -100) - (ai.brain(aa).weight(ii) * 40)
						line (oldxx, oldyy)-(xx, yy), nummerToRGB(ai.Brain(aa).id)
						oldxx = xx: oldyy = yy
					next ii
				end if
			next aa
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
end 0


sub bird.go_left()
	'if state = 0 then state = 1
	x -=5
	if x < 0 or x > graph_scr_x-1 then x = 0
end sub

sub bird.go_right()
	'if state = 0 then state = 1
	x +=5
	if x > graph_scr_x-1 then x = 200
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



