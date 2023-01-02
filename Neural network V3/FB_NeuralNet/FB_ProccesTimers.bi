#If Defined(__FB_WIN32__) 'windows timers resolution update
Declare Function HighPrecisionClock Lib "winmm" Alias "timeBeginPeriod"(As Ulong=1) As Long
HighPrecisionClock
#EndIf

type moving_avg
	as double array(255)
	as ubyte arrayIndex
	as ubyte bufferLengte
	as double ReturnSom
	declare sub ResetBuffer()
	declare function MA(in as double, lengte as ubyte) as long
end type


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

'------------Cpu time fuctions

Enum cpu_State
	timer_startup = 0
	timer_loading_img
	LOW_VALUE			'marker loped timers vs single timers
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
	__
	MAX_VALUE = __ -1	'mark highest value
End Enum


type cpu_time
	as double current_timer = timer
	as moving_avg timers(cpu_State.MAX_VALUE)
	as double precent(cpu_State.MAX_VALUE)
	as cpu_State current_index = 0
	declare sub Start(s as cpu_State)
	declare sub UpdateUsedTimePrecent(low as short = cpu_State.LOW_VALUE, high as short = cpu_State.MAX_VALUE)
end type


sub cpu_time.Start(s as cpu_State)
	dim as double this_time = timer
	dim as double passed_time = this_time - current_timer
	if s <> current_index then 
		timers(current_index).ma(passed_time, 254)
		current_index = s
		current_timer = timer 'this_time
	end if
end sub


sub cpu_time.UpdateUsedTimePrecent(low as short = cpu_State.LOW_VALUE, high as short = cpu_State.MAX_VALUE)
	dim as short range = high - low
	dim as double totalTime = 0
	'dim as double count = 0
	dim i as short
	
	for i = low to high
		totalTime += timers(i).ReturnSom
	next i
	
	totalTime /= 100 ' for 100% max
	for i = low to high
		precent(i) = timers(i).ReturnSom / totalTime
		'count += timers(i).ReturnSom / totalTime
	next i
	'print count
end sub

'print cpu_State.LOW_VALUE
'print cpu_State.MAX_VALUE
'sleep
