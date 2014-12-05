%% @author Tanyel Bariser
%% @course MSc Computer Science
%% @deadline Sunday, 12th January, 23:55
%%
%% Program takes temperature and converts from Celsius to Fahrenheit then vice versa.
-module(convert).
-export([start/0, controller/0, temperature_converter/0, display/0]).

%% Takes user input then creates and monitors controller processes.
%% When first controller process dies it traps exit message and creates another.
%% Non numeric user input causes exception error.
start() ->
	io:format("This program converts your number from Celsius to Fahrenheit then vice versa.~n"),
	{ok, [Degrees]} = io:fread("Enter temperature in degrees: ", "~d"),
	process_flag(trap_exit, true),
	register(controller, spawn_link(fun convert:controller/0)),
	controller ! {Degrees, celsius},
	receive
		{'EXIT', From, Reason} ->
			io:format("Controller ~p died with reason ~p.~nRestarting~n" , [From, Reason]),
			register(controller, spawn(fun convert:controller/0)),
			controller ! {Degrees, fahrenheit}
	end.

%% Creates temperature_converter process and sends
%% ConvertToCelsius/Fahrenheit message with Degrees as argument.
controller() ->
	receive
		{Degrees, celsius} ->
			register(temperature_converter, spawn(fun convert:temperature_converter/0)),
			temperature_converter ! {'ConvertToFahrenheit', Degrees},
			controller();
		{Degrees, fahrenheit} ->
			register(temperature_converter, spawn(fun convert:temperature_converter/0)),
			temperature_converter ! {'ConvertToCelsius', Degrees},
			controller();
		die ->
			exit({normal_completion})
	end.

%% Creates display process and sends Temperature message
%% with degrees in Celsius and Fahrenheit as arguments.
temperature_converter() ->
	receive
		{'ConvertToFahrenheit', Celsius} ->
			register(display, spawn(fun convert:display/0)),
			display ! {'Temperature', Celsius, 32+Celsius*9/5};
		{'ConvertToCelsius', Fahrenheit} ->
			register(display, spawn(fun convert:display/0)),
			display ! {'Temperature', (Fahrenheit-32)*5/9, Fahrenheit}
	end.

%% Outputs degrees in Celsius and Fahrenheit.
%% Kills current controller process.
display() ->
	receive
		{'Temperature', Celsius, Fahrenheit} ->
			io:format("~n~p degrees Celsius is ~p degrees Fahrenheit~n~n", [Celsius, Fahrenheit]),
			controller ! die
	end.