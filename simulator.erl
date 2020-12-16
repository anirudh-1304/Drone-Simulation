-module(simulator).
-export([simulate/3,drone_6043/2,drone_5937/2]).

drone_6043(D1,L1) ->                                                      %%% Process for drone_1
    receive
        shutdown ->
            io:format("simulation shutdown for drone_6043~n", []);
        {Lat,Long,Date,Station,S_lat,S_long,Distance} ->
            io:format(L1,"Droneid: ~p is at Lat: ~p Long: ~p Tube Station: ~p at Lat: ~p Long: ~p and Distance: ~pm~n", [6043,Lat,Long,Station,S_lat,S_long,Distance]),
            Traffic= traffic_rand(),
            io:format(D1,"Droneid: ~p Time: ~p Speed: ~p Traffic: ~p~n", [6043,Date,30,Traffic]),
            drone_6043(D1,L1)
    end.

drone_5937(D2,L2) ->                                                      %%% Process for drone_1
    receive
        shutdown ->
            io:format("simulation shutdown for drone_5937~n", []);
        {Lat,Long,Date,Station,S_lat,S_long,Distance} ->
            io:format(L2,"Droneid: ~p is at Lat: ~p Long: ~p Tube Station: ~p at Lat: ~p Long: ~p and Distance: ~pm~n", [5937,Lat,Long,Station,S_lat,S_long,Distance]),
            Traffic= traffic_rand(),
            io:format(D2,"Droneid: ~p Time: ~p Speed: ~p Traffic: ~p~n", [5937,Date,30,Traffic]),
            drone_5937(D2,L2)
    end.

traffic_rand()->                                                          %%% Randomely selecting traffic status
	Values=["HEAVY","LIGHT","MODERATE"],
	N = rand:uniform(length(Values)),
	lists:nth(N,Values).

dispatcher(Filename_6043,Filename_5937,Tube_file)->                       %%% Dispatcher for each drone process
    spawn(get_coordinates, read_coordinates, [Filename_6043,drone_1,Tube_file]),
    spawn(get_coordinates, read_coordinates, [Filename_5937,drone_2,Tube_file]).
    
simulate(Filename_6043,Filename_5937,Tube_file) ->                        %%% First function to be called to start simulation calling for test
    {ok, D1} = file:open("6043_report.txt", [write]),
    {ok, D2} = file:open("5937_report.txt", [write]),
    {ok, L1} = file:open("6043_Communication.log", [write]),
    {ok, L2} = file:open("5937_Communication.log", [write]),
    register(drone_1, spawn(simulator, drone_6043, [D1,L1])),
    register(drone_2, spawn(simulator, drone_5937, [D2,L2])),
    dispatcher(Filename_6043,Filename_5937,Tube_file).
