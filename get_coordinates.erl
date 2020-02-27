-module(get_coordinates).
-export([read_coordinates/3]).

read_coordinates(Filename,Drone_id,Tube_file)->                            %%% For reading the coordinates from the file
    {ok, Device} = file:open(Filename, [read]),
    try get_all_lines(Device,Drone_id,Tube_file,na,0)
      after file:close(Device)
    end.

get_all_lines(Device,Drone_id,Tube_file,Operation,N) ->
    case file:read_line(Device) of
        eof  -> [];
        {ok,Line} ->
		    case Operation of 
			    tube->                                                     %%% Only when a tube coordinates need to fetch
                    [Name1,Lat1,Long1]=re:split(Line, ",",[{return,list}]),
		            {Lat1,lists:sublist(Long1,1,length(Long1)-2),
					lists:sublist(Name1,2,length(Name1)-2),Device};
				_->                                                        %%% To fetch drone coordinates
				    [_,Lat1,Long1,Date1]=re:split(Line, ",",[{return,list}]),
                    Lat=lists:sublist(Lat1,2,length(Lat1)-2),
                    Long=lists:sublist(Long1,2,length(Long1)-2),
                    Date=lists:sublist(Date1,13,8),
                    case lists:sublist(Date,1,length(Date)-3) of           %%% Logic to stop simulation at 08:10
                        "08:10"-> Drone_id ! shutdown;
                        _-> case N of                                      %%% Logic to send only 10 coordinates per drone
                                11->
                                    timer:sleep(3000),                     %%% To have a delay between 10 coordinates batch
				                    {Station,Distance,S_lat,S_long}=get_station(Lat,Long,Tube_file),
                                    Drone_id ! {Lat,Long,Date,Station,S_lat,S_long,Distance},   %%% sending coordinates to drone
                                    get_all_lines(Device,Drone_id,Tube_file,Operation,0);
                                _-> 
	                            {Station,Distance,S_lat,S_long}=get_station(Lat,Long,Tube_file),
                                    Drone_id ! {Lat,Long,Date,Station,S_lat,S_long,Distance},   %%% sending coordinates to drone
                                    get_all_lines(Device,Drone_id,Tube_file,Operation,N+1)  
                            end
                    end
			end
    end.
	
get_station(Lat,Long,Tube_file)->                                          %%% To get the nearest station under 300m   
    {ok, Device} = file:open(Tube_file, [read]),
    get_station(Lat,Long,Tube_file,Device).
	
get_station(Lat,Long,Tube_file,Device)->
    case get_all_lines(Device,na,Tube_file,tube,0) of
	    {Lat_t,Long_t,Name,Device} ->
		    Distance=calculate_distance(list_to_float(Lat),list_to_float(Long),list_to_float(Lat_t),list_to_float(Long_t)),
	        if 
	            Distance>300 -> get_station(Lat,Long,Tube_file,Device);
		        true ->
			        {Name,Distance,Lat_t,Long_t}
        	end;
		_ ->file:close(Device),
                    {"no station nearby",0,na,na}
	end.
	
calculate_distance(Lat1, Long1, Lat2, Long2) ->                            %%% To get the exact distance between two     
                                                                           %%% coordinates in meteres 
    Deg2rad = fun(Deg) -> math:pi()*Deg/180 end,
    [RLong1, RLat1, RLong2, RLat2] = [Deg2rad(Deg) || Deg <- [Long1, Lat1, Long2, Lat2]],
    DLon = RLong2 - RLong1,
    DLat = RLat2 - RLat1,
    A = math:pow(math:sin(DLat/2), 2) + math:cos(RLat1) * math:cos(RLat2) * math:pow(math:sin(DLon/2), 2),
    C = 2 * math:asin(math:sqrt(A)),
    %% suppose radius of Earth is 6372.8 km
    Km = 6372.8 * C,
    Km*1000.
