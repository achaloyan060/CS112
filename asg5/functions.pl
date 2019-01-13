

haversine( Lat1, Lon1, Lat2, Lon2, D) :- 
    Temp is sin( (Lat2 - Lat1) / 2 ) ** 2
       + cos( Lat1 ) * cos( Lat2 ) * sin( (Lon2 - Lon1) / 2 ) ** 2,
    D is 2 * atan2( sqrt( Temp ), sqrt( 1 - Temp )) * 3961 .

toHours( time(Hours, Mins), H) :-
    H is Hours + Mins / 60 .
  
toRads( degmin( Degrees, Mins) , Rads) :-
    Rads is (Degrees + Mins / 60) / 180 * pi . 

distance( Start, Finish, D) :-
    airport( Start, _, Lat1, Lon1 ),
    airport( Finish, _, Lat2, Lon2 ),
    toRads( Lat1, Lat1_r ),
    toRads( Lat2, Lat2_r ),
    toRads( Lon1, Lon1_r ),
    toRads( Lon2, Lon2_r ),
    haversine( Lat1_r, Lon1_r, Lat2_r, Lon2_r, D).

not( X ) :- X, !, fail.
not( _ ).

getflight( End, End, _, [End], _ ).

getflight( Curr, End, Visited, [[Curr, DepartTime, ArriveTime] | List],
          DepartTimeHM ) :-
    flight( Curr, End, DepartTimeHM ),
    not( member( End, Visited ) ),
    toHours( DepartTimeHM, DepartTime ),
    distance( Curr, End, Dmi ),
    Dhr is Dmi/500,            /*convert miles to hours*/
    ArriveTime is DepartTime + Dhr,
    ArriveTime < 24.0,
    getflight( End, End, [End | Visited], List, _).

getflight( Curr, End, Visited, [[Curr, DepartTime, ArriveTime] | List],
          DepartTimeHM ) :-
    flight( Curr, Next, DepartTimeHM ),
    not( member( Next, Visited ) ),
    toHours( DepartTimeHM, DepartTime ),
    distance( Curr, Next, Dmi ),
    Dhr is Dmi/500,            /*convert miles to hours*/
    ArriveTime is DepartTime + Dhr,
    ArriveTime < 24.0,
    flight( Next, _, NextDepartTimeHM ),
    toHours( NextDepartTimeHM, NextDepartTime ),
    TimeDiff is NextDepartTime - ArriveTime - 0.5,
    TimeDiff >= 0,
    getflight( Next, End, [Next | Visited], List, NextDepartTimeHM ).

printDigits( Digits ) :-
    Digits < 10, print(0), print(Digits).
printDigits( Digits ) :-
    Digits >= 10, print(Digits).


printTime( Time ) :-
    Hours is (floor( Time * 60 )) // 60,
    Mins is (floor( Time * 60 )) mod 60,
    printDigits( Hours ), print(':'), printDigits( Mins ).

writeflight( [] ) :-
    nl.

writeflight( [[Start, DTime, ATime], Finish | []] ) :-
    airport( Start, Start_name, _, _),
    airport( Finish, Finish_name, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( Start ), write( '  ' ),
    write( Start_name ),
    printTime( DTime ), nl,
    write( '     ' ), write( 'arrive  ' ),
    write( Finish ), write( '  ' ),
    write( Finish_name ),
    printTime( ATime ), nl,
    !, true.

writeflight( [[Start, DTime, ATime], [Finish, ADTime, AATime] 
                                     | Rest] ) :-
    airport( Start, Start_name, _, _),
    airport( Finish, Finish_name, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( Start ), write( '  ' ),
    write( Start_name ),
    printTime( DTime ), nl,
    write( '     ' ), write( 'arrive  ' ),
    write( Finish ), write( '  ' ),
    write( Finish_name ),
    printTime( ATime ), nl,
    !, writeflight( [[Finish, ADTime, AATime] | Rest] ).

fly( Depart, Depart ) :-
    write( 'Error: cannot fly from and to the same airport.' ),
    !, fail.

fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),

    getflight( Depart, Arrive, [Depart], List, _ ),
    !, nl,
    writeflight( List ),
    true.

fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    write( 'Error: requested flight is not possible.' ),
    !, fail.

fly( _, _) :-
    write( 'Error: nonexistent airport(s).' ), nl,
    !, fail.
