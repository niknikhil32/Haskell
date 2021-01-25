

flight(miami, florida,panair,9000,3,7000).
flight(chicago, florida,ab,8000,4,1000).
flight(miami, chicago,ab,8000,4,1000).
flight(delhi,amsterdam,bc,1000,2,500).
flight(mauritius,amsterdam,bc,1000,1,300).
flight(delhi, mauritius,ca,2000,7,4000).
flight(berlin, seoul,ab,6000,8,4500).
flight(paris, seoul,bc,3000,9,4600).
flight(berlin, paris,ca,4000,10,4700).


/**
 * 
 * Defining an inverse relationship between the flights if flight between x and y exists then also there is a flight between y and x
 * 
 */
joined(X,Y,_,_,_,_) :- flight(X,Y,_,_,_,_) ; flight(Y,X,_,_,_,_). 




country(paris,france).
country(miami,usa).
country(chicao,usa).
country(delhi,india).
country(berlin,germany).
country(mauritius,france).
country(seoul,korea).
country(amsterdam ,holland).




/**
 * this calculates the total distance for the path defined
 *
 * 
 */

dist([_], 0).            
dist([A, B | T], L) :-
    flight(A, B,_ ,L1,_,_), 
    dist([B|T], L2),     
    L is L1 + L2.

/**
Calculates cost of the trip 
*/
cost([_], 0).            
cost([A, B | T], L) :-
    flight(A, B,_ ,_,_,L1), 
    cost([B|T], L2),     
    L is L1 + L2.
/*
This calculates the total time taken by the flight trip
*/

triptime([_], 0).            
triptime([A, B | T], L) :-
    flight(A, B,_ ,_,L1,_), 
    triptime([B|T], L2),     
    L is L1 + L2.

 list_length(Xs,L) :- list_length(Xs,0,L) .

list_length( []     , L , L ) .
list_length( [_|Xs] , T , L ) :-
  T1 is T+1 ,
  list_length(Xs,T1,L)
  .   

list_planes(X, Y, Z) :- Z is X - Y.

/**
 * Q1 -- Listing all the airports in the country 
   findAll()/3 is th einbuilt predicate which returns the list of the output
   instead of iterating it over and over again
 *
 * 
 */
list_airport(A, N) :-
    findall(Ki, country(Ki, A), N).


/**
 * Defining a filter which filters the trips as per the airlines given
 Here in Filter you can pass the name of any airline and it will not show the flights from that particular airline
 *
 * 
 */
filterfortrip(AA,BB,Search, Way) :-
  traverse(AA,BB,Search, [AA],QQ),
  reverse(QQ,Way).

traverse(AA,BB, Search, PP,[BB|PP]) :-
  flight(AA,BB,Aero,_,_,_),
  call(Search, Aero).
traverse(AA,BB, Search, Travelled,Way) :-
  flight(AA,CC,Aero,_,_,_),
  CC \== BB,
  call(Search, Aero),
   \+ member(CC,Travelled),
  traverse(CC,BB, Search, [CC|Travelled],Way).              


/**
 * Definning a trip predicate to determine all the trips between cities
 here any filter is passed to the filterfortrip which means it considers trips with all the airlines
 * 
 */


trip(X,Y,T) :-filterfortrip(X,Y,any,T).





/**
 * Finding the list of all the trips between tweo cities ,findAll()/4 is an inbuilt predicate in prolog
 which returns the 
 */

 all_trip(C,L,T):-
      findall(Ci, filterfortrip(C,L,any,Ci), T).
    


/**
 * Calculates the trip distance between two cities ,to get all the data enter semicolon after every result
 */
trip_dist(X,Y,[T,D]):-
         filterfortrip(X,Y,any,T),
         dist(T,D).

        

/**
 * Calculates the trip cost between two cities ,to get all the data enter semicolon after every result
 */

trip_cost(X,Y,[T,C]):-
          filterfortrip(X,Y,any,T),
          cost(T,C).
  any(_).         


/**
 * Calculates the total trip time,to get all the data enter semicolon after every result
 */
     

     trip_time(X,Y,[T,C]):-
         filterfortrip(X,Y,any,T),
         triptime(T,C).


/**
 * this identifies how many planes does the use change between flight for two cities
  generally it is list ofplanes -2 (ie length of list  of cities between two cities -2)
 */
     
  trip_change(X,Y,[T,I]):-
                   filterfortrip(X,Y,any,T),
                   list_length(T,L),
                   list_planes(L,2,I).


   /**
 * This gives all the trips avoiding one airline 
 */

all_trip_noairline(C,L,T,A):-
   findall(Ci, filterfortrip(C,L, except(A), Ci), T).   

except(X, Y):- X\=Y.
                     
  
                

cheapest(X,Y,T):-
    findall([Ci,Cj],trip_cost(X,Y,[Ci,Cj]),K),
    aggregate_all(min(A,B), 
       member([B,A], K), 
       T).

   /**
 * This gives the shortest trip between two entered cities
 */


 cheapest(X,Y,T,C):-
    cheapest(X,Y,min(C,T)).

shortest(X,Y,T) :-
      findall([Ci,Cj],trip_dist(X,Y,[Ci,Cj]),K),
    aggregate_all(min(A,B), 
       member([B,A], K), 
       T).
     
/**
This gives the fastest trip between two entered cities
*/


 shortest(X,Y,T,C):-
    shortest(X,Y,min(C,T)).


fastest(X,Y,T):-
      findall([Ci,Cj],trip_time(X,Y,[Ci,Cj]),K),
       aggregate_all(min(A,B), 
       member([B,A], K), 
       T).
/**
This provides trip to all nations ,to get the all data put semicolon after every output

**/

 fastest(X,Y,T,C):-
    fastest(X,Y,min(C,T)).

trip_to_nation(X,Y,T):-
      list_airport(Y,A),
      
      member(City, A), 
  
      all_trip(X, City, T). 
 
     
      
  
      
 

   
                    
                   