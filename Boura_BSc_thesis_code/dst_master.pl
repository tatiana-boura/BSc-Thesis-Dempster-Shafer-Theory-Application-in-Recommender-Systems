:-module(dst_master).
:-export(bpa/3).
:-export(compute_K/1).
:-export(belief/2).
:-export(belief_comb/3).

:-use_module("mass_func.pl").

:-lib(ic_sets).
:-lib(ic).


%Tars are the subsets and NewVals their corresponding values
%bpa( -Tars, -NewVals, -TarsAndVals) produces the new focal points Tars their combined values NewVals and returns combinations list
bpa(Tars, NewVals, TarsAndVals):-
				theta(Theta),
                findall( (A,Val), compute(A, Val, Theta), TarsAndVals),
                sort(TarsAndVals, SortedTarsAndVals),
                sum_same_sets(SortedTarsAndVals, NewTarsAndVals),
                split_to_Tars_Vals(NewTarsAndVals, Tars, Vals),
                sum(Vals,Div),
                list_div(Div, Vals, NewVals)
                .

%if a set can be the intersection of more than one combination of sets, then it will have more
%than one value computed, all in neighbour elements, so 'zip' them in one
%sum_same_sets(+L, -NewL) handles duplicate sets    
sum_same_sets([],[]):-!.
sum_same_sets([X],[X]):-!.
sum_same_sets([(T1,V1),(T2,V2)|Rest], NewL):-
                                        T1 == T2,   %if same set
                                        !,
                                        V is V1 + V2,   %zip to one
                                        sum_same_sets([(T1,V)|Rest], NewL)
                                        .
sum_same_sets([(T1,V1),(T2,V2)|Rest], [(T1,V1)|NewL]):-
                                        sum_same_sets([(T2,V2)|Rest], NewL)
                                        .

%given a list [(Tar,Val)|..] splits to two lists, [Tar|..] and [Var|..]
%split_to_Tars_Vals(+List, -L1, -L2) splits turples of List to lists L1 and L2
split_to_Tars_Vals([], [], []):-!.
split_to_Tars_Vals([(T,V)|Rest], [T|Tars], [V|Vals]):-
                                    split_to_Tars_Vals(Rest, Tars, Vals).

                                    
                                   
%restricts A to be subset of Hyper
%compute(-A, -Val, +Hyper) finds a combination of sets whose intersection is A and computes its value
compute( A, Val, Hyper) :-
		ic_sets:subset(A, Hyper),
        not_empty(A),
        new_focal_points(Sets),
        ic_sets:all_intersection(Sets,A),
        constrain_membership(Sets),
        values(1,Sets,Vals),
        compute_val(Vals,Val)
        .



%new_focal_points(-Sets) %Sets are the new focal points
new_focal_points(Sets):- 
					theta(Theta),
					length(Theta,Length),
					num_of_m(N),
					intsets(Sets, N, 1, Length) %N sets from [],[1],..,[1..Length]					
					.
		
        
%search for each set S in Ss 
%label_sets(+Sets)
label_sets([]).
label_sets([S|Ss]) :-
                insetdomain(S,_,_,_),
                label_sets(Ss)
                .

    
%Set S is not empty (its cardinality is at least 1)
%not_empty(+S)
not_empty(S):-
            #(S,C),
            C #>= 1
            .

%each set S of Sets is not empty
%all_not_empty(+Sets)            
all_not_empty([]).
all_not_empty([S|Sets]):-
                    not_empty(S),
                    all_not_empty(Sets)
                    .    
        
    
%for each subset, finds its mass-function's value 
%values(+Cur, +Sets, -Vals)
values(_, [], []).
values(Cur, [S|Rest], [V|L]):-
                    m(Cur,S,V),
                    Newcur is Cur+1,
                    values(Newcur, Rest, L)
                    .


%finds product of list's elements 
%compute_val(+List, -P)
compute_val([], 1.0).
compute_val([X|Rest] , Val):-
                    compute_val(Rest,V),
                    Val is V * X
                    .


%divide each element of list with K
%list_div(+Div, +List, -FinalList)
list_div(_,[],[]).
list_div(K, [X|Rest], [Y|NewRest]):-
                    Y is X / K,
                    list_div(K, Rest, NewRest)
                    .
                    
%compute_K(-K) computes K by finding all non empty intersections                    
compute_K(K):-
            theta(Theta),
            findall( Val, compute(_, Val, Theta), Vals),
            sum(Vals,V),
            K is 1 - V
            .
	
%constrain_membership(+Sets) Sets to be focal points of mass functions			   
constrain_membership(Sets):-help_constr(Sets,1).

%help_constr(+Sets, +N) helper function for constrain_membership               
help_constr([],_).
help_constr([S|Sets],Cur):-
							m(Cur,S,_),
							NewCur is Cur + 1,
							help_constr(Sets, NewCur)
							.              
               
                             
%belief(+A, -V) finds belief for set A                    
belief(A, V):-        
            findall(Val, compute(_,Val,A), Vals),        %and find values for them
            %norm_compute(Vals, V)
			sum(Vals, V)
            .

%belief_comb(+A, -V, -Vals) finds belief for set A and returns the Vals list                   
belief_comb(A, V, Vals):-        
            findall(Val, compute(_,Val,A), Vals),        %and find values for them
            %norm_compute(Vals, V)
            sum(Vals, V)
            .


%norm_compute(+Vals, -FinalV)	normalizes results		  
norm_compute(Vals, FinalV):-
			compute_K(K),
            Div is 1-K,
            sum(Vals, Value),
			FinalV is Value / Div
			.            
            
%plau(+A, -FinalV) finds plau of A, using belief
plau(A, FinalV):-
            complement(A, ACompl),
            belief(ACompl, V),
            FinalV is 1 - V
            .


%complement(+A, -ACompl) finds complement of set A            
complement(A, ACompl):-
				theta(Theta),
				subtract(Theta, A, ACompl)
                .


