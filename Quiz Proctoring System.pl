assign_proctors(AllTAs, Quizzes, TeachingSchedule, ProctoringSchedule):-
    free_schedule(AllTAs, TeachingSchedule, FreeSchedule),!,
	assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule).




free_schedule(AllTAs,[], []).
free_schedule(AllTAs, [day( DayName , DaySchedule) | T1] ,  [FreeSchedule|T2]  ):-
	 working(AllTAs,DayName,R),
	 freeday(R,day( DayName , DaySchedule),FreeSchedule),
	 free_schedule(AllTAs,T1,T2).
 



freeslot(_,[],[]).
freeslot([H|T], [HT|TT],[HS1|TFS]):-
      subtract(H,HT,HS),
	  permutation(HS,HS1),	
	  freeslot(T,TT,TFS).


freeday([],day(DayName,[[],[],[],[],[]]),day(DayName,[[],[],[],[],[]])).	 
freeday( R, day( DayName , [HT|TT])  ,day( DayName , [HF|TF])):-
     Z1=[R,R,R,R,R],
     freeslot(Z1,[HT|TT],[HF|TF]).

%freeday([H|T],day(DayName,TT),day(DayName,TF)).



    
working([],_,[]).
working([ta(N,DayOff)|T] , DayName , [N|TL]):-
      DayOff \= DayName ,
	  working(T,DayName,TL).

working([ta(N,DayOff)|T] , DayName , R):-
        DayOff = DayName ,
		working(T,DayName,R).
assign_quiz( [], _ , [] ).
assign_quiz(quiz(Course, Day, Slot, Count), [HF2|TF2], ATAs):-

	HF2 = day(DayName, DaySchedule),
    Day = DayName,
    nth1(Slot, DaySchedule, C),
    length(C, SL),
    SL >= Count,
    permutation(C, X),
    takecount(X, Count, ATAs).
   

assign_quiz(quiz(Course, Day, Slot, Count), [_|TF2], AssignedTAs):-
    assign_quiz(quiz(Course, Day, Slot, Count), TF2, AssignedTAs).

takecount(_, 0, []).
takecount([HX|TX], Count, [HX|TO]):-
    Count1 is Count - 1 ,
    takecount(TX, Count1, TO).

	
updateF(_,[],_,[]).
updateF(ATAs,[HF4|TF4],quiz(Course,Day,Slot,Count),[HNF4|TNF4]):-
    HNF4=day(DayName, DaySchedule2),
    HF4=day(DayName, DaySchedule),
	DayName=Day,	
	edit(DaySchedule,1,Slot,DaySchedule2),		
    updateF(ATAs,TF4,quiz(Course,Day,Slot,Count),TNF4).
	
	
updateF(ATAs,[HF4|TF4],quiz(Course,Day,Slot,Count),[HF4|TNF4]):-	
    HF4=day(DayName, DaySchedule),
	DayName\=Day,
	updateF(ATAs,TF4,quiz(Course,Day,Slot,Count),TNF4).
	
	
edit([],_,_,[]).	
edit([HDO|TDO],CountH,Slot,[HDO|TDN]):-
      CountH\=Slot,
	  CountH1 is CountH+1 ,
	  edit(TDO,CountH,Slot,TDN).
 	
edit([HDO|TDO],CountH,Slot,[HDN|V]):-   
     CountH=Slot,
	 CountH1 is CountH+1 ,
     subtract(ATAs,HDO,HDN),
	 edit(TDO,CountH,Slot,V).
     
	
	
assign_quizzes([],_,[]).	
assign_quizzes([HQ|TQ],FreeSchedule,[proctors(HQ,ATAs)|TP]):-
    HQ=quiz(Course,Day,Slot,Count),
    assign_quiz(HQ,FreeSchedule,ATAs),
	updateF(ATAs,FreeSchedule,HQ,NewF),
	assign_quizzes(TQ,NewF,TP).	