%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% EVENT CALCULUS %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

holdsAt(F, 0) :- initially(F).
holdsAt(F, T) :- initially(F), !, \+(clipped(0, F, T)).
holdsAt(F, T) :- happens(E, T1), initiates(E, F, T1), T1 < T, \+clipped(T1, F, T).
clipped(T1, F, T2) :- happens(E, T), T > T1, T < T2, terminates(E, F, T).

initially(light_off).
initiates(push_button, light_on, T) :- holdsAt(light_off, T).
terminates(push_button, light_off, T) :- holdsAt(light_off, T).
initiates(push_button, light_off, T) :- holdsAt(light_on, T).
terminates(push_button, light_on, T) :- holdsAt(light_on, T).

happens(push_button, 3).
happens(push_button, 5).
happens(push_button, 6).
happens(push_button, 8).
happens(push_button, 9).
