%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% EVENT CALCULUS %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ontology
% holdsAt(F, T): The fluent F holds at time T (used in domain-independent axioms)
% happens(E, T): Event E happened at time T (used in domain-dependent axioms)
% initiates(E, F, T): Event E causes fluent F to hold at time T (used in domain-dependent axioms)
% terminates(E, F, T): Event E causes fluent F to cease to hold at time T (used in domain-dependent axioms)
% clipped(T1, F, T2): Fluent F has been made false between T1 and T2 (used in domain-independent axioms)
% initially(F) : Fluent F holds at time 0 (used in domain-dependent axioms)

% Domain-independent axioms
holdsAt(F, 0) :- initially(F).
holdsAt(F, T) :- initially(F), \+(clipped(0, F, T)), !.
holdsAt(F, T) :- happens(E, T1), T1 < T, initiates(E, F, T1), \+(clipped(T1, F, T)), !.
clipped(T1, F, T2) :- happens(E, T), T >= T1, T < T2, terminates(E, F, T).

% Example 1 of domain-dependent axioms
% There is a light switch that turns ON and OFF a light.
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

% Example 2 of domain-dependent axioms
% A robot can grab a box if and only if it it in position A
% and if can only drop the box in position B.
% The robot can also move between A and B.
initiates(grab, holding, T) :- holdsAt(not_holding, T), holdsAt(at_a, T).
terminates(grab, not_holding, T) :- holdsAt(not_holding, T).
terminates(drop, holding, T) :- holdsAt(holding, T), holdsAt(at_b, T).
initiates(drop, not_holding, T) :- holdsAt(holding, T).
initiates(move, at_a, T) :- holdsAt(at_b, T).
terminates(move, at_a, T) :- holdsAt(at_a, T).
initiates(move, at_b, T) :- holdsAt(at_a, T).
terminates(move, at_b, T) :- holdsAt(at_b, T).

initially(not_holding).
initially(at_b).
happens(move, 3).
happens(grab, 6).
happens(move, 7).
happens(drop, 8).
