% @Author: Puffrora
% @Date:   2020-05-24 11:11:31
% @Last Modified by:   Puffrora
% @Last Modified time: 2020-06-30 12:22:15


:- ensure_loaded(library(clpfd)).


puzzle_solution(Puzzle, WordList) :-
  
  % 1. Get valid slots
  % get horizontal slots
  get_slots(Puzzle, HSlots),
  % transpose the matrix
  transpose(Puzzle, Puzzle_T),
  % get vertical slots
  get_slots(Puzzle_T, VSlots),
  % get total slots
  merge_lists(HSlots, VSlots, TotalSlots),
  % remove slot which length is 1 because a word contains at least two charactor
  delete_len1(TotalSlots, Slots),
  % 2. Match slots to words
  match(Slots, WordList).

% ===================== Get Slots Part =====================


% get_slots(Puzzle_Matrix, Slots).
% Find all fillable slots, each slots 
% must contain _ and between two '#' symbols
get_slots(Puzzle_Matrix, Slots) :-
  map_to_each_row(Puzzle_Matrix, RowSlots),
  sum_list(RowSlots, Slots).


% map a function to each element of a list
% [a, b, c, d,...] -> [f(a), f(b), f(c), f(d), ...]
map_to_each_row([], []).
map_to_each_row([H|Back], [FH|FBack]) :-
  find_slots_each_row(H, FH),
  map_to_each_row(Back, FBack).


% merge all the element of a list
% [[[a,c], [t,f]], [[], [b,v,c]]] -> [[a,c], [t,f], [], [b,v,c]]
sum_list(List, Final) :-
  sum_list(List, [], Final).

sum_list([], Final, Final).
sum_list([H|Back], Acc, Final) :-
  merge_lists(Acc, H, Acc1),
  sum_list(Back, Acc1, Final).


% find_slots_each_row(Row, RowSlots).
% find all valid slots in a row
find_slots_each_row(Row, RowSlots) :-
  % add accumulator
  find_slots_each_row_acc(Row, [], RowSlots).


% find_slots_each_row_acc(Row, Acc, RowSlots).
% When the row is empty list
find_slots_each_row_acc([], [], []).
% Happens if the end of a row is not '#'
find_slots_each_row_acc([], Acc, RowSlots) :-
  Acc \= [],
  RowSlots = [Acc].
% When the row is not empty
find_slots_each_row_acc([H|Back], Acc, RowSlots) :-
  ( H == '#'
    ->  RowSlots = [Acc|Rest],
        find_slots_each_row_acc(Back, [], Rest)
    ;   merge_lists(Acc, [H], Acc1),
        find_slots_each_row_acc(Back, Acc1, RowSlots)
  ).


% ===================== Match Slots Part =====================


% match(Slots, WordList).
% Steps:
% 1. Find the best slot (with fewest matching words)
% 2. Select one word from the matching words above
% 3. Delete the matched slot and word from all slots and all words
% 4. Repeat 1 until all slots matched
match([], []).
match(Slots, WordList) :-
  find_best_slot(Slots, WordList, BestSlot),
  find_best_word(BestSlot, WordList, BestWord),
  delete_slots(BestSlot, Slots, RestSlots),
  delete_slots(BestWord, WordList, RestWords),
  match(RestSlots, RestWords).


% find_best_slot(AllSlots, WordList, BestSlot).
% Holds true if BestSlot is a slot that has the fewest matching words
find_best_slot([Slot|Slots], WordList, BestSlot) :-
  get_occur_times(Slot, WordList, Count),
  find_best_slot(Slots, WordList, Count, BestSlot, Slot).


% get_occur_times(Slot, List, Count).
% Get the occurrence times, the Count, of Slot in List
get_occur_times(Slot, List, Count) :-
  find_unified(Slot, List, FilteredList),
  length(FilteredList, Count).


% find_best_slot([Slot|Slots], WordList, BestMatchCount, BestSlot, AccSlot). 
% Iteratively find the BestSlot with the fewest matching words.
find_best_slot([], _, _, BestSlot, BestSlot).
find_best_slot([Slot|Slots], WordList, BestMatchCount, BestSlot, AccSlot) :-
  get_occur_times(Slot, WordList, Count),
  ( Count < BestMatchCount 
    ->  find_best_slot(Slots, WordList, Count, BestSlot, Slot)
    ;   find_best_slot(Slots, WordList, BestMatchCount, BestSlot, AccSlot)
  ).


% find_best_word(BestSlot, WordList, BestWord).
% Get a word matching the BestSlot.
find_best_word(BestSlot, WordList, BestWord) :-
  find_unified(BestSlot, WordList, MatchingWords),
  member(BestWord, MatchingWords),
  BestSlot = BestWord.


% ===================== Basic Function Part =====================


% merge_lists(L1, L2, L3).
% L3 is L1 + L2
merge_lists([], L, L).
merge_lists([H|Back], L, [H|Rest]) :-
  merge_lists(Back, L, Rest).


% function to delete the slots with length <= 1
delete_len1(TotalSlots, Slots) :-
  delete_len1(TotalSlots, [], Slots).

delete_len1([], Final, Final).
delete_len1([H|Back], Acc, Final) :-
  length(H, L),
  ( L > 1
    -> merge_lists(Acc, [H], Acc1),
       delete_len1(Back, Acc1, Final)
    ;  delete_len1(Back, Acc, Final)

  ).


% delte the found slots from all slots list
% eg. delete_slots([_1895,_1234,c], [[_1589,v,c], [_1895,_1234,c], [a,_2020,c], [r,a,c]], Slots).
% Slots = [[_1589,v,c], [a,_2020,c], [r,a,c]].
delete_slots(Target, List, FilteredList) :-
  delete_slots(Target, List, [], FilteredList).

delete_slots(Target, [], Final, Final).
delete_slots(Target, [H|Back], Acc, Final) :-
  ( Target \== H
    -> merge_lists(Acc, [H], Acc1),
       delete_slots(Target, Back, Acc1, Final)
    ;  delete_slots(Target, Back, Acc, Final)

  ).


% find all elements in a list that can be unified with the target
% eg. find_unified([a,_,c], [[a,v,c], [g,c,c], [a,r,c], [r,a,c]], FL).
% FL = [[a,v,c], [a,r,c]].
find_unified(Target, List, FilteredList) :-
  find_unified(Target, List, [], FilteredList).

find_unified(Target, [], Final, Final).
find_unified(Target, [H|Back], Acc, Final) :-
  ( Target \= H
    -> find_unified(Target, Back, Acc, Final)
    ;  merge_lists(Acc, [H], Acc1),
       find_unified(Target, Back, Acc1, Final)

  ).

