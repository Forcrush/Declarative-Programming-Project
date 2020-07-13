% SWI Prolog provides two different, incompatible transpose/2 
% predicates and loads the wrong one by default. This ensures the 
% correct one is loaded. 
:- ensure_loaded(library(clpfd)).

% main(PuzzleFile, WordlistFile, SolutionFile).
% Entry point of the program. PuzzleFile contains the unsolved puzzle
% , WordlistFile contains the list of words to fill in and the
% solution will be written to the SolutionFile. 
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

% read_file(Filename, Content).
% Holds when the Content is a 2D matrix of characters representing the
% contents of the file.
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% read_lines(Stream, Content).
% Holds when Content is a 2D list of characters representing the
% content of a file. In Content, each row respresents a line in the
% file while each element in a row represents a character in a line.
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

% read_line(Stream, Line, Last).
% Should hold when Line is the first line in a stream of lines and
% Last correctly indicates if it is the last line in the stream. 
% Line is a list of characters while Last is either true or false.
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% print_puzzle(SolutionFile, Puzzle).
% Writes a puzzle to a SolutionFile. 
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% print_row(Stream, Row).
% Adds a row to a stream and adds a new line at the end of each row.
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% put_puzzle_char(Stream, Char).
% Adds a character to a stream.
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% valid_puzzle(Puzzle).
% Holds when a puzzle is valid. A valid puzzle is defined as either 
% an empty puzzle or all rows in the puzzle having the same length. 
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

% solve_puzzle(Puzzle, WordList, Solved). 
% Holds true when Solved is the solution of Puzzle given a WordList. 
% The main steps involved in solving a puzzle are: 
%   1. Replace the underscores in Puzzle with logical variables.
%   2. Get all slots in Puzzle by getting horizontal slots, 
%      transposing the Puzzle, getting vertical slots, combining 
%      horizontal slots and vertical slots and removing invalid
%      slots. 
%   3. Match slots with words repeatedly until the puzzle is 
%      solved. 
solve_puzzle(Puzzle, WordList, Solved) :-
  % Step 1: replace underscores with logical variables.
  pre_process(Puzzle, Solved),

  % Step 2: getting all slots.
  get_matrix_slots(Solved, HSlots), % get horizontal slots
  transpose(Solved, TransposedPuzzle), % transpose the matrix
  get_matrix_slots(TransposedPuzzle, VSlots), % get vertical slots
  merge_lists(HSlots, VSlots, TotalSlots), % join HSlots and VSlot
  filter_lists(length_greater_than(1), TotalSlots, Slots),

  % Step 3: matching slots with words.
  match(Slots, WordList).

% pre_process(Puzzle, Solved).
% Is true when Solved is the solution of Puzzle. 
pre_process([], []).
pre_process(Rs, Solved) :-
  maplist(process_row, Rs, Solved).

% process_row(Row, RowSolved).
% Holds true when RowSolved is the solution of Row. 
process_row([], []).
process_row(R, RowSolved) :-
  maplist(replace, R, RowSolved).

% replace(Char, ReplacedChar).
% Replaces '_' with logical variable.
replace(Char, ReplacedChar) :- 
  ( Char == '_'
  ->ReplacedChar = _
  ; ReplacedChar = Char 
  ).

% get_matrix_slots(Matrix, Slots).
% Holds true when Matrix is a puzzle and Slots contains a list of all
% the slots that can be filled in. Both Matrix and Slots are 2D lists
% .
get_matrix_slots(Matrix, Slots) :-
  % TempSlots is [[[],[],[]],
  %               [[],[],[]]].
  % needs to reduce the brackets by 1 level.
  maplist(get_row_slots, Matrix, TempSlots), 
  % usee foldl to merge(reduce) the brackets by 1 level.
  foldl(merge_lists, TempSlots, [], Slots). 

% get_row_slots(Row, RSlots).
% Is true when RSlots contains all slots in the Row. 
get_row_slots(R, RSlots) :- 
  get_row_slots_2(R, [], RSlots).

% get_row_slots_2(Row, Accumulator, RSlots).
% Holds true when RSlots contatins a list of all the slots in Row.
% This predicate iterates over each element in Row (a vector). Each
% empty element is added to the accumulator and the accumulator is
% added to the RSlots when a '#' is encountered. 
get_row_slots_2([], [], []).
get_row_slots_2([], Acc, RSlots) :- 
  Acc \= [],
  RSlots = [Acc].
get_row_slots_2([C|Cs], Acc, RSlots) :-
  ( C == '#'
  ->RSlots = [Acc|Rest],
    get_row_slots_2(Cs, [], Rest)
  ; merge_lists(Acc, [C], Acc1),
    get_row_slots_2(Cs, Acc1, RSlots)
  ).

% match(Slots, WordList).
% matches words to slots by: 
%   1. Get the next slot. A best slot is one that have the fewest
%      matching words. 
%   2. Get the matching words for the selected slot. And select a 
%      word as a match. 
%   3. Remove the matched slots and words from the list of all slots. 
%   4. Repeat step 1 for the remaining slots and words. 
match([], []).
match(Slots, WordList) :-
  get_next_slot(Slots, WordList, NextSlot),
  get_next_word(NextSlot, WordList, Word),
  filter_lists(\==(NextSlot), Slots, RestSlots),
  filter_lists(\==(Word), WordList, RestWords),
  match(RestSlots, RestWords).

% get_next_slot(AllSlots, WordList, NextSlot).
% Holds true when NextSlot is a slot that have the fewest matching
% words in AllSlots given the list of all words WordList.
get_next_slot([Slot|Slots], WordList, NextSlot) :-
  get_matching_count(Slot, WordList, Count),
  get_next_slot(Slots, WordList, NextSlot, Slot, Count).

% get_matching_count(Slot, List, Count).
% Holds true when Count is the number of occurences of Slot in List.
get_matching_count(Slot, List, Count) :-
  filter_failed_elements(\=(Slot), List, FilteredList), 
  length(FilteredList, Count).

% get_next_slot([Slot|Slots], WordList, NextSlot, AccSlot, 
% BestMatchCount). 
% Gets the NextSlot that has the fewest matching words. The variables
% are: 
%   - [Slot|Slots]: the list of all slots
%   - WordList: the list of all words
%   - AccSlot: the current slot that has the fewest matching words. 
%   - BestMatchCount: the number of matching words for AccSlot. 
%   - NextSlot: the next slot to choose
% At each iteration, the Count of the current Slot is calculated. 
% And if the Count is better than the previous best slot, we replace
% the previous best slot with this Slot. 
% After we have exhausted the lsit of slots, we simply replace
% NextSlot with the Slot in the accumulator. 
get_next_slot([], _WordList, NextSlot, NextSlot, _Count).
get_next_slot([Slot|Slots], WordList, NextSlot, AccSlot, BestMatchCount) :-
  get_matching_count(Slot, WordList, Count),
  (
    Count < BestMatchCount 
  ->get_next_slot(Slots, WordList, NextSlot, Slot, Count)
  ; get_next_slot(Slots, WordList, NextSlot, AccSlot, BestMatchCount)
  ).

% get_next_word(NextSlot, WordList, Word).
% Given a slot, gets a single matching word.
get_next_word(NextSlot, WordList, Word) :-
  filter_failed_elements(\=(NextSlot), WordList, MatchingWords),
  member(Word, MatchingWords),
  NextSlot = Word.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Some generic utility predicates that can be used in other programs
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% samelength(List1, List2).
% Is true when two lists are of the same length.
samelength([], []).
samelength([_|L1], [_|L2]) :-
	samelength(L1, L2).

% merge_lists(List1, List2, List3).
% Holds when List3 is the merged result of List1 and List2.
merge_lists([], L, L ).
merge_lists([E|Es], L, [E|Rest]) :-
  merge_lists(Es, L, Rest).

% filter_lists(Filter, List, FilteredList)
% Holds true when Filter is a predicate, and FilteredList contains
% the elements from List that satisfies the Filter.
filter_lists(_, [], []).
filter_lists(F, [E|Es], FilteredList) :- 
  ( call(F, E)
  ->FilteredList = [E|FilteredList1]
  ; FilteredList = FilteredList1
  ),
  filter_lists(F, Es, FilteredList1).

% filter_failed_elements(F, [E|Es], FilteredList).
% Applies a filter F over each element of a list. Holds true when 
% FilteredList is a collection of elements that fails the filter. 
filter_failed_elements(_, [], []).
filter_failed_elements(F, [E|Es], FilteredList) :- 
  (
    call(F, E)
  ->FilteredList = FilteredList1
  ; FilteredList = [E|FilteredList1]
  ),
  filter_failed_elements(F, Es, FilteredList1).

% length_greater_than(Value, List).
% Is true when the length of List is bigger than the specified Value. 
length_greater_than(Value, List) :-
  length(List, L),
  L > Value.
