% -*- Prolog -*-

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Menu 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% play/0
% Starts the game by displaying the main menu
play :-
    main_menu.

% main_menu/0
% Displays the game menu and handles user choices
main_menu :-
    write('Welcome to Doblin!'), nl,
    get_grid_size(Size),
    display_menu,
    get_menu_choice(Choice, Size),
    validate_choice(Choice, Size).

% display_menu/0
% Displays the available options in the menu
display_menu :-
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Human'), nl,
    write('4. Computer vs Computer'), nl,
    write('5. Quit'), nl,
    write('Choose an option: '), nl.

% get_menu_choice(+Choice, +Size)
% Reads the user menu choice and handles invalid input using repeat
get_menu_choice(Choice, Size) :-
    repeat,
    catch(read(Choice), error(syntax_error(_), _), fail),
    integer(Choice),
    (   Choice >= 1,
        Choice =< 5
    ),
    !,
    true.

% validate_choice(+Choice, +Size)
% Validates the menu choice and handles it
validate_choice(Choice, _) :-
    integer(Choice),
    member(Choice, [5]),
    write('Goodbye!'), nl.

validate_choice(Choice, Size) :-
    integer(Choice),
    member(Choice, [1, 2, 3, 4]),
    configure_game(Choice, Size, GameConfig),
    initial_state(GameConfig, InitialState),
    game_loop(InitialState).

validate_choice(_) :-
    write('Invalid option! Please try again.'), nl,
    main_menu.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_grid_size(+Size)
% Asks for and validates grid size
get_grid_size(Size) :-
    write('Choose grid size (between 6 and 9): '),
    repeat,
    catch(read(Input), error(syntax_error(_), _), fail),
    valid_grid_size(Input, Size).

% valid_grid_size(+Input, -Size)
% Validates grid size
valid_grid_size(Input, Size) :-
    integer(Input),
    Input >= 6,
    Input =< 9,
    Size = Input.

valid_grid_size(_) :-
    write('Invalid grid size! Please choose a size between 6 and 9.'), nl,
    get_grid_size(_).

% valid_name(+Name)
% Ensures name does not exceed 16 characters and is not 'CPU'
valid_name(Name) :-
    Name \= 'CPU',
    Name \= 'CPU1',
    Name \= 'CPU2',
    atom_length(Name, Length),
    Length =< 16. 

% configure_game(+Mode, +Size, -Config) :- 
% Configures the game based on selected mode
configure_game(1, Size, config(Name1, Name2, _, _, Size)) :- 
    write('Human vs Human selected.'), nl,
    get_player_name('Player 1', Name1),
    get_player_name('Player 2', Name2).

configure_game(2, Size, config(Name1, 'CPU', _, Level, Size)) :- 
    write('Human vs Computer selected.'), nl,
    get_player_name('Player 1', Name1),
    get_ai_level('CPU', Level).

configure_game(3, Size, config('CPU', Name, Level, _, Size)) :- 
    write('Computer vs Human selected.'), nl,
    get_ai_level('CPU', Level),
    get_player_name('Player 2', Name).

configure_game(4, Size, config('CPU1', 'CPU2', Level1, Level2, Size)) :-
    write('Computer vs Computer selected.'), nl,
    get_ai_level('CPU1', Level1),
    get_ai_level('CPU2', Level2).

% get_player_name(+PlayerLabel, -Name)
% Asks for player name and validates it
get_player_name(PlayerLabel, Name) :-
    format('Enter name for ~w (only letters and max 16 chars): ', [PlayerLabel]),
    catch(read(Name), error(syntax_error(_), _), fail),
    valid_name(Name).

% get_ai_level(+CPUName, -Level)
% Asks for AI difficulty
get_ai_level(CPUName, Level) :-
    format('Choose difficulty for ~w (1: Easy, 2: Hard): ', [CPUName]),
    catch(read(Input), error(syntax_error(_), _), fail),
    validate_difficulty(CPUName, Input, Level).

% validate_difficulty(+CPUName, +Input, -Level)
% Validates AI difficulty input
validate_difficulty(_, 1, 1).
validate_difficulty(_, 2, 2).
validate_difficulty(CPUName, _, _) :-
    write('Invalid difficulty! Please choose 1 or 2.'), nl,
    get_ai_level(CPUName, Level).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game State Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initializes the game state based on the configuration
initial_state(config(Name1, Name2, AI1Level, AI2Level, Size), game_state(Grid1, Grid2, CurrentPlayer, Name1, Name2, RowMapping, ColMapping, AI1Level, AI2Level)) :-
    CurrentPlayer = Name1,
    initialize_grids(Size, Grid1, Grid2),
    generate_mappings(Size, RowMapping, ColMapping).

% Initializes two grids and their coordinate mappings
initialize_grids(Size, Grid1, Grid2) :-
    create_empty_grid(Size, Grid1),
    create_empty_grid(Size, Grid2).

% Creates an empty grid of the specified size, filled with '_'
create_empty_grid(Size, Grid) :-
    length(Grid, Size),
    maplist(length_(Size), Grid),
    maplist(maplist(=( '_ ')), Grid).  % Initialize each cell with '_'

% Helper predicate to define list length for rows
length_(Size, List) :-
    length(List, Size).

% Generates row and column mappings for the second grid
generate_mappings(Size, RowMapping, ColMapping) :-
    numlist(1, Size, Base),
    random_permutation(Base, RowMapping),
    random_permutation(Base, ColMapping).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Display the current game state
display_game(game_state(Grid1, Grid2, CurrentPlayer, Name1, Name2, RowMapping, ColMapping,_,_)) :-
    length(Grid1, Size),
    nl,
    Width is Size * 3 - 2,
    print_player_names(Name1, Width, 2),
    print_player_names(Name2, Width, Width + 7), nl,  
    print_column_labels(Size, ColMapping), 
    print_side_by_side(Grid1, Grid2, RowMapping),
    nl,
    print_current_player(CurrentPlayer, Name1, Name2).

print_current_player(CurrentPlayer, Name1, Name2) :-
    CurrentPlayer = Name1,
    format('Current Player: ~w~n', [Name1]), nl,
    !.

print_current_player(CurrentPlayer, Name1, Name2) :-
    CurrentPlayer = Name2,
    format('Current Player: ~w~n', [Name2]), nl,
    !.

print_player_names(Name, Width, Offset) :-
    atom(Name),  % Ensure Name is an atom
    atom_length(Name, NameLength),  % Get the length of the name
    Padding is Width - NameLength,  % Total padding needed with the dynamic offset
    LeftPadding is Padding // 2,    % Half padding for the left
    RightPadding is Padding - LeftPadding,  % Remaining padding for the right
    format('~*|~s~*|', [LeftPadding + Offset, Name, RightPadding]).

% Print column labels for both grids
print_column_labels(Size, ColMapping) :-
    write('  '), % Space before column labels for Player 1
    print_column_labels_in_order(Size),  % Write Player 1 labels
    write('   '), % Space between the grids
    print_column_labels_based_on_mapping(ColMapping),  % Write Player 2 labels
    nl.

% Print column labels for Grid 1
print_column_labels_in_order(Size) :-
    numlist(1, Size, Numbers),
    maplist(generate_column_label, Numbers, Labels),
    print_aligned_labels(Labels).

% Print column labels for Grid 2
print_column_labels_based_on_mapping(ColMapping) :-
    maplist(generate_column_label, ColMapping, MappedLabels),
    print_aligned_labels(MappedLabels).

print_aligned_labels([]).
print_aligned_labels([Label|Rest]) :-
    format('~w  ', [Label]),  % Two spaces after each label
    print_aligned_labels(Rest).

% Generate column label for a given index
generate_column_label(Index, Label) :-
    Base is 26,
    Quotient is (Index - 1) // Base,
    Remainder is (Index - 1) mod Base,
    char_code('a', A),
    Quotient =:= 0,
    Code is A + Remainder,
    char_code(Label, Code), 
    !.

generate_column_label(Index, Label) :-
    Base is 26,
    Quotient is (Index - 1) // Base,
    Remainder is (Index - 1) mod Base,
    char_code('a', A),
    FirstCode is A + Quotient - 1,
    SecondCode is A + Remainder,
    char_code(First, FirstCode),
    char_code(Second, SecondCode),
    atom_codes(Label, [First, Second]).

% Print both grids side by side
print_side_by_side(Grid1, Grid2, RowMapping) :-
    length(Grid1, Size),
    numlist(1, Size, Player1RowNumbers),  % Generate sequential row numbers for Player 1
    print_side_by_side_rows(Grid1, Grid2, Player1RowNumbers, RowMapping).

% Helper to print rows side by side with correct row numbers
print_side_by_side_rows([], [], [], []).
print_side_by_side_rows(Player1Rows, Player2Rows, Player1RowNumbers, Player2RowNumbers) :-
    last(Player1RowNumbers,Player1Last),
    length(Player1RowNumbers,SizeP1),
    nth1(SizeP1,Player1RowNumbers,_,RestPlayer1Rows),
    last(Player2RowNumbers,Player2Last),
    length(Player2RowNumbers,SizeP2),
    nth1(SizeP2,Player2RowNumbers,_,RestPlayer2Rows),

    last(Player1Rows,Row1),
    last(Player2Rows,Row2),
    nth1(SizeP1,Player1Rows,_,Rest1),
    nth1(SizeP2,Player2Rows,_,Rest2),
    
    format('~d ', [Player1Last]),  % Correct Player 1 row number
    print_row(Row1),  % Print Player 1 row
    write('    '),  % Space between grids
    print_row(Row2),  % Print Player 2 row
    format(' ~d', [Player2Last]),  % Correct Player 2 row number (mapped)
    nl,
    print_side_by_side_rows(Rest1, Rest2, RestPlayer1Rows, RestPlayer2Rows).

% Prints a single row
print_row([]).
print_row([Cell]) :-
    format('~w', [Cell]). 
print_row([Cell|Rest]) :-
    format('~w ', [Cell]),  
    print_row(Rest).

display_quit_message(Player) :-
    write('-------------------------------------------'), nl,
    write('      [INFO] Player Abandonment Detected!  '), nl,
    write('-------------------------------------------'), nl,
    format('~w has quit the game. Thanks for playing!', [Player]), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move Execution and Validation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility to find the index of an element in a list.
index_of([Element|_], Element, 1). 
index_of([_|Tail], Element, Index) :- 
    index_of(Tail, Element, Index1),  
    Index is Index1 + 1.              

%function that receives a mapping that translates coordinates from grid1 to grid2 and generates a mapping to to the oposite
reverseMapping(Mapping, ReverseMapping) :-
    length(Mapping, Size),
    numlist(1, Size, InitialReverseMapping),
    reverseMappingAux(Mapping, 1, InitialReverseMapping, ReverseMapping).

reverseMappingAux([], _, ReverseMapping, ReverseMapping).
reverseMappingAux([Elem|Tail], Index, CurrentReverseMapping, ReverseMapping) :-
    nth1(Elem, CurrentReverseMapping, _, TempReverseMapping), % Remove the element at position Elem
    nth1(Elem, UpdatedReverseMapping, Index, TempReverseMapping), % Insert Index at position Elem
    NextIndex is Index + 1,
    reverseMappingAux(Tail, NextIndex, UpdatedReverseMapping, ReverseMapping).

% checks if a position is within bounds and does not already have a piece
validate_move(Grid1, move(Row, Col)) :-
    length(Grid1, Max),
    Row >= 1, Row =< Max,
    Col >= 1, Col =< Max,
    nth1(Row, Grid1, TargetRow),
    nth1(Col, TargetRow, Symbol),
    Symbol = '_ '.

% Executes a move if valid and updates the game state.
move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping,AI1Level,AI2Level), move(Row, ColLetter), game_state(NewGrid1, NewGrid2, NextPlayer, Player1, Player2, RowMapping, ColMapping,AI1Level,AI2Level)) :-
    atom(ColLetter),
    letter_to_index(ColLetter, Col),
    handle_player_move(CurrentPlayer, Player1, Row, Col, Grid1, Grid2, RowMapping, ColMapping, NewGrid1, NewGrid2, NextPlayer, Player2, TranslatedRow, TranslatedCol).

% Handles Player 1 move
handle_player_move(CurrentPlayer, Player1, Row, Col, Grid1, Grid2, RowMapping, ColMapping, NewGrid1, NewGrid2, NextPlayer, Player2, TranslatedRow, TranslatedCol) :-
    CurrentPlayer = Player1,
    validate_move(Grid1, move(Row, Col)),
    NextPlayer = Player2,
    place_symbol_player1('X ', Grid1, Grid2, Row, Col, RowMapping, ColMapping, NewGrid1, NewGrid2).

% Handles Player 2 move
handle_player_move(CurrentPlayer, Player1, Row, Col, Grid1, Grid2, RowMapping, ColMapping, NewGrid1, NewGrid2, NextPlayer, Player2, TranslatedRow, TranslatedCol) :-
    CurrentPlayer \= Player1,
    handle_player2_coordinates(Player2, Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol),
    validate_move(Grid2, move(TranslatedRow, TranslatedCol)),
    NextPlayer = Player1,
    reverseMapping(RowMapping, ReverseRowMapping),
    reverseMapping(ColMapping, ReverseColMapping),
    place_symbol_player2('O ', Grid2, Grid1, TranslatedRow, TranslatedCol, ReverseRowMapping, ReverseColMapping, NewGrid2, NewGrid1).

% Handles translation of coordinates for Player 2 depending on whether they are a CPU or human
handle_player2_coordinates(Player2, Row, Col, _RowMapping, _ColMapping, TranslatedRow, TranslatedCol) :-
    Player2 = 'CPU',
    TranslatedRow = Row,
    TranslatedCol = Col.

handle_player2_coordinates(Player2, Row, Col, _RowMapping, _ColMapping, TranslatedRow, TranslatedCol) :-
    Player2 = 'CPU2',
    TranslatedRow = Row,
    TranslatedCol = Col.

handle_player2_coordinates(_, Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol) :-
    translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol).

% Places a symbol on both grids according to the mappings.
place_symbol_player1(Symbol, Grid1, Grid2, Row, Col, RowMapping, ColMapping, NewGrid1, NewGrid2) :-
    update_grid(Grid1, Row, Col, Symbol, NewGrid1),
    translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol),
    update_grid(Grid2, TranslatedRow, TranslatedCol, Symbol, NewGrid2).

place_symbol_player2(Symbol, Grid1, Grid2, Row, Col, RowMappingForGrid2, ColMappingForGrid2, NewGrid1, NewGrid2) :-
    
    update_grid(Grid1, Row, Col, Symbol, NewGrid1),
    translate_coordinates(Row, Col, RowMappingForGrid2, ColMappingForGrid2, TranslatedRow, TranslatedCol),
    update_grid(Grid2, TranslatedRow, TranslatedCol, Symbol, NewGrid2).

% Updates a specific cell in a grid.
update_grid(Grid, Row, Col, Symbol, NewGrid) :-
    nth1(Row, Grid, OldRow),
    nth1(Col, OldRow, _, TempRow),
    nth1(Col, NewRow, Symbol, TempRow),
    nth1(Row, Grid, _, TempGrid),
    nth1(Row, NewGrid, NewRow, TempGrid).

% Translates coordinates for grid1 or grid2 depending on the provided mapping.
translate_coordinates(Row, Col, RowMapping, ColMapping, NewRow, NewCol) :-
    index_of(RowMapping, Row, NewRowIndex), 
    index_of(ColMapping, Col, NewColIndex), 
    NewRow is NewRowIndex,                  
    NewCol is NewColIndex.                  

% Map lettered columns to numeric columns
letter_to_index(a, 1).
letter_to_index(b, 2).
letter_to_index(c, 3).
letter_to_index(d, 4).
letter_to_index(e, 5).
letter_to_index(f, 6).
letter_to_index(g, 7).
letter_to_index(h, 8).
letter_to_index(i, 9).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Counts how many lines of 4 or squares a player has in their own grid
calculate_points(Grid, Player, Player1, Points) :-
    get_symbol(Player, Player1, Symbol),
    findall(_, horizontal_lines(Grid, Symbol), Horizontal),
    findall(_, vertical_lines(Grid, Symbol), Vertical),
    findall(_, diagonal_lines(Grid, Symbol), Diagonals),
    findall(_, squares_of_four(Grid, Symbol), Squares),
    length(Horizontal, HorizontalCount),
    length(Vertical, VerticalCount),
    length(Diagonals, DiagonalCount),
    length(Squares, SquareCount),
    Points is HorizontalCount + VerticalCount + DiagonalCount + SquareCount.

% Assigns the correct symbol based on the player
get_symbol(Player, Player1, 'X ') :- Player = Player1.
get_symbol(Player, Player1, 'O ') :- Player \= Player1.

% Finds all horizontal lines of length 4 of a certain symbol
horizontal_lines(Grid, Symbol) :-
    member(Row, Grid),
    append(_, [Symbol, Symbol, Symbol, Symbol | _], Row).

% Finds all vertical lines of length 4 of a certain symbol
vertical_lines(Grid, Symbol) :-
    transpose(Grid, TransposedGrid),
    horizontal_lines(TransposedGrid, Symbol).

% Finds all diagonal lines (\ and /) of length 4 of a certain symbol
diagonal_lines(Grid, Symbol) :-
    diagonals(Grid, Diagonals),
    member(Diagonal, Diagonals),
    contains_sequence(Diagonal, [Symbol, Symbol, Symbol, Symbol]).

% Checks if a sequence of 4 symbols appears in a list (diagonal)
contains_sequence(Diagonal, Sequence) :-
    append(_, Sequence, Diagonal).

% Extracts all diagonals (\ and /) from a grid
diagonals(Grid, Diagonals) :-
    length(Grid, Size),
    findall(Diagonal, extract_diagonal(Grid, Size, Diagonal), Diagonals1),
    findall(Diagonal, extract_reverse_diagonal(Grid, Size, Diagonal), Diagonals2),
    append(Diagonals1, Diagonals2, Diagonals).
grimace([
    ['a', 'b', 'c', 'd'],
    ['e', 'f', 'g', 'h'],
    ['i', 'j', 'k', 'l'],
    ['w', 'x', 't', 'z']
]).

% Extracts a single diagonal from the grid (\ direction) of length 4
extract_diagonal(Grid, Size, Diagonal) :-
    between(1, Size, Row),    
    between(1, Size, Col),  
    findall(Cell, (
        between(0, 3, Offset),
        RowOffset is Row + Offset,
        ColOffset is Col + Offset,
        within_bounds(RowOffset, Size),
        within_bounds(ColOffset, Size),
        nth1(RowOffset, Grid, GridRow),
        nth1(ColOffset, GridRow, Cell)
    ), Diagonal),
    length(Diagonal, 4).

% Extracts a single diagonal from the grid ('/' direction) of length 4
extract_reverse_diagonal(Grid, Size, Diagonal) :-
    between(1, Size, Row),  
    between(1, Size, Col), 
    findall(Cell, (
        between(0, 3, Offset), 
        RowOffset is Row + Offset,
        ColOffset is Col - Offset,
        within_bounds(RowOffset, Size),
        within_bounds(ColOffset, Size),
        nth1(RowOffset, Grid, GridRow),
        nth1(ColOffset, GridRow, Cell)
    ), Diagonal),
    length(Diagonal, 4).

% Checks if a value is within bounds
within_bounds(Value, Max) :-
    Value >= 1, Value =< Max.

% Finds all 2x2 squares of the same symbol
squares_of_four(Grid, Symbol) :-
    length(Grid, Size),
    Max is Size - 1,
    between(1, Max, Row),
    between(1, Max, Col),
    nth1(Row, Grid, Row1),
    nth1(Col, Row1, Symbol),
    NextRow is Row + 1,
    nth1(NextRow, Grid, Row2),
    nth1(Col, Row2, Symbol),
    NextCol is Col + 1,
    nth1(NextCol, Row1, Symbol),
    nth1(NextCol, Row2, Symbol).

 
% Returns all valid moves for the current game state.
valid_moves(Grid1, ListOfMoves) :-
    length(Grid1, Size),
    findall(move(Row, Letter), 
        (   between(1, Size, Row), 
            between(1, Size, Col),
            validate_move(Grid1, move(Row, Col)),
            col_to_atom(Col, Letter)
        ), 
        ListOfMoves).

% translates a column number to its respective letter 
col_to_atom(Col, Letter) :-
    Letters = [a, b, c, d, e, f, g, h, i],
    nth1(Col, Letters, Letter).

% Checks if the game is over and determines the winner
game_over(game_state(Grid1, Grid2, _, _, _, _, _, _, _), Winner) :-
    valid_moves(Grid2,Moves2),
    length(Moves2,Lmoves2),
    % No valid moves left for player 2 (he always does last move)
    Lmoves2 = 0,
    calculate_points(Grid1, player1,player1, Points1),
    calculate_points(Grid2, player2,player1, Points2),
    format('Player 1 (X) Points: ~w~n', [Points1]),
    format('Player 2 (O) Points: ~w~n', [Points2]),
    check_winner(Points1, Points2, Winner).


% Determines the winner based on points
check_winner(Points1, Points2, draw) :-
    Points1 = Points2.

check_winner(Points1, Points2, player1) :-
    Points1 < Points2.

check_winner(Points1, Points2, player2) :-
    Points1 > Points2.

% Evaluates the current game state and returns how bad or good it is for the current player
value(game_state(Grid1, _, _, _, _, _), Player, Value) :-
    evaluate_board(Grid1, Player, Value).

random_move(Grid,Move) :-
    valid_moves(Grid, ListOfMoves),
    random_member(Move, ListOfMoves).

% when there are no more moves returns the best one it found
get_best_move(_,_,_,_,_,[],Move,Move) :-!.
get_best_move(Grid,Points,Difference,Player,Player1,[Move|ListOfMoves],CurrentBest,BestMove) :-
    Move = move(Row,ColLetter),
    atom(ColLetter),
    letter_to_index(ColLetter,Col),
    get_symbol(Player, Player1, Symbol),
    update_grid(Grid, Row, Col, Symbol, NewGrid), % simulates the move
    calculate_points(NewGrid,Player,Player1,UpdatedPoints), % calculates the amount of points the player gets when that move is played
    NewDifference is UpdatedPoints-Points, % calculates amout of points gained
    compare_best_move(NewDifference, Difference, Grid, Points, Player, Player1, ListOfMoves, Move, BestMove, CurrentBest).
    
% checks if Move makes less points than CurrentBest if yes it updates CurrentBest otherwise keeps searching until list of moves is empty
compare_best_move(NewDifference, Difference, Grid, Points, Player, Player1, ListOfMoves, Move, BestMove, CurrentBest) :-
    NewDifference =< Difference,
    get_best_move(Grid, Points, NewDifference, Player, Player1, ListOfMoves, Move, BestMove).
compare_best_move(NewDifference, Difference, Grid, Points, Player, Player1, ListOfMoves, Move, BestMove, CurrentBest) :-
    NewDifference > Difference,
    get_best_move(Grid, Points, Difference, Player, Player1, ListOfMoves, CurrentBest, BestMove).

% uses the get_best_move predicate to find the move that makes less points in each turn
greedy_move(Grid,Player,Player1,Move) :-
    valid_moves(Grid,ListOfMoves),
    calculate_points(Grid,Player,Player1,Points),
    get_best_move(Grid,Points,999,Player,Player1,ListOfMoves,move(-1,-1),Move).
        
% Chooses a move for the computer player based on difficulty level (level 1 should return a random valid move and level 2 the best play with a greedy algorithm)
choose_move(Grid, Level, Player, Player1, Move) :-
    Level = 1,
    random_move(Grid, Move).

choose_move(Grid, Level, Player, Player1, Move) :-
    Level = 2,
    greedy_move(Grid,Player,Player1, Move).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Runs the game loop, alternating turns between players
game_loop(GameState) :-
    display_game(GameState),
    check_game_status(GameState).

% Check if the game is over or continue with the next player turn
check_game_status(GameState) :-
    game_over(GameState, Winner),
    !,
    announce_winner(Winner).

check_game_status(GameState) :-
    current_player_turn(GameState, NewGameState),
    handle_next_state(NewGameState).

% Handle the next game state (either quit or continue the loop)
handle_next_state(quit) :- !.
handle_next_state(NewGameState) :- game_loop(NewGameState).

announce_winner(Winner) :-
    Winner = draw,
    write('The game ended in a draw!'), nl.

announce_winner(Winner) :-
        format('Congratulations, ~w! You Won!', [Winner]), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Player Turn Logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handles current player turn
current_player_turn(GameState, NewGameState) :-
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level),
    handle_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState).

% Handle player turn if the current player is not a CPU
handle_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState) :-
    CurrentPlayer \= 'CPU',
    CurrentPlayer \= 'CPU1',
    CurrentPlayer \= 'CPU2',
    handle_player_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState).

% Handle computer turn otherwise for 'CPU'
handle_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState) :-
    CurrentPlayer = 'CPU',
    handle_computer_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState).

% Handle computer turn otherwise for 'CPU1'
handle_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState) :-
    CurrentPlayer = 'CPU1',
    handle_computer_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState).

% Handle computer turn otherwise for 'CPU2'
handle_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState) :-
    CurrentPlayer = 'CPU2',
    handle_computer_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState).

% Handles a human player turn
handle_player_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState) :-
    format('~w, it\'s your turn! Enter your move (Row,Col) or type "quit" to exit: ', [CurrentPlayer]),
    read(Input),
    check_input(Input, Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState).

check_input(quit, Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, quit) :-
    display_quit_message(CurrentPlayer).

check_input(move(Row, Col), Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState) :-
    move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level), move(Row, Col), NewGameState),
    !.

check_input(_, Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState) :-
    write('Invalid move! Try again.'), nl,
    handle_player_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState).
% Handles a computer player turn
handle_computer_turn(Grid1, Grid2,CurrentPlayer,Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState) :-
    CurrentPlayer = Player1,
     choose_move(Grid1, AI1Level, CurrentPlayer,Player1, Move),
     % Replace 1 with AI level if needed
    move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level), Move, NewGameState),
    format('Computer chose move: ~w~n', [Move]).

handle_computer_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level, NewGameState) :-
    CurrentPlayer = Player2,
     choose_move(Grid2, AI2Level, CurrentPlayer, Player1, Move),
    move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level), Move, NewGameState),
    % operations required to display the move with the grid2 coordinates
    Move = move(Row,ColLetter),
    letter_to_index(ColLetter,Col),
    reverseMapping(RowMapping,RRowMapping),
    reverseMapping(ColMapping,RColMapping),
    translate_coordinates(Row,Col,RRowMapping,RColMapping,TranslatedRow,TranslatedCol),
    col_to_atom(TranslatedCol,TranslatedColLetter),
    
    format('Computer chose move: move(~d,~w)~n', [TranslatedRow,TranslatedColLetter]).
