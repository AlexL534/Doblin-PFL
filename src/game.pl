% -*- Prolog -*-

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(random_between)).
:- use_module(library(between)).

% Gives access to the game menu
play :-
    main_menu.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Menu and Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Displays the game menu and allows the players to configure game type, board size, and AI difficulty levels
main_menu :-
    write('Welcome to Doblin!'), nl,
    write('Choose grid size (between 6 and 9): '),
    read(Size),
    (   Size >= 6, Size =< 9
    ->  format('Grid size: ~w~n', [Size]),
        write('1. Human vs Human'), nl,
        write('2. Human vs Computer'), nl,
        write('3. Computer vs Computer'), nl,
        write('Choose an option: '), nl,
        read(Choice),
        configure_game(Choice, Size, GameConfig),
        initial_state(GameConfig, GameState),
        game_loop(GameState)
    ;   write('Invalid grid size! Please choose a size between 6 and 9.'), nl,
        main_menu  % Retry if size is invalid
    ).

% Helper predicate to ensure the name length does not exceed 16 characters.
valid_name(Name) :-
    atom_length(Name, Length),
    Length =< 16. 

% Configures the game based on the menu selection.
configure_game(1, Size, config(Name1, Name2, _, _, Size)) :- 
    write('Human vs Human selected.'), nl,
    write('Enter name for Player 1: '), 
    read(Name1),
    (   valid_name(Name1)
    ->  true  % If name is valid, proceed
    ;   write('Name cannot be longer than 16 characters. Please try again.'), nl,
        configure_game(1, Size, config(Name1, Name2, _, _, Size))  % Re-enter the name
    ),
    write('Enter name for Player 2: '), 
    read(Name2),
    (   valid_name(Name2)
    ->  true  % If name is valid, proceed
    ;   write('Name cannot be longer than 16 characters. Please try again.'), nl,
        configure_game(1, Size, config(Name1, Name2, _, _, Size))  % Re-enter the name
    ).

configure_game(2, Size, config(Name1, 'CPU', Level, _, Size)) :- 
    write('Human vs Computer selected.'), nl,
    write('Enter your name: '),
    read(Name1),
    (   valid_name(Name1)
    ->  true  % If name is valid, proceed
    ;   write('Name cannot be longer than 16 characters. Please try again.'), nl,
        configure_game(2, Size, config(Name1, 'CPU', Level, _, Size))  % Re-enter the name
    ),
    write('Choose computer difficulty (1: Easy, 2: Hard): '),
    read(Level).

configure_game(3, Size, config('CPU1', 'CPU2', Level1, Level2, Size)) :-
    write('Computer vs Computer selected.'), nl,
    write('Choose difficulty for CPU1 (1: Easy, 2: Hard): '), read(Level1),
    write('Choose difficulty for CPU2 (1: Easy, 2: Hard): '), read(Level2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Initialization and State
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initializes the game state based on the configuration
initial_state(config(Name1, Name2, _, _, Size), game_state(Grid1, Grid2, Name1, Name2, RowMapping, ColMapping)) :-
    initialize_grids(Size, Grid1, Grid2, RowMapping, ColMapping).

% Initializes two grids and their coordinate mappings.
initialize_grids(Size, Grid1, Grid2, RowMapping, ColMapping) :-
    create_empty_grid(Size, Grid1),
    create_empty_grid(Size, Grid2),
    generate_mappings(Size, RowMapping, ColMapping).

% Creates an empty grid of the specified size, filled with '_'
create_empty_grid(Size, Grid) :-
    length(Grid, Size),
    maplist(length_(Size), Grid),
    maplist(maplist(=( '_ ')), Grid).  % Initialize each cell with '_'

% Helper predicate to define list length for rows
length_(Size, List) :-
    length(List, Size).

% Generates row and column mappings for the second grid.
generate_mappings(Size, RowMapping, ColMapping) :-
    numlist(1, Size, Base),
    random_permutation(Base, RowMapping),
    random_permutation(Base, ColMapping).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Display the current game state
display_game(game_state(Grid1, Grid2, Name1, Name2, CurrentPlayer, _)) :-
    length(Grid1, Size),  % Get the grid size (assumes square grid)
    write('Grids:'), nl,
    Width is Size * 3 - 2,
    print_centered_with_offset(Name1, Width, 1),
    print_centered_with_offset(Name2, Width, Width + 7), nl,  
    generate_column_labels(Size, Player1Labels),  % Generate column labels for Player 1
    generate_column_labels(Size, Player2Labels),  % Generate column labels for Player 2
    print_column_labels(Player1Labels, Player2Labels),  % Print column labels for both grids
    numlist(1, Size, Player1RowNumbers),  % Sequential row numbers for Player 1
    random_permutation(Player1RowNumbers, Player2RowNumbers),  % Randomized row numbers for Player 2
    print_side_by_side(Grid1, Grid2, Player1RowNumbers, Player2RowNumbers), % Print both grids side by side
    nl, % Blank line between grids
    (   CurrentPlayer = Name1
    ->  format('Current Player: ~w~n', [Name1])  % Player 1's turn
    ;   format('Current Player: ~w~n', [Name2])  % Player 2's turn
    ).

% Helper to print a name centered within a specific width with a dynamic offset
print_centered_with_offset(Name, Width, Offset) :-
    atom(Name),  % Ensure Name is an atom
    atom_length(Name, NameLength),  % Get the length of the name
    Padding is Width - NameLength,  % Total padding needed with the dynamic offset
    LeftPadding is Padding // 2,    % Half padding for the left
    RightPadding is Padding - LeftPadding,  % Remaining padding for the right
    format('~*|~s~*|', [LeftPadding + Offset, Name, RightPadding]).

% Print column labels for both grids
print_column_labels(Player1Labels, Player2Labels) :-
    write('  '), % Space before column labels for Player 1
    print_aligned_labels(Player1Labels),  % Write Player 1 labels with spacing
    write('   '), % Space between the grids
    print_aligned_labels(Player2Labels),  % Write Player 2 labels with spacing
    nl.

% Print aligned column labels with two spaces between each label
print_aligned_labels([]).
print_aligned_labels([Label|Rest]) :-
    format('~w  ', [Label]),  % Two spaces after each label
    print_aligned_labels(Rest).

% Generate column labels (e.g., ['A', 'B', 'C', ...] up to grid size)
generate_column_labels(Size, Labels) :-
    numlist(1, Size, Numbers),
    maplist(generate_column_label, Numbers, Labels).

% Generate column label for a given index (supports up to 'ZZ')
generate_column_label(Index, Label) :-
    Base is 26,
    Quotient is (Index - 1) // Base,
    Remainder is (Index - 1) mod Base,
    char_code('A', A),
    (   Quotient =:= 0
    ->  Code is A + Remainder,
        char_code(Label, Code)
    ;   FirstCode is A + Quotient - 1,
        SecondCode is A + Remainder,
        char_code(First, FirstCode),
        char_code(Second, SecondCode),
        atom_codes(Label, [First, Second])
    ).

% Print both grids side by side with randomized row numbers for Player 2
print_side_by_side([], [], [], []).
print_side_by_side([Row1|Rest1], [Row2|Rest2], [Player1RowNum|RestPlayer1Rows], [Player2RowNum|RestPlayer2Rows]) :-
    format('~d ', [Player1RowNum]),  % Print Player 1 row number
    print_row(Row1),  % Print Player 1 row
    write('    '),  % Space between grids
    print_row(Row2),  % Print Player 2 row
    format('~d', [Player2RowNum]),  % Print Player 2 row number
    nl,
    print_side_by_side(Rest1, Rest2, RestPlayer1Rows, RestPlayer2Rows).

% Prints a single row
print_row([]).
print_row([Cell]) :-
    format('~w', [Cell]). 
print_row([Cell|Rest]) :-
    format('~w ', [Cell]),  
    print_row(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move Execution and Validation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%checks if a position in within bounds and does not already have a piece
validate_move(Grid1, move(Row, Col)) :-
    length(Grid1,Max),
    Row >=1, Row =< Max,
    Col >=1, Col =< Max,
    nth1(Row,Grid1,TargetRow),
    nth1(Col,TargetRow,Symbol),
    Symbol == '_'.
    
% Executes a move if valid and updates the game state.
move(game_state(Grid1, Grid2, Player1, Player2, RowMapping, ColMapping), move(Row, Col), game_state(NewGrid1, NewGrid2, Player2, Player1, RowMapping, ColMapping)) :-
    validate_move(Grid1, move(Row, Col)),
    place_symbol(Grid1, Grid2, Row, Col, Player1, RowMapping, ColMapping, NewGrid1, NewGrid2).

% Places a symbol on both grids according to the mappings.
place_symbol(Grid1, Grid2, Row, Col, Symbol, RowMapping, ColMapping, NewGrid1, NewGrid2) :-
    translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol),
    update_grid(Grid1, Row, Col, Symbol, NewGrid1),
    update_grid(Grid2, TranslatedRow, TranslatedCol, Symbol, NewGrid2).

% Updates a specific cell in a grid.
update_grid(Grid, Row, Col, Symbol, NewGrid) :-
    nth1(Row, Grid, OldRow),
    nth1(Col, OldRow, _, TempRow),
    nth1(Col, NewRow, Symbol, TempRow),
    nth1(Row, Grid, _, TempGrid),
    nth1(Row, NewGrid, NewRow, TempGrid).

% Translates coordinates for the second grid.
translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol) :-
    nth1(Row, RowMapping, TranslatedRow),
    nth1(Col, ColMapping, TranslatedCol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Merge the two grids into a larger one
combine_grids(Grid1, Grid2, CombinedGrid) :-
    length(Grid1, Rows),
    transpose(Grid1, TransposedGrid1),
    transpose(Grid2, TransposedGrid2),
    append(TransposedGrid1, TransposedGrid2, CombinedGrid).

% Counts the points for a player (number of lines or squares of 4 consecutive symbols)
calculate_points(Grid1, Grid2, Player, Points) :-
    combine_grids(Grid1, Grid2, CombinedGrid),
    
    % Count horizontal lines
    findall(_, (nth1(Row, CombinedGrid, R), count_consecutive(R, Player, 4)), Horizontal),
    
    % Count vertical lines
    transpose(CombinedGrid, TransposedGrid),
    findall(_, (nth1(Col, TransposedGrid, C), count_consecutive(C, Player, 4)), Vertical),
    
    % Count diagonal lines (\ and /)
    findall(_, (diagonal_lines(CombinedGrid, Player), count_consecutive(_, Player, 4)), Diagonals),
    
    length(Horizontal, HorizontalCount),
    length(Vertical, VerticalCount),
    length(Diagonals, DiagonalCount),
    Points is HorizontalCount + VerticalCount + DiagonalCount.

% Count consecutive symbols in a row or column
count_consecutive([], _, 0).
count_consecutive([Player|Rest], Player, Count) :-
    count_consecutive(Rest, Player, RestCount),
    Count is RestCount + 1.
count_consecutive([Other|_], Player, 0) :-
    Other \= Player.

% Find diagonal lines considering both grids
diagonal_lines(CombinedGrid, Player) :-
    % You can implement this by checking both \ and / diagonals
    % across the combined grid. Ensure you check both grids.
    true.

% Game over condition: Check if the game is a draw
draw_condition(Grid1, Grid2) :-
    calculate_points(Grid1, Grid2, player1, Points1),
    calculate_points(Grid1, Grid2, player2, Points2),
    Points1 = Points2.

all_moves(Grid,Moves) :-
    length(Grid,Max),
    findall(move(Row,Col), (between(1,Max,Row),between(1,Max,Col)),Moves).
% Returns all valid moves for the current game state.
valid_moves(Grid1, ListOfMoves) :-
    all_moves(Grid1,Moves),
    findall(move(Row, Col), (member(move(Row,Col),Moves),validate_move(Grid1, move(Row, Col))), ListOfMoves).


% Checks if the game is over and determines the winner
game_over(game_state(Grid1, Grid2, _, _, _, _), Winner) :-
    valid_moves(Grid1,Moves1),
    valid_moves(Grid2,Moves2),
    length(Moves1,Lmoves1),
    length(Moves2,Lmoves2),
    Lmoves1 == 0, Lmoves2 == 0,
    (winning_condition(Grid1,Grid2) -> Winner = player1;
     winning_condition(Grid1,Grid2) -> Winner = player2;
     draw_condition(Grid1, Grid2) -> Winner = draw), !, fail.

% Evaluates the current game state and returns how bad or good it is for the current player
value(game_state(Grid1, _, _, _, _, _), Player, Value) :-
    evaluate_board(Grid1, Player, Value).

% Chooses a move for the computer player based on difficulty level (level 1 should return a random valid move and level 2 the best play with a greedy algorithm)
choose_move(GameState, Level, Move) :-
    (Level = 1 -> random_move(GameState, Move);
     Level = 2 -> greedy_move(GameState, Move)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Runs the game loop, alternating turns between players
game_loop(GameState) :-
    display_game(GameState),
    (game_over(GameState, Winner) ->
        format('Game Over! Winner: ~w~n', [Winner]);
        current_player_turn(GameState, NewGameState),
        game_loop(NewGameState)).

% Handles the current player turn
current_player_turn(game_state(Board, CurrentPlayer, Captured), NewGameState) :-
    (CurrentPlayer = human ->
        read_move(Move);
        choose_move(game_state(Board, CurrentPlayer, Captured), 2, Move)),
    move(game_state(Board, CurrentPlayer, Captured), Move, NewGameState).

% Placeholder predicates for required logic (to be implemented):
% validate_move/2, winning_condition/1, evaluate_board/3
% random_move/2, greedy_move/2
