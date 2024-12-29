% -*- Prolog -*-

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(random_between)).
:- use_module(library(between)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Menu 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starts the game by displaying the main menu
play :-
    main_menu.

% Displays the game menu and handles user choices
main_menu :-
    write('Welcome to Doblin!'), nl,
    get_grid_size(Size),
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Quit'), nl,
    write('Choose an option: '), nl,
    catch(read(Choice), _, fail),
    (   integer(Choice), member(Choice, [1, 2, 3, 4])
    ->  (   Choice = 4
        ->  write('Goodbye!'), nl;   
            configure_game(Choice, Size, GameConfig),
            initial_state(GameConfig, InitialState),
            game_loop(InitialState)
        );   
        write('Invalid option! Please try again.'), nl,
        main_menu
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Asks for and validates grid size
get_grid_size(Size) :-
    write('Choose grid size (between 6 and 9): '),
    catch(read(Input), _, fail),
    (   integer(Input), Input >= 6, Input =< 9
    ->  Size = Input
    ;   write('Invalid grid size! Please choose a size between 6 and 9.'), nl,
        get_grid_size(Size)
    ).

% Ensures name does not exceed 16 characters
valid_name(Name) :-
    atom_length(Name, Length),
    Length =< 16. 

% Configures the game based on selected mode
configure_game(1, Size, config(Name1, Name2, _, _, Size)) :- 
    write('Human vs Human selected.'), nl,
    get_player_name('Player 1', Name1),
    get_player_name('Player 2', Name2).

configure_game(2, Size, config(Name1, 'CPU', Level, _, Size)) :- 
    write('Human vs Computer selected.'), nl,
    get_player_name('Your', Name1),
    get_ai_level(Level).

configure_game(3, Size, config('CPU1', 'CPU2', Level1, Level2, Size)) :-
    write('Computer vs Computer selected.'), nl,
    get_ai_level(Level1, 'CPU1'),
    get_ai_level(Level2, 'CPU2').

% Prompts for player name
get_player_name(PlayerLabel, Name) :-
    format('Enter name for ~w: ', [PlayerLabel]),
    catch(read(Input), _, fail),
    (   atom(Input), valid_name(Input)
    ->  Name = Input
    ;   write('Invalid name! Please enter a name using only letters and ensure it does not exceed 16 characters.'), nl,
        get_player_name(PlayerLabel, Name)
    ).

% Prompts for AI difficulty
get_ai_level(Level, Label) :-
    ( var(Label) -> format('Choose computer difficulty (1: Easy, 2: Hard): ', []);
      format('Choose difficulty for ~w (1: Easy, 2: Hard): ', [Label])
    ),
    catch(read(Input), _, fail),
    (   integer(Input), member(Input, [1, 2])
    ->  Level = Input
    ;   write('Invalid difficulty! Please choose 1 or 2.'), nl,
        get_ai_level(Level, Label)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game State Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initializes the game state based on the configuration
initial_state(config(Name1, Name2, _, _, Size), game_state(Grid1, Grid2, Name1, Name2, RowMapping, ColMapping)) :-
    initialize_grids(Size, Grid1, Grid2, RowMapping, ColMapping).

% Initializes two grids and their coordinate mappings
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

% Generates row and column mappings for the second grid
generate_mappings(Size, RowMapping, ColMapping) :-
    numlist(1, Size, Base),
    random_permutation(Base, RowMapping),
    random_permutation(Base, ColMapping).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Display the current game state
display_game(game_state(Grid1, Grid2, Name1, Name2, CurrentPlayer, _)) :-
    length(Grid1, Size),
    write('Grids:'), nl,
    Width is Size * 3 - 2,
    print_player_names(Name1, Width, 2),
    print_player_names(Name2, Width, Width + 7), nl,  
    generate_column_labels(Size, Player1Labels),
    generate_column_labels(Size, Player2Labels),
    print_column_labels(Player1Labels, Player2Labels), 
    numlist(1, Size, Player1RowNumbers),  
    random_permutation(Player1RowNumbers, Player2RowNumbers),  
    print_side_by_side(Grid1, Grid2, Player1RowNumbers, Player2RowNumbers),
    nl,
    print_current_player(CurrentPlayer, Name1, Name2).


% Helper predicate to print the current player
print_current_player(CurrentPlayer, Name1, Name2) :-
    (   CurrentPlayer = Name1
    ->  format('Current Player: ~w~n', [Name1])  % Player 1 turn
    ;   format('Current Player: ~w~n', [Name2])  % Player 2 turn
    ).

% Helper to print a name centered within a specific width with a dynamic offset
print_player_names(Name, Width, Offset) :-
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

% checks if a position in within bounds and does not already have a piece
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

% Counts the points for a player in their respective grid
calculate_points(Grid, Player, Points) :-
    (Player = player1 -> Symbol = 'X'; Symbol = 'O'),
    findall(_, horizontal_lines(Grid, Symbol), Horizontal),
    findall(_, vertical_lines(Grid, Symbol), Vertical),
    findall(_, diagonal_lines(Grid, Symbol), Diagonals),
    findall(_, squares_of_four(Grid, Symbol), Squares),
    length(Horizontal, HorizontalCount),
    length(Vertical, VerticalCount),
    length(Diagonals, DiagonalCount),
    length(Squares, SquareCount),
    Points is HorizontalCount + VerticalCount + DiagonalCount + SquareCount.

% Finds all horizontal lines of length 4
horizontal_lines(Grid, Symbol) :-
    member(Row, Grid),
    sublist([Symbol, Symbol, Symbol, Symbol], Row).

% Finds all vertical lines of length 4
vertical_lines(Grid, Symbol) :-
    transpose(Grid, TransposedGrid),
    horizontal_lines(TransposedGrid, Symbol, Length).

% Finds all diagonal lines (\ and /) of length 4
diagonal_lines(Grid, Symbol) :-
    diagonals(Grid, Diagonals),
    member(Diagonal, Diagonals),
    sublist([Symbol, Symbol, Symbol, Symbol], Diagonal).

% Extracts all diagonals (\ and /) from a grid
diagonals(Grid, Diagonals) :-
    length(Grid, Size),
    findall(Diagonal, extract_diagonal(Grid, Size, Diagonal), Diagonals).

% Extracts a single diagonal from the grid
extract_diagonal(Grid, Size, Diagonal) :-
    MaxOffset is Size - 1,
    MinOffset is -(MaxOffset),
    between(MinOffset, MaxOffset, Offset), 
    findall(Cell, (
        between(1, Size, Row),
        Col is Row + Offset,
        within_bounds(Col, Size),
        nth1(Row, Grid, GridRow),
        nth1(Col, GridRow, Cell)
    ), Diagonal).

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

% Sublist helper to match consecutive symbols
sublist(Sub, List) :-
    append(_, Rest, List),
    append(Sub, _, Rest).

% Count consecutive symbols in a row or column
count_consecutive([], _, 0).
count_consecutive([Player|Rest], Player, Count) :-
    count_consecutive(Rest, Player, RestCount),
    Count is RestCount + 1.
count_consecutive([Other|_], Player, 0) :-
    Other \= Player.

% Game over condition: Check if the game is a draw
draw_condition(Grid1, Grid2) :-
    calculate_points(Grid1, player1, Points1),
    calculate_points(Grid2, player2, Points2),
    Points1 = Points2.

winning_condition(Grid1,Grid2) :-
    calculate_points(Grid1, player1, Points1),
    calculate_points(Grid2, player2, Points2),
    Points1 < Points2.

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
    (   winning_condition(Grid1, Grid2) -> 
        calculate_points(Grid1, player1, Points1),
        calculate_points(Grid2, player2, Points2),
        (Points1 > Points2 -> Winner = player1; Winner = player2);
        draw_condition(Grid1, Grid2) -> Winner = draw
    ).

% Evaluates the current game state and returns how bad or good it is for the current player
value(game_state(Grid1, _, _, _, _, _), Player, Value) :-
    evaluate_board(Grid1, Player, Value).


random_move(Grid,Move) :-
        valid_moves(Grid,ListOfMoves),
        random_member(Move,ListOfMoves).
% Chooses a move for the computer player based on difficulty level (level 1 should return a random valid move and level 2 the best play with a greedy algorithm)
choose_move(Grid, Level, Move) :-
    (Level = 1 -> random_move(Grid, Move);
     Level = 2 -> greedy_move(Grid, Move)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Runs the game loop, alternating turns between players
game_loop(GameState) :-
    display_game(GameState),
    (game_over(GameState, Winner) ->
        format('Game Over! Winner: ~w~n', [Winner]);
        current_player_turn(GameState, NewGameState),
        (NewGameState = quit ->
            write('Game exited by the player. Goodbye!'), nl;
        game_loop(NewGameState))
    ).

% Handles the current player turn and alternates turns
current_player_turn(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2), game_state(NewGrid1, NewGrid2, NextPlayer, Player1, Player2)) :-
    % Check whose turn it is (current player is CurrentPlayer)
    (CurrentPlayer = Player1 -> 
        (choose_move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2), 2, Move),
         move(game_state(Grid1, Grid2, Player1, Player2), Move, game_state(NewGrid1, NewGrid2, Player2, Player1, Player2))
        );
        (choose_move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2), 1, Move),
         move(game_state(Grid1, Grid2, Player1, Player2), Move, game_state(NewGrid1, NewGrid2, Player1, Player2))
        )
    ),
    % Switch to the next player
    (CurrentPlayer = Player1 -> NextPlayer = Player2; NextPlayer = Player1).

current_player_turn(GameState, quit) :-
    write('Enter your move (or type "quit" to exit): '), nl,
    catch(read(Move), _, fail),
    (Move = quit -> true; fail).

% Placeholder predicates for required logic (to be implemented):
% evaluate_board/3
% random_move/2, greedy_move/2
